// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFIConvert.scala
*       Author          :       yexc
*       Revision        :       2020/01/13
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       
*
*       io.en           :       input, ...
*       io.din          :       input[width-1:0], ...
*       io.dout         :       output[width-1:0], ...
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import vpu.DataGating._
import vpu.HardFloatHelper._
import vpu.Packing._
import vpu.util._

class VFIConvert(params: VPUParams) extends VModule(params) {
    class VFXUFn(sz_op: Int) extends Bundle {
      val rm = UInt(VPUConstants.FRM_SZ.W)
      val op = UInt(sz_op.W)

      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
      def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
      override def cloneType = new VFXUFn(sz_op).asInstanceOf[this.type]
    }

   class VFPToIntOperand extends Bundle {
      val fn = new VFXUFn(FCvtFun_SZ)
      val vsrc2e = new FUCBVec
      override def cloneType = new VFPToIntOperand().asInstanceOf[this.type]
     }

    class VFPToIntResult extends Bundle {
       val vFPToIntOut = new SEWVec
       //val exc       = UInt(VPUConstants.FFLAGS_SZ.W)
       val exc       = new FFLAGSVec
       override def cloneType = new VFPToIntResult().asInstanceOf[this.type]
     }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val req   = Flipped(Valid(new VFPToIntOperand))
     val resp  = Valid(new VFPToIntResult)
  })

  val fn = io.req.bits.fn.dgate(io.req.valid)

  val f16 = io.req.bits.vsrc2e.f16
  val f32 = io.req.bits.vsrc2e.f32
  val f64 = io.req.bits.vsrc2e.f64
  val f128 = io.req.bits.vsrc2e.f128

  // op(0) -- unsigned(0) or signed(1)
  // op(1) -- unwiden(0) or widen(1)
  // op(2) -- unnarrow(0) or narrow(1)
  val op_float2int = MuxCase(
    0.U, Array(
      fn.op_is(FCvtFun_F2X)  -> "b001".U,
      fn.op_is(FCvtFun_F2XU) -> "b000".U,
      fn.op_is(FCvtFun_F2WX)  -> "b011".U,
      fn.op_is(FCvtFun_F2WXU) -> "b010".U,
      fn.op_is(FCvtFun_F2NX)  -> "b101".U,
      fn.op_is(FCvtFun_F2NXU) -> "b100".U
    ))

  def stagesFConv = 0
  val isNarrowOp = fn.op_is(FCvtFun_F2NX,FCvtFun_F2NXU)
  val notNarrowOp = fn.op_is(FCvtFun_F2X,FCvtFun_F2XU,FCvtFun_F2WX,FCvtFun_F2WXU)

                                // F2X[U]      F2NX[U]     F2WX[U]
                                // SEW=32      SEW=16      SEW=32
  // results_float2int(0)._1  --> (f32->int32, f32->int16, f32->int64)  // f32->int16 require FP16
                                // SEW=16      SEW=8       SEW=16
  // results.float2int(1)._1  --> (f16->int16, f16->int8,  f16->int32)  // f16->int8 require FP8 which is impossible
                                // SEW=64      SEW=32      SEW=64
  // results.float2int(2)._1  --> (f64->int64, f64->int32, f64->int128) // f64->int32 require FP32
                                // SEW=128      SEW=64       SEW=128
  // results.float2int(3)._1  --> (f128->int64, f128->int64, f128->int256)  // f128->int64 require FP64
  val results_float2int = 
    List(     (FPS, f32, recode_sp _, List((32,  e32Depth),  (16, f16Depth), (64,  e64Depth)),  f32Depth,  ieee_sp _, (8, 24))) ++
 (F16).option((FPH, f16, recode_hp _, List((16,  e16Depth),  (8,         0),  (32,  e32Depth)),  f16Depth,  ieee_hp _, (5, 11))) ++
 (F64).option((FPD, f64, recode_dp _, List((64,  e64Depth),  (32, f32Depth), (128, e128Depth)), f64Depth,  ieee_dp _, (11, 53))) ++
(F128).option((FPQ, f128,recode_qp _, List((128, e128Depth), (64, f64Depth), (256, e256Depth)), f128Depth, ieee_qp _, (16, 112))) map {
     case (fp, in, recode, ele_list, fDepth, ieee, (exp, sig)) => {
      val results_int = ele_list.filter(_._2 != 0) map {
       case (wd, eDepth) => {
        val out = Wire(Vec(eDepth, UInt(wd.W)))
        val exc = Wire(Vec(fDepth, UInt(VPUConstants.FFLAGS_SZ.W))) // have fDepth exceptions output
        val loop = fDepth min eDepth

        for(i <- 0 until loop) {
          val valid = ((io.vsew === fp) && notNarrowOp) || ((io.vsew === (fp-1.U)) && isNarrowOp)
          val input = (dgate(valid, in(i)))
          val rm = dgate(valid, fn.rm)
          val op = dgate(valid, op_float2int)
          val fp2int = Module(new hardfloat.RecFNToIN(exp, sig, wd)) 
          val name = "float" + (exp+sig).toString + "_2_int" + (wd).toString + "_inst"
          fp2int.suggestName(name)
          fp2int.io.signedOut := op(0)
          fp2int.io.in := input
          fp2int.io.roundingMode := rm
          out(i) := fp2int.io.out
          exc(i) := fp2int.io.intExceptionFlags
        }

        // cleanup
        for(i <- loop until eDepth)
          out(i) := 0.U 
        for(i <- loop until fDepth)
          exc(i) := 0.U

        (out, exc)
       }
      }

      // results_int(0) is Narrow instruction,
      // results_int(1) is normal instruction or none,
      // results_int(2) is Wideden instruction or none.
      val expect_out = if(results_int.size == 3)        // F2X[U], F2NX[U], F2WX[U]
          MuxCase(results_int(0)._1.asUInt, Array(
            (fn.op_is(FCvtFun_F2NX, FCvtFun_F2NXU)) -> results_int(1)._1.asUInt,
            (fn.op_is(FCvtFun_F2WX, FCvtFun_F2WXU)) -> results_int(2)._1.asUInt
          ))  
        else if((results_int.size == 2) && (ele_list(1)._2 != 0))   // F2X[U], F2NX[U]
          Mux(fn.op_is(FCvtFun_F2NX, FCvtFun_F2NXU), results_int(1)._1.asUInt, results_int(0)._1.asUInt)
        else if((results_int.size == 2) && (ele_list(2)._2 != 0))   // F2X[U], F2WX[U]
          Mux(fn.op_is(FCvtFun_F2WX, FCvtFun_F2WXU), results_int(1)._1.asUInt, results_int(0)._1.asUInt)
        else  // F2X[U]
          results_int(0)._1.asUInt


      val expect_exc = if(results_int.size == 3)        // F2X[U], F2NX[U], F2WX[U]
          MuxCase(results_int(0)._2, Array(
            (fn.op_is(FCvtFun_F2NX, FCvtFun_F2NXU)) -> results_int(1)._2,
            (fn.op_is(FCvtFun_F2WX, FCvtFun_F2WXU)) -> results_int(2)._2
          ))  
        else if((results_int.size == 2) && (ele_list(1)._2 != 0))   // F2X[U], F2NX[U]
          Mux(fn.op_is(FCvtFun_F2NX, FCvtFun_F2NXU), results_int(1)._2, results_int(0)._2)
        else if((results_int.size == 2) && (ele_list(2)._2 != 0))   // F2X[U], F2WX[U]
          Mux(fn.op_is(FCvtFun_F2WX, FCvtFun_F2WXU), results_int(1)._2, results_int(0)._2)
        else  // F2X[U]
          results_int(0)._2

     (expect_out, expect_exc)
   }
  }

  val out_res = Wire(UInt((LMULMAX*ELEN).W))
  val out_exc = Wire(new FFLAGSVec)

  (F32, F16, F64, F128) match {
    case (true, false, false, false) => { // F32
      out_res := results_float2int(0)._1
      out_exc.exc32 := results_float2int(0)._2
    }
    case (true, true, false, false) => { // F32, F16
      out_res := MuxCase(0.U, Array(
        (((io.vsew === FPS) && notNarrowOp) || ((io.vsew === FPH) && isNarrowOp)) ->  results_float2int(0)._1,
        (((io.vsew === FPH) && notNarrowOp) || ((io.vsew === 0.U) && isNarrowOp)) ->  results_float2int(1)._1
      ))
      out_exc.exc32 := results_float2int(0)._2
      out_exc.exc16 := results_float2int(1)._2
    }
    case (true, false, true, false) => { // F32, F64
      out_res := MuxCase(0.U, Array(
        (((io.vsew === FPS) && notNarrowOp) || ((io.vsew === FPH) && isNarrowOp)) ->  results_float2int(0)._1,
        (((io.vsew === FPD) && notNarrowOp) || ((io.vsew === FPS) && isNarrowOp)) ->  results_float2int(1)._1
      ))
      out_exc.exc32 := results_float2int(0)._2
      out_exc.exc64 := results_float2int(1)._2
    }
    case (true, true, true, false) => { // F32, F16, F64
       out_res := MuxCase(0.U, Array(
        (((io.vsew === FPS) && notNarrowOp) || ((io.vsew === FPH) && isNarrowOp)) ->  results_float2int(0)._1,
        (((io.vsew === FPH) && notNarrowOp) || ((io.vsew === 0.U) && isNarrowOp)) ->  results_float2int(1)._1,
        (((io.vsew === FPD) && notNarrowOp) || ((io.vsew === FPS) && isNarrowOp)) ->  results_float2int(2)._1
      ))
      out_exc.exc32 := results_float2int(0)._2
      out_exc.exc16 := results_float2int(1)._2
      out_exc.exc64 := results_float2int(2)._2
    }
    case (true, false, true, true) => { // F32, F64, F128
       out_res := MuxCase(0.U, Array(
        (((io.vsew === FPS) && notNarrowOp) || ((io.vsew === FPH) && isNarrowOp)) ->  results_float2int(0)._1,
        (((io.vsew === FPD) && notNarrowOp) || ((io.vsew === FPS) && isNarrowOp)) ->  results_float2int(1)._1,
        (((io.vsew === FPQ) && notNarrowOp) || ((io.vsew === FPD) && isNarrowOp)) ->  results_float2int(2)._1
      ))
      out_exc.exc32 := results_float2int(0)._2
      out_exc.exc64 := results_float2int(1)._2
      out_exc.exc128 := results_float2int(2)._2
    }
    case (true, true, true, true) => { // F32, F16, F64, F128
       out_res := MuxCase(0.U, Array(
        (((io.vsew === FPS) && notNarrowOp) || ((io.vsew === FPH) && isNarrowOp)) ->  results_float2int(0)._1,
        (((io.vsew === FPH) && notNarrowOp) || ((io.vsew === 0.U) && isNarrowOp)) ->  results_float2int(1)._1,
        (((io.vsew === FPD) && notNarrowOp) || ((io.vsew === FPS) && isNarrowOp)) ->  results_float2int(2)._1,
        (((io.vsew === FPQ) && notNarrowOp) || ((io.vsew === FPD) && isNarrowOp)) ->  results_float2int(3)._1
      ))
      out_exc.exc32 := results_float2int(0)._2
      out_exc.exc16 := results_float2int(1)._2
      out_exc.exc64 := results_float2int(2)._2
      out_exc.exc128 := results_float2int(3)._2
    }
    case _ => {
      // do nothing
    }
  }

  val mux = Wire(new VFPToIntResult)

  for(i <- 0 until e8Depth)    mux.vFPToIntOut.e8(i)    := out_res(8*(i+1)-1,  8*i)
  for(i <- 0 until e16Depth)   mux.vFPToIntOut.e16(i)   := out_res(16*(i+1)-1, 16*i)
  for(i <- 0 until e32Depth)   mux.vFPToIntOut.e32(i)   := out_res(32*(i+1)-1, 32*i)
  for(i <- 0 until e64Depth)   mux.vFPToIntOut.e64(i)   := out_res(64*(i+1)-1, 64*i)
  for(i <- 0 until e128Depth)  mux.vFPToIntOut.e128(i)  := out_res(128*(i+1)-1, 128*i)
  for(i <- 0 until e256Depth)  mux.vFPToIntOut.e256(i)  := out_res(256*(i+1)-1, 256*i)
  for(i <- 0 until e512Depth)  mux.vFPToIntOut.e512(i)  := out_res(512*(i+1)-1, 512*i)
  for(i <- 0 until e1024Depth)  mux.vFPToIntOut.e1024(i)  := out_res(1024*(i+1)-1, 1024*i)

  mux.exc := out_exc

  io.resp <> Pipe(io.req.valid, mux, stagesFConv)
}


