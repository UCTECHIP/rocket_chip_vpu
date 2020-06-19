// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VIFConvert.scala
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


class VIFConvert(params: VPUParams) extends VModule(params) {

    class VFXUFn(sz_op: Int) extends Bundle {
      val rm = UInt(VPUConstants.FRM_SZ.W)
      val op = UInt(sz_op.W)

//      def dgate(valid: Bool) = this.cloneType.fromBits(DataGating.dgate(valid, this.asUInt))
      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
      def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
      override def cloneType = new VFXUFn(sz_op).asInstanceOf[this.type]
    }

   class VIntToFPOperand extends Bundle {
      val fn = new VFXUFn(FCvtFun_SZ)
      val vsrc2e = new FToXSEWVec
      val v0en   = new FSEW1wVec
      override def cloneType = new VIntToFPOperand().asInstanceOf[this.type]
     }

    class VIntToFPResult extends Bundle {
       val vIntToFPOut = new FUCBVec
       val exc       = new FFLAGSVec
       //val exc       = UInt(VPUConstants.FFLAGS_SZ.W)
       override def cloneType = new VIntToFPResult().asInstanceOf[this.type]
     }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val req   = Flipped(Valid(new VIntToFPOperand))
     val resp  = Valid(new VIntToFPResult)
  })

  val fn = io.req.bits.fn

  val e8 = io.req.bits.vsrc2e.e8
  val e16 = io.req.bits.vsrc2e.e16
  val e32 = io.req.bits.vsrc2e.e32
  val e64 = io.req.bits.vsrc2e.e64
  val e128 = io.req.bits.vsrc2e.e128
  val e256 = io.req.bits.vsrc2e.e256

  val f16_en  = io.req.bits.v0en.f16
  val f32_en  = io.req.bits.v0en.f32
  val f64_en  = io.req.bits.v0en.f64
  val f128_en  = io.req.bits.v0en.f128

  // op(0) -- unsigned(0) or signed(1)
  // op(1) -- unwiden(0) or widen(1)
  // op(2) -- unnarrow(0) or narrow(1)
  val op_int2float = MuxCase(
    0.U, Array(
      fn.op_is(FCvtFun_X2F)  -> "b001".U,
      fn.op_is(FCvtFun_XU2F) -> "b000".U,
      fn.op_is(FCvtFun_X2WF)  -> "b011".U,
      fn.op_is(FCvtFun_XU2WF) -> "b010".U,
      fn.op_is(FCvtFun_X2NF)  -> "b101".U,
      fn.op_is(FCvtFun_XU2NF) -> "b100".U
    ))

  def stagesFConv = 1
  val isNarrowOp = fn.op_is(FCvtFun_X2NF,FCvtFun_XU2NF)
  val notNarrowOp = fn.op_is(FCvtFun_X2F,FCvtFun_XU2F,FCvtFun_X2WF,FCvtFun_XU2WF)


                               //  X2F(SEW32)  X2WF(SEW16)  X2NF(SEW32)
  // results_int2float(0)._1  --> (int32->f32, int16->f32, int64->f32)    // int16->f32 require floating-point support FP16
                               //  X2F(SEW16)  X2WF(SEW8)   X2NF(SEW16)
  // results.int2float(1)._1  --> (int16->f16, int8->f16,  int32->f16)    // int8->f16 require floating-point support FP8 which is impossible
                               //  X2F(SEW64)  X2WF(SEW32)  X2NF(SEW64)
  // results.int2float(2)._1  --> (int64->f64, int32->f64, int128->f64)   // int32->f64 require floating-point support FP32
                               //  X2F(SEW128)  X2WF(SEW64)   X2NF(SEW128)
  // results.int2float(3)._1  --> (int128->f128, int64->f128, int256->f128)  //  int64->f128 require floating-point support FP64
  val results_int2float = 
    List(     (FPS, f32_en, List((e32,  32,  e32Depth),  (e16, 16, f16Depth), (e64,  64,  e64Depth)),  f32Depth,  ieee_sp _, (8, 24))) ++
 (F16).option((FPH, f16_en, List((e16,  16,  e16Depth),  (e8,  8,         0), (e32,  32,  e32Depth)),  f16Depth,  ieee_hp _, (5, 11))) ++
 (F64).option((FPD, f64_en, List((e64,  64,  e64Depth),  (e32, 32, f32Depth), (e128, 128, e128Depth)), f64Depth,  ieee_dp _, (11, 53))) ++
(F128).option((FPQ, f128_en,List((e128, 128, e128Depth), (e64, 64, f64Depth), (e256, 256, e256Depth)), f128Depth, ieee_qp _, (16, 112))) map {
     case (fp, v0en, ele_list, fDepth, ieee, (exp, sig)) => {
      val results_fp = ele_list.filter(_._3 != 0) map {
       case (in, wd, eDepth) => {
        val out = Wire(Vec(fDepth, UInt((exp+sig+1).W)))
        val exc = Wire(Vec(fDepth, UInt(VPUConstants.FFLAGS_SZ.W)))
        val loop = fDepth min eDepth

        for(i <- 0 until loop) {
          val my_log2Ceil = log2Ceil(wd).U(3.W)
          val valid = (((io.vsew === (log2Ceil(wd)-3).U) && notNarrowOp) || ((io.vsew === (my_log2Ceil-4.U)) && isNarrowOp)) && io.req.valid
          val rm = dgate(valid, fn.rm)
          val op = dgate(valid, op_int2float)
          val data_in = dgate(valid, in(i))
          val int2fp = Module(new hardfloat.INToRecFN(wd, exp, sig)) 
          val name = "int" + wd.toString + "_2_fp" + (exp+sig).toString + "_inst"
          int2fp.suggestName(name)
          int2fp.io.signedIn := RegNext(op(0), 0.U)
          int2fp.io.in := RegNext(data_in, 0.U)
          int2fp.io.roundingMode := RegNext(rm, 0.U)
          int2fp.io.detectTininess := hardfloat.consts.tininess_afterRounding
          out(i) := (Pipe(true.B, int2fp.io.out, 1).bits)
          exc(i) := Pipe(true.B, int2fp.io.exceptionFlags, 1).bits
        }
        // cleanup
        for(i <- loop until fDepth) {
          out(i) := 0.U 
          exc(i) := 0.U
        }
        (out, exc)
       }
      }

      val expect_out = 
        if(results_fp.size == 3)  // X[U]2F, X[U]2WF, X[U]2NF
          MuxCase(results_fp(0)._1, Array(
            (op_int2float(2,1) === "b01".U) -> results_fp(1)._1,
            (op_int2float(2,1) === "b10".U) -> results_fp(2)._1
          ))
        else if((results_fp.size == 2) && (ele_list(1)._3 != 0))    // X[U]2F, X[U]2WF
          Mux(op_int2float(1) === 0.U, results_fp(0)._1, results_fp(1)._1) 
        else if((results_fp.size == 2) && (ele_list(2)._3 != 0))    // X[U]2F, X[U]2NF
          Mux(op_int2float(2) === 0.U, results_fp(0)._1, results_fp(1)._1) 
        else 
          results_fp(0)._1  // X[U]2F

      val expect_exc = 
        if(results_fp.size == 3)  // X[U]2F, X[U]2WF, X[U]2NF
          MuxCase(results_fp(0)._2, Array(
            (op_int2float(2,1) === "b01".U) -> results_fp(1)._2,
            (op_int2float(2,1) === "b10".U) -> results_fp(2)._2
          ))
        else if((results_fp.size == 2) && (ele_list(1)._3 != 0))    // X[U]2F, X[U]2WF
          Mux(op_int2float(1) === 0.U, results_fp(0)._2, results_fp(1)._2) 
        else if((results_fp.size == 2) && (ele_list(2)._3 != 0))    // X[U]2F, X[U]2NF
          Mux(op_int2float(2) === 0.U, results_fp(0)._2, results_fp(1)._2) 
        else 
          results_fp(0)._2  // X[U]2F
      (expect_out, expect_exc)
   }
  }

  val mux  = Wire(new VIntToFPResult)

  (F32, F16, F64, F128) match {
    case (true, false, false, false) => { // F32
      mux.vIntToFPOut.f32 := results_int2float(0)._1
      mux.exc.exc32 := results_int2float(0)._2
    }
    case (true, true, false, false) => { // F32, F16
      mux.vIntToFPOut.f32 := results_int2float(0)._1
      mux.vIntToFPOut.f16 := results_int2float(1)._1
      mux.exc.exc32 := results_int2float(0)._2
      mux.exc.exc16 := results_int2float(1)._2
    }
    case (true, false, true, false) => { // F32, F64
      mux.vIntToFPOut.f32 := results_int2float(0)._1
      mux.vIntToFPOut.f64 := results_int2float(1)._1
      mux.exc.exc32 := results_int2float(0)._2
      mux.exc.exc64 := results_int2float(1)._2
    }
    case (true, true, true, false) => { // F32, F16, F64
      mux.vIntToFPOut.f32 := results_int2float(0)._1
      mux.vIntToFPOut.f16 := results_int2float(1)._1
      mux.vIntToFPOut.f64 := results_int2float(2)._1
      mux.exc.exc32 := results_int2float(0)._2
      mux.exc.exc16 := results_int2float(1)._2
      mux.exc.exc64 := results_int2float(2)._2
    }
    case (true, false, true, true) => { // F32, F64, F128
      mux.vIntToFPOut.f32 := results_int2float(0)._1
      mux.vIntToFPOut.f64 := results_int2float(1)._1
      mux.vIntToFPOut.f128 := results_int2float(2)._1
      mux.exc.exc32 := results_int2float(0)._2
      mux.exc.exc64 := results_int2float(1)._2
      mux.exc.exc128 := results_int2float(2)._2
    }
    case (true, true, true, true) => { // F32, F16, F64, F128
      mux.vIntToFPOut.f32 := results_int2float(0)._1
      mux.vIntToFPOut.f16 := results_int2float(1)._1
      mux.vIntToFPOut.f64 := results_int2float(2)._1
      mux.vIntToFPOut.f128 := results_int2float(3)._1
      mux.exc.exc32 := results_int2float(0)._2
      mux.exc.exc16 := results_int2float(1)._2
      mux.exc.exc64 := results_int2float(2)._2
      mux.exc.exc128 := results_int2float(3)._2
    }
    case _ => {
      // do nothing
    }
  }

  val valid_real = Pipe(true.B, io.req.valid, 2).bits
  io.resp <> Pipe(valid_real, mux, 0)

}

