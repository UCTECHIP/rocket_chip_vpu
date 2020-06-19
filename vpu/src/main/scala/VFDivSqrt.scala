// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFDivSqrt.scala
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

//class VFDivSqrt(params: VPUParams)(implicit p: Parameters) extends VModule(params)(p) {
class VFDivSqrt(params: VPUParams) extends VModule(params) {
    class VFDUFn(sz_op: Int) extends Bundle {
      val rm = UInt(VPUConstants.FRM_SZ.W)
      val op = UInt(sz_op.W)

      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
      def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
      override def cloneType = new VFDUFn(sz_op).asInstanceOf[this.type]
    }

   class VFDivSqrtOperand extends Bundle {
      val fn = new VFDUFn(FDivFun_SZ)
      val vsrc1e = new FUCBVec
      val vsrc2e = new FUCBVec
      val v0en   = new FSEW1wVec
      override def cloneType = new VFDivSqrtOperand().asInstanceOf[this.type]
     }

    class VFDivSqrtResult extends Bundle {
       val vFDivSqrtOut = new FUCBVec
       //val exc      = UInt(VPUConstants.FFLAGS_SZ.W)
       val exc      = new FFLAGSVec
       override def cloneType = new VFDivSqrtResult().asInstanceOf[this.type]
     }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val kill  = Input(Bool())
     val req   = Flipped(Decoupled(new VFDivSqrtOperand))
     val resp  = Decoupled(new VFDivSqrtResult)
  })

  val fn = io.req.bits.fn

  val f16_vsrc1 = io.req.bits.vsrc1e.f16
  val f16_vsrc2 = io.req.bits.vsrc2e.f16
  val f16_en  = io.req.bits.v0en.f16

  val f32_vsrc1 = io.req.bits.vsrc1e.f32
  val f32_vsrc2 = io.req.bits.vsrc2e.f32
  val f32_en  = io.req.bits.v0en.f32

  val f64_vsrc1 = io.req.bits.vsrc1e.f64
  val f64_vsrc2 = io.req.bits.vsrc2e.f64
  val f64_en  = io.req.bits.v0en.f64

  val f128_vsrc1 = io.req.bits.vsrc1e.f128
  val f128_vsrc2 = io.req.bits.vsrc2e.f128
  val f128_en  = io.req.bits.v0en.f128

  val enableFlagPre = Wire(Bool())
  val enableFlagReg = RegInit(false.B)

  enableFlagPre := Mux(io.req.fire(), true.B, Mux(io.resp.fire(), false.B, enableFlagReg))
  enableFlagReg := enableFlagPre

  val results = 
         List((FPS, f32_vsrc1, f32_vsrc2, f32_en, f32Depth, recode_sp _, ieee_sp _, (8, 24))) ++
 (F16).option((FPH, f16_vsrc1, f16_vsrc2, f16_en, f16Depth, recode_hp _, ieee_hp _, (5, 11))) ++
 (F64).option((FPD, f64_vsrc1, f64_vsrc2, f64_en, f64Depth, recode_dp _, ieee_dp _, (11, 53))) ++
(F128).option((FPQ, f128_vsrc1,f128_vsrc2,f128_en,f128Depth,recode_qp _, ieee_qp _, (16, 112))) map {
     case (fp, vsrc1, vsrc2, v0en, fDepth, recode, ieee, (exp, sig)) => {
       val outData = Wire(Vec(fDepth, UInt((exp+sig+1).W)))
       val outValid = Wire(Vec(fDepth, Bool()))
       val inReady = Wire(Vec(fDepth, Bool()))
       val exc = Wire(Vec(fDepth, UInt(5.W)))
       val transValidPre = Wire(Vec(fDepth, Bool()))
       val transValidReg = RegInit(VecInit(Seq.fill(fDepth)(false.B)))
       for(i <- 0 until fDepth) 
       {
         val divSqrt = Module(new hardfloat.DivSqrtRecFN_small(exp, sig, 0))
         val name = "DivSqrt_f" + (exp+sig).toString
         divSqrt.suggestName(name)
         val valid = (io.vsew === fp) && io.req.valid
         val lhs = MuxCase(0.U, Array(
              fn.op_is(FDivFun_Div)  -> vsrc2(i),
              fn.op_is(FDivFun_RDiv) -> vsrc1(i),
              fn.op_is(FDivFun_Sqrt) -> vsrc2(i)
           ))
         val rhs = MuxCase(0.U, Array(
              fn.op_is(FDivFun_Div)  -> vsrc1(i),
              fn.op_is(FDivFun_RDiv) -> vsrc2(i),
              fn.op_is(FDivFun_Sqrt) -> vsrc2(i)
           ))
         divSqrt.io.inValid := Pipe(true.B, (v0en(i) === 1.U) && valid, 1).bits
         divSqrt.io.sqrtOp := Pipe(true.B, fn.op_is(FDivFun_Sqrt), 1).bits
         divSqrt.io.a := (RegNext(lhs, 0.U))
         divSqrt.io.b := (RegNext(rhs, 0.U))
         divSqrt.io.roundingMode := RegNext(io.req.bits.fn.rm, 0.U)
         divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding
      
         outValid(i) := MuxCase(false.B, Array(
            fn.op_is(FDivFun_Sqrt) -> divSqrt.io.outValid_sqrt,
            fn.op_is(FDivFun_Div, FDivFun_RDiv) -> divSqrt.io.outValid_div
         ))
         inReady(i) := divSqrt.io.inReady
         outData(i) := (divSqrt.io.out)
         exc(i) := divSqrt.io.exceptionFlags
         transValidPre(i) := Mux(io.resp.fire(), false.B,
               Mux(outValid(i) || (!(v0en(i) === 1.U) && valid && enableFlagPre), true.B, transValidReg(i)))
         transValidReg(i) := transValidPre(i)
      }
      
      //(outData, exc, outValid, inReady)
      (outData, exc, transValidReg, inReady)
     }
   }
  
  (F32, F16, F64, F128) match {
    case (true, false, false, false) => { // F32
     for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := (results(0)._1)(i)
     //io.resp.bits.exc := results(0)._2.reduce(_|_)
     io.resp.bits.exc.exc32 := results(0)._2
     io.resp.valid := results(0)._3.reduce(_&&_)
     io.req.ready := results(0)._4.reduce(_&&_)
    }
    case (true, true, false, false) => { // F32, F16
     for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := (results(0)._1)(i)
     for(i <- 0 until f16Depth)    io.resp.bits.vFDivSqrtOut.f16(i) := (results(1)._1)(i)
     //io.resp.bits.exc := MuxCase(0.U, Array(
     //  (io.vsew === FPS) -> results(0)._2.reduce(_|_),
     //  (io.vsew === FPH) -> results(1)._2.reduce(_|_)
     //))
     io.resp.bits.exc.exc32 := results(0)._2
     io.resp.bits.exc.exc16 := results(1)._2
     io.resp.valid := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._3.reduce(_&&_),
       (io.vsew === FPH) -> results(1)._3.reduce(_&&_)
     ))
     io.req.ready := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._4.reduce(_&&_),
       (io.vsew === FPH) -> results(1)._4.reduce(_&&_)
     ))
    }
    case (true, false, true, false) => { // F32, F64
     //for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := dgate(io.resp.valid, (results(0)._1)(i))
     //for(i <- 0 until f64Depth)    io.resp.bits.vFDivSqrtOut.f64(i) := dgate(io.resp.valid, (results(1)._1)(i))
     for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := (results(0)._1)(i)
     for(i <- 0 until f64Depth)    io.resp.bits.vFDivSqrtOut.f64(i) := (results(1)._1)(i)
     //io.resp.bits.exc := MuxCase(0.U, Array(
     //  (io.vsew === FPS) -> results(0)._2.reduce(_|_),
     //  (io.vsew === FPD) -> results(1)._2.reduce(_|_)
     //))
     io.resp.bits.exc.exc32 := results(0)._2
     io.resp.bits.exc.exc64 := results(1)._2
     io.resp.valid := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._3.reduce(_&&_),
       (io.vsew === FPD) -> results(1)._3.reduce(_&&_)
      ))
     io.req.ready := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._4.reduce(_&&_),
       (io.vsew === FPD) -> results(1)._4.reduce(_&&_)
     ))
    }
    case (true, true, true, false) => { // F32, F16, F64
     for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := (results(0)._1)(i)
     for(i <- 0 until f16Depth)    io.resp.bits.vFDivSqrtOut.f16(i) := (results(1)._1)(i)
     for(i <- 0 until f64Depth)    io.resp.bits.vFDivSqrtOut.f64(i) := (results(2)._1)(i)
     //io.resp.bits.exc := MuxCase(0.U, Array(
     //  (io.vsew === FPS) -> results(0)._2.reduce(_|_),
     //  (io.vsew === FPH) -> results(1)._2.reduce(_|_),
     //  (io.vsew === FPD) -> results(2)._2.reduce(_|_)
     //))
     io.resp.bits.exc.exc32 := results(0)._2
     io.resp.bits.exc.exc16 := results(1)._2
     io.resp.bits.exc.exc64 := results(2)._2
     io.resp.valid := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._3.reduce(_&&_),
       (io.vsew === FPH) -> results(1)._3.reduce(_&&_),
       (io.vsew === FPD) -> results(2)._3.reduce(_&&_)
     ))
     io.req.ready := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._4.reduce(_&&_),
       (io.vsew === FPH) -> results(1)._4.reduce(_&&_),
       (io.vsew === FPD) -> results(2)._4.reduce(_&&_)
     ))
    }
    case (true, false, true, true) => { // F32, F64, F128
     for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := (results(0)._1)(i)
     for(i <- 0 until f64Depth)    io.resp.bits.vFDivSqrtOut.f64(i) := (results(1)._1)(i)
     for(i <- 0 until f128Depth)    io.resp.bits.vFDivSqrtOut.f128(i) := (results(2)._1)(i)
     //io.resp.bits.exc := MuxCase(0.U, Array(
     //  (io.vsew === FPS) -> results(0)._2.reduce(_|_),
     //  (io.vsew === FPD) -> results(1)._2.reduce(_|_),
     //  (io.vsew === FPQ) -> results(2)._2.reduce(_|_)
     //))
     io.resp.bits.exc.exc32 := results(0)._2
     io.resp.bits.exc.exc64 := results(1)._2
     io.resp.bits.exc.exc128 := results(2)._2
     io.resp.valid := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._3.reduce(_&&_),
       (io.vsew === FPD) -> results(1)._3.reduce(_&&_),
       (io.vsew === FPQ) -> results(2)._3.reduce(_&&_)
     ))
     io.req.ready := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._4.reduce(_&&_),
       (io.vsew === FPD) -> results(1)._4.reduce(_&&_),
       (io.vsew === FPQ) -> results(2)._4.reduce(_&&_)
     ))
    }
    case (true, true, true, true) => { // F32, F16, F64, F128
     for(i <- 0 until f32Depth)    io.resp.bits.vFDivSqrtOut.f32(i) := (results(0)._1)(i)
     for(i <- 0 until f16Depth)    io.resp.bits.vFDivSqrtOut.f16(i) := (results(1)._1)(i)
     for(i <- 0 until f64Depth)    io.resp.bits.vFDivSqrtOut.f64(i) := (results(2)._1)(i)
     for(i <- 0 until f128Depth)   io.resp.bits.vFDivSqrtOut.f128(i) := (results(3)._1)(i)
     //io.resp.bits.exc := MuxCase(0.U, Array(
     //  (io.vsew === FPS) -> results(0)._2.reduce(_|_),
     //  (io.vsew === FPH) -> results(1)._2.reduce(_|_),
     //  (io.vsew === FPD) -> results(2)._2.reduce(_|_),
     //  (io.vsew === FPQ) -> results(3)._2.reduce(_|_)
     //))
     io.resp.bits.exc.exc32 := results(0)._2
     io.resp.bits.exc.exc16 := results(1)._2
     io.resp.bits.exc.exc64 := results(2)._2
     io.resp.bits.exc.exc128 := results(3)._2
     io.resp.valid := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._3.reduce(_&&_),
       (io.vsew === FPH) -> results(1)._3.reduce(_&&_),
       (io.vsew === FPD) -> results(2)._3.reduce(_&&_),
       (io.vsew === FPQ) -> results(3)._2.reduce(_|_)
     ))
     io.req.ready := MuxCase(false.B, Array(
       (io.vsew === FPS) -> results(0)._4.reduce(_&&_),
       (io.vsew === FPH) -> results(1)._4.reduce(_&&_),
       (io.vsew === FPD) -> results(2)._4.reduce(_&&_),
       (io.vsew === FPQ) -> results(3)._2.reduce(_|_)
     ))
    }
    case _ => {
     // do nothing
    }
  }

}



