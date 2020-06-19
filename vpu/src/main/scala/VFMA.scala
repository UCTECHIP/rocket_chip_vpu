// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFMA.scala
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

//class VFPUFMAPipe(params: VPUParams)(implicit p: Parameters) extends VModule(params)(p) {
class VFMA(params: VPUParams) extends VModule(params) {
   val VFRED = true
   class VFXUFn(sz_op: Int) extends Bundle {
     val rm = UInt(VPUConstants.FRM_SZ.W)
     val op = UInt(sz_op.W)

      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
     def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
     override def cloneType = new VFXUFn(sz_op).asInstanceOf[this.type]
   }

  class VFMAOperand extends Bundle {
     val fn = new VFXUFn(FMAFun_SZ)
     val vsrc1e = new FUCBVec
     val vsrc2e = new FUCBVec
     val vdvs3  = new FUCBVec
     val v0en   = new FSEW1wVec
     override def cloneType = new VFMAOperand().asInstanceOf[this.type]
    }

   class VFMAResult extends Bundle {
      val vFMAOut = new FUCBVec
      //val exc       = UInt(VPUConstants.FFLAGS_SZ.W)
      val exc      = new FFLAGSVec
      override def cloneType = new VFMAResult().asInstanceOf[this.type]
    }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val prodAddWhat = Input(UInt(1.W))
     val kill  = Input(Bool())
     val redsum = Input(Bool())
     val firstElen = Input(Bool())
     val isVd2SEW = Input(Bool())
     val req   = Flipped(Valid(new VFMAOperand))
     val resp  = Valid(new VFMAResult)
  })

if(!VFRED)
{
  val vfma = Module(new VFPUFMAPipeImp(params))
  vfma.io.req := io.req
  io.resp := vfma.io.resp
  vfma.io.vsew := io.vsew
  vfma.io.prodAddWhat := io.prodAddWhat
  vfma.io.kill := io.kill
  vfma.io.isVd2SEW := io.isVd2SEW
}
else
{
  val vfma = Module(new VFPUFMAPipeImp(params))
  val muxReq  = Wire(new VFMAOperand)

  val s_ready :: s_calc :: Nil = Enum(2)
  val state = RegInit(s_ready)
  val cnt = RegInit(0.U(2.W))

  val again    = Mux(state === s_calc && cnt === 1.U && io.redsum, RegNext(vfma.io.resp.fire(), false.B), false.B)

  when(state === s_ready) { cnt := 0.U }
  when(state === s_calc ) { cnt := Mux(vfma.io.resp.fire(), cnt + 1.U, cnt) } 

  switch(state){
    is(s_ready){
      when(io.req.fire())
        { state := s_calc }
    }
    is(s_calc){
      when(io.resp.fire()) { state := s_ready}
    }
  }

  val resF32 = RegEnable(vfma.io.resp.bits.vFMAOut.f32(0), 0.U, vfma.io.resp.valid)
  val resF64 = if(F64) RegEnable(vfma.io.resp.bits.vFMAOut.f64(0), 0.U, vfma.io.resp.valid) else 0.U

  muxReq.fn := io.req.bits.fn
  muxReq.vsrc1e.f32(0) := Mux(io.redsum && !io.firstElen, resF32, io.req.bits.vsrc1e.f32(0))
  muxReq.vsrc1e.f32(1) := io.req.bits.vsrc1e.f32(1)  // Donnot Care
  if(F64) muxReq.vsrc1e.f64(0) := Mux(io.redsum && !io.firstElen, resF64, io.req.bits.vsrc1e.f64(0))

  muxReq.vsrc2e.f32(0) := Mux(io.redsum, Mux(io.req.bits.v0en.f32(cnt) === 1.U, io.req.bits.vsrc2e.f32(cnt), 0.U), io.req.bits.vsrc2e.f32(0))
  muxReq.vsrc2e.f32(1) := io.req.bits.vsrc2e.f32(1) // Donnot Care
  if(F64) muxReq.vsrc2e.f64(0) := Mux(io.redsum, Mux(io.req.bits.v0en.f64(0) === 1.U, io.req.bits.vsrc2e.f64(0), 0.U), io.req.bits.vsrc2e.f64(0))

  muxReq.vdvs3.f32(0) := io.req.bits.vdvs3.f32(0)
  muxReq.vdvs3.f32(1) := io.req.bits.vdvs3.f32(1)
  if(F64) muxReq.vdvs3.f64(0) := io.req.bits.vdvs3.f64(0)

  muxReq.v0en.f32(0) := Mux(io.redsum, 1.U, io.req.bits.v0en.f32(0))
  muxReq.v0en.f32(1) := Mux(io.redsum, 1.U, io.req.bits.v0en.f32(1))
  if(F64) muxReq.v0en.f64(0) := Mux(io.redsum, 1.U, io.req.bits.v0en.f64(0))

  vfma.io.req <> Pipe(io.req.valid || (again && io.vsew === FPS && !io.isVd2SEW), muxReq, 0)
  //vfma.io.req := io.req

  //io.resp := vfma.io.resp
  io.resp.bits.vFMAOut := vfma.io.resp.bits.vFMAOut
  io.resp.bits.exc := vfma.io.resp.bits.exc
  io.resp.valid := Mux(io.redsum, vfma.io.resp.valid && !(cnt === 0.U && io.vsew === FPS && !io.isVd2SEW), vfma.io.resp.valid)

  vfma.io.vsew := io.vsew
  vfma.io.prodAddWhat := io.prodAddWhat
  vfma.io.kill := io.kill
  vfma.io.isVd2SEW := io.isVd2SEW
}


}



class VFPUFMAPipeImp(params: VPUParams) extends VModule(params) {

   class VFXUFn(sz_op: Int) extends Bundle {
     val rm = UInt(VPUConstants.FRM_SZ.W)
     val op = UInt(sz_op.W)

      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
     def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
     override def cloneType = new VFXUFn(sz_op).asInstanceOf[this.type]
   }

  class VFMAOperand extends Bundle {
     val fn = new VFXUFn(FMAFun_SZ)
     val vsrc1e = new FUCBVec
     val vsrc2e = new FUCBVec
     val vdvs3  = new FUCBVec
     val v0en   = new FSEW1wVec
     override def cloneType = new VFMAOperand().asInstanceOf[this.type]
    }

   class VFMAResult extends Bundle {
      val vFMAOut = new FUCBVec
      //val exc       = UInt(VPUConstants.FFLAGS_SZ.W)
      val exc      = new FFLAGSVec
      override def cloneType = new VFMAResult().asInstanceOf[this.type]
    }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val prodAddWhat = Input(UInt(1.W))
     val kill  = Input(Bool())
     val isVd2SEW = Input(Bool())
     val req   = Flipped(Valid(new VFMAOperand))
     val resp  = Valid(new VFMAResult)
  })

  def stagesQFMA = 3
  def stagesDFMA = 3
  def stagesSFMA = 3
  def stagesHFMA = 3
  val fn = io.req.bits.fn

//  val one_qp = "h3FF0_0000_0000_0000_0000_0000_0000_0000".U(128.W)
//  val one_dp = "h3FF0_0000_0000_0000".U(64.W)
//  val one_sp = "h3F80_0000".U(32.W)
//  val one_hp = "h3C00".U(16.W)

  val one_hp = "h0_8000".U(17.W)
  val one_sp = "h0_8000_0000".U(33.W)
  val one_dp = "h0_8000_0000_0000_0000".U(65.W)
  val one_qp = "h0_7FF0_8000_0000_0000_0000_0000_0000_0000".U(129.W)


  val fma_op = MuxCase(0.U(2.W), Array(
     fn.op_is(FMAFun_Sub, FMAFun_MSub, FMAFun_RSub) -> "b01".U(2.W),
     fn.op_is(FMAFun_NMSub)            -> "b10".U(2.W),
     fn.op_is(FMAFun_NMAdd)            -> "b11".U(2.W)
  ))

  val f16_vsrc1 = io.req.bits.vsrc1e.f16
  val f16_vsrc2 = io.req.bits.vsrc2e.f16
  val f16_vdvs3 = io.req.bits.vdvs3.f16
  val f16_en  = io.req.bits.v0en.f16

  val f32_vsrc1 = io.req.bits.vsrc1e.f32
  val f32_vsrc2 = io.req.bits.vsrc2e.f32
  val f32_vdvs3 = io.req.bits.vdvs3.f32
  val f32_en  = io.req.bits.v0en.f32

  val f64_vsrc1 = io.req.bits.vsrc1e.f64
  val f64_vsrc2 = io.req.bits.vsrc2e.f64
  val f64_vdvs3 = io.req.bits.vdvs3.f64
  val f64_en  = io.req.bits.v0en.f64

  val f128_vsrc1 = io.req.bits.vsrc1e.f128
  val f128_vsrc2 = io.req.bits.vsrc2e.f128
  val f128_vdvs3 = io.req.bits.vdvs3.f128
  val f128_en  = io.req.bits.v0en.f128

  val results = 
         List((stagesSFMA, one_sp, FPS, f32_vsrc1, f32_vsrc2, f32_vdvs3, f32Depth, f32_en, recode_sp _, ieee_sp _, (8, 24))) ++
 (F16).option((stagesHFMA, one_hp, FPH, f16_vsrc1, f16_vsrc2, f16_vdvs3, f16Depth, f16_en, recode_hp _, ieee_hp _, (5, 11))) ++
 (F64).option((stagesDFMA, one_dp, FPD, f64_vsrc1, f64_vsrc2, f64_vdvs3, f64Depth, f64_en, recode_dp _, ieee_dp _, (11, 53))) ++
(F128).option((stagesQFMA, one_qp, FPQ, f128_vsrc1,f128_vsrc2,f128_vdvs3,f128Depth,f128_en,recode_qp _, ieee_qp _, (15, 113))) map {
   case (stages, one, fp, vsrc1, vsrc2, vdvs3, depth, v0en, recode, ieee, (exp, sig)) => {
     val val_fp = Mux(io.isVd2SEW, ((io.vsew+1.U) === fp), (io.vsew === fp))
     val out = Wire(Vec(depth, UInt((exp+sig+1).W)))
     val exc = Wire(Vec(depth, UInt((5).W)))
     for(i <- 0 until depth) {
       val fma = Module(new MulAddRecFNPipe(stages min 2, exp, sig))
       val name = "fmaInst_f" + (exp+sig).toString
       fma.suggestName(name)
       val valid = (v0en(i) === 1.U) && val_fp & io.req.valid
       val fma_multiplicand = Mux(fn.op_is(FMAFun_Sub), vsrc2(i), vsrc1(i))
       val fma_multiplier = MuxCase(vdvs3(i), Array(
          fn.op_is(FMAFun_Add, FMAFun_Sub, FMAFun_RSub) -> one((exp+sig-1), 0),
          (fn.op_is(FMAFun_Mul) || io.prodAddWhat === 1.U) -> vsrc2(i)
       ))
       val fma_addend = MuxCase(vsrc2(i), Array(
          fn.op_is(FMAFun_Sub) -> vsrc1(i),
          fn.op_is(FMAFun_Mul) -> 0.U((exp+sig).W),
          (io.prodAddWhat === 1.U) -> vdvs3(i)
        )) 
       fma.io.validin := Pipe(true.B, valid, 1).bits
       fma.io.op := RegNext(fma_op, 0.U)
       fma.io.a := RegNext((fma_multiplicand), 0.U)
       fma.io.b := RegNext((fma_multiplier), 0.U)
       fma.io.c := RegNext((fma_addend), 0.U)
       fma.io.roundingMode := RegNext(io.req.bits.fn.rm, 0.U)
       fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
       out(i) := Pipe(fma.io.validout, (fma.io.out), (stages-2)).bits
       exc(i) := Pipe(fma.io.validout, fma.io.exceptionFlags, (stages-2)).bits
    }
    (out, exc)
   }
  }

  val mux  = Wire(new VFMAResult)
  val real_vsew = Mux(io.isVd2SEW, (io.vsew+1.U), io.vsew)

  (F32, F16, F64, F128) match {
    case (true, false, false, false) => { // F32
       mux.vFMAOut.f32 := results(0)._1
       //mux.exc := results(0)._2.reduce(_|_)
       mux.exc.exc32 := results(0)._2
    }
    case (true, true, false, false) => { // F32, F16
       mux.vFMAOut.f32 := results(0)._1
       mux.vFMAOut.f16 := results(1)._1
       //mux.exc := MuxCase(0.U, Array(
       //  (real_vsew === FPS) -> results(0)._2.reduce(_|_),
       //  (real_vsew === FPH) -> results(1)._2.reduce(_|_)
       //))
       mux.exc.exc32 := results(0)._2
       mux.exc.exc16 := results(1)._2
    }
    case (true, false, true, false) => { // F32, F64
       mux.vFMAOut.f32 := results(0)._1
       mux.vFMAOut.f64 := results(1)._1
       //mux.exc := MuxCase(0.U, Array(
       //  (real_vsew === FPS) -> results(0)._2.reduce(_|_),
       //  (real_vsew === FPD) -> results(1)._2.reduce(_|_)
       //))
       mux.exc.exc32 := results(0)._2
       mux.exc.exc64 := results(1)._2
    }
    case (true, true, true, false) => { // F32, F16, F64
       mux.vFMAOut.f32 := results(0)._1
       mux.vFMAOut.f16 := results(1)._1
       mux.vFMAOut.f64 := results(2)._1
       //mux.exc := MuxCase(0.U, Array(
       //  (real_vsew === FPS) -> results(0)._2.reduce(_|_),
       //  (real_vsew === FPH) -> results(1)._2.reduce(_|_),
       //  (real_vsew === FPD) -> results(2)._2.reduce(_|_)
       //))
       mux.exc.exc32 := results(0)._2
       mux.exc.exc16 := results(1)._2
       mux.exc.exc64 := results(2)._2
    }
    case (true, false, true, true) => { // F32, F64, F128
       mux.vFMAOut.f32 := results(0)._1
       mux.vFMAOut.f64 := results(1)._1
       mux.vFMAOut.f128 := results(2)._1
       //mux.exc := MuxCase(0.U, Array(
       //  (real_vsew === FPS) -> results(0)._2.reduce(_|_),
       //  (real_vsew === FPD) -> results(1)._2.reduce(_|_),
       //  (real_vsew === FPQ) -> results(2)._2.reduce(_|_)
       //))
       mux.exc.exc32 := results(0)._2
       mux.exc.exc64 := results(1)._2
       mux.exc.exc128 := results(2)._2
    }
    case (true, true, true, true) => { // F32, F16, F64, F128
       mux.vFMAOut.f32 := results(0)._1
       mux.vFMAOut.f16 := results(1)._1
       mux.vFMAOut.f64 := results(2)._1
       mux.vFMAOut.f128 := results(3)._1
       //mux.exc := MuxCase(0.U, Array(
       //  (real_vsew === FPS) -> results(0)._2.reduce(_|_),
       //  (real_vsew === FPH) -> results(1)._2.reduce(_|_),
       //  (real_vsew === FPD) -> results(2)._2.reduce(_|_),
       //  (real_vsew === FPQ) -> results(3)._2.reduce(_|_)
       //))
       mux.exc.exc32 := results(0)._2
       mux.exc.exc16 := results(1)._2
       mux.exc.exc64 := results(2)._2
       mux.exc.exc128 := results(3)._2
    }
    case _ => {
      // do nothing
    }
  }

  val valid_real = Pipe(true.B, io.req.valid, 4).bits
  io.resp <> Pipe(valid_real, mux, 0)
}



