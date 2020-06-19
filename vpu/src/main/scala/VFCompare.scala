// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFCompare.scala
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

class VFCompare(params: VPUParams) extends VModule(params) {
    class VFXUFn(sz_op: Int) extends Bundle {
      val rm = UInt(VPUConstants.FRM_SZ.W)
      val op = UInt(sz_op.W)

      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
      def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
      override def cloneType = new VFXUFn(sz_op).asInstanceOf[this.type]
    }

   class VFCmpOperand extends Bundle {
      val fn = new VFXUFn(CmpFun_SZ)
      val vsrc1e = new FUCBVec
      val vsrc2e = new FUCBVec
      val v0en   = new FSEW1wVec
      override def cloneType = new VFCmpOperand().asInstanceOf[this.type]
     }

    class VFCmpResult extends Bundle {
       val vFMinMaxCmpOut = new FUCBVec
       val vFCmpOut = new FSEW1wVec
       val exc      = new FFLAGSVec
       //val exc       = UInt(VPUConstants.FFLAGS_SZ.W)
       override def cloneType = new VFCmpResult().asInstanceOf[this.type]
     }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val vlmul = Input(UInt(VLMUL_SZ.W))
     val redsum = Input(Bool())
     val firstElen = Input(Bool())
     val req   = Flipped(Valid(new VFCmpOperand))
     val resp  = Valid(new VFCmpResult)
  })

  val vfcmp = Module(new VFCompareImp(params))
  val muxReq  = Wire(new VFCmpOperand)

  val s_ready :: s_calc :: Nil = Enum(2)
  val state = RegInit(s_ready)
  val cnt = RegInit(0.U(2.W))

  val again    = Mux(state === s_calc && cnt === 1.U && io.redsum, RegNext(vfcmp.io.resp.fire(), false.B), false.B)

  when(state === s_ready) { cnt := 0.U }
  when(state === s_calc ) { cnt := Mux(vfcmp.io.resp.fire(), cnt + 1.U, cnt) } 

  switch(state){
    is(s_ready){
      when(io.req.fire())
        { state := s_calc }
    }
    is(s_calc){
      when(io.resp.fire()) { state := s_ready}
    }
  }
//  val negInfRec_sp = "hFF80_0000".U(32.W)
//  val negInfRec_dp = "hFFF0_0000_0000_0000".U(64.W)

//  val posInfRec_sp = "h7F80_0000".U(32.W)
//  val posInfRec_dp = "h7FF0_0000_0000_0000".U(64.W)

  val posInfRec_hp = "h0_C000".U(17.W)
  val posInfRec_sp = "h0_C000_0000".U(33.W)
  val posInfRec_dp = "h0_C000_0000_0000_0000".U(65.W)
  val posInfRec_qp = "h0_BFF0_8000_0000_0000_0000_0000_0000_0000".U(129.W)

  val negInfRec_hp = "h1_C000".U(17.W)
  val negInfRec_sp = "h1_C000_0000".U(33.W)
  val negInfRec_dp = "h1_C000_0000_0000_0000".U(65.W)
  val negInfRec_qp = "h1_BFF0_8000_0000_0000_0000_0000_0000_0000".U(129.W)

  val resF32 = RegEnable(vfcmp.io.resp.bits.vFMinMaxCmpOut.f32(0), 0.U, vfcmp.io.resp.valid)
  val resF64 = if(F64) RegEnable(vfcmp.io.resp.bits.vFMinMaxCmpOut.f64(0), 0.U, vfcmp.io.resp.valid) else 0.U

  val infRec_sp = Mux(io.req.bits.fn.op_is(CmpFun_Max), negInfRec_sp, posInfRec_sp)
  val infRec_dp = Mux(io.req.bits.fn.op_is(CmpFun_Max), negInfRec_dp, posInfRec_dp)

  muxReq.fn := io.req.bits.fn
  muxReq.vsrc1e.f32(0) := Mux(io.redsum && !io.firstElen, resF32, io.req.bits.vsrc1e.f32(0))
  muxReq.vsrc1e.f32(1) := io.req.bits.vsrc1e.f32(1)  // Donnot Care
  if(F64) muxReq.vsrc1e.f64(0) := Mux(io.redsum && !io.firstElen, resF64, io.req.bits.vsrc1e.f64(0))

  muxReq.vsrc2e.f32(0) := Mux(io.redsum, Mux(io.req.bits.v0en.f32(cnt) === 1.U, io.req.bits.vsrc2e.f32(cnt), infRec_sp), io.req.bits.vsrc2e.f32(0))
  muxReq.vsrc2e.f32(1) := io.req.bits.vsrc2e.f32(1) // Donnot Care
  if(F64) muxReq.vsrc2e.f64(0) := Mux(io.redsum, Mux(io.req.bits.v0en.f64(0) === 1.U, io.req.bits.vsrc2e.f64(0), infRec_dp), io.req.bits.vsrc2e.f64(0))

  muxReq.v0en.f32(0) := Mux(io.redsum, 1.U, io.req.bits.v0en.f32(0))
  muxReq.v0en.f32(1) := Mux(io.redsum, 1.U, io.req.bits.v0en.f32(1))
  if(F64) muxReq.v0en.f64(0) := Mux(io.redsum, 1.U, io.req.bits.v0en.f64(0))

  vfcmp.io.req <> Pipe(io.req.valid || (again && io.vsew === FPS), muxReq, 0)
  //vfcmp.io.req := io.req

  //io.resp := vfcmp.io.resp
  io.resp.bits.vFMinMaxCmpOut := vfcmp.io.resp.bits.vFMinMaxCmpOut
  io.resp.bits.vFCmpOut := vfcmp.io.resp.bits.vFCmpOut
  io.resp.bits.exc := vfcmp.io.resp.bits.exc
  io.resp.valid := Mux(io.redsum, vfcmp.io.resp.valid && !(cnt === 0.U && io.vsew === FPS), vfcmp.io.resp.valid)

  vfcmp.io.vsew := io.vsew
  vfcmp.io.vlmul := io.vlmul
}


class VFCompareImp(params: VPUParams) extends VModule(params)
   with HasFPUParameters { 
    def xLen = 64
    def fLen = 64

    class VFXUFn(sz_op: Int) extends Bundle {
      val rm = UInt(VPUConstants.FRM_SZ.W)
      val op = UInt(sz_op.W)


      def dgate(valid: Bool) = DataGating.dgate(valid, this.asUInt).asTypeOf(this.cloneType)
      def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
      override def cloneType = new VFXUFn(sz_op).asInstanceOf[this.type]
    }

   class VFCmpOperand extends Bundle {
      val fn = new VFXUFn(CmpFun_SZ)
      val vsrc1e = new FUCBVec
      val vsrc2e = new FUCBVec
      val v0en   = new FSEW1wVec
      override def cloneType = new VFCmpOperand().asInstanceOf[this.type]
     }

    class VFCmpResult extends Bundle {
       val vFMinMaxCmpOut = new FUCBVec
       val vFCmpOut = new FSEW1wVec
       val exc      = new FFLAGSVec
       override def cloneType = new VFCmpResult().asInstanceOf[this.type]
     }

  val io = IO(new Bundle {
     val vsew  = Input(UInt(VSEW_SZ.W))
     val vlmul = Input(UInt(VLMUL_SZ.W))
     val req   = Flipped(Valid(new VFCmpOperand))
     val resp  = Valid(new VFCmpResult)
  })

  def stagesFCmp = 1

  val fn = io.req.bits.fn.dgate(io.req.valid)

  val f16_in1 = io.req.bits.vsrc1e.f16
  val f32_in1 = io.req.bits.vsrc1e.f32
  val f64_in1 = io.req.bits.vsrc1e.f64
  val f128_in1 = io.req.bits.vsrc1e.f128

  val f16_in2 = io.req.bits.vsrc2e.f16
  val f32_in2 = io.req.bits.vsrc2e.f32
  val f64_in2 = io.req.bits.vsrc2e.f64
  val f128_in2 = io.req.bits.vsrc2e.f128

  val f16_en  = io.req.bits.v0en.f16
  val f32_en  = io.req.bits.v0en.f32
  val f64_en  = io.req.bits.v0en.f64
  val f128_en  = io.req.bits.v0en.f128

  val wqp = (16, 112)
  val wdp = (11, 53)
  val wsp = (8, 24)
  val whp = (5, 11)

  // fmfeq.vv vd, vs2, vs1, vm  ==>  vs2 < vs1? 1 else 0
  val val_cmp = fn.op_is(CmpFun_EQ, CmpFun_NE, CmpFun_LT, CmpFun_LE, CmpFun_GT, CmpFun_Min, CmpFun_Max, CmpFun_GE)

  // ins(0) = (vsrc1e.f32, vsrc2e.f32, depth, wsp) --> for UCB
  // ins(1) = (vsrc1e.f64, vsrc2e.f64, depth, wdp) --> for UCB
  val ins = 
    List(     (FPS, 32, f32_in1,  f32_in2, f32Depth,  recode_sp _, wsp, f32_en)) ++
 (F16).option((FPH, 16, f16_in1,  f16_in2, f16Depth,  recode_hp _, whp, f16_en)) ++
 (F64).option((FPD, 64, f64_in1,  f64_in2, f64Depth,  recode_dp _, wdp, f64_en)) ++
(F128).option((FPQ, 128,f128_in1, f128_in2,f128Depth, recode_qp _, wqp, f128_en)) map {
    case (fp, wd, in1, in2, depth, recode, wp, v0en) => {
      val valid = (io.vsew === fp) && val_cmp && io.req.valid
      val out1 = Wire(Vec(depth, UInt((wd+1).W))) 
      val out2 = Wire(Vec(depth, UInt((wd+1).W))) 
      for(i <- 0 until depth)
      {
        out1(i) := (dgate(valid && (v0en(i) === 1.U), in1(i)))
        out2(i) := (dgate(valid && (v0en(i) === 1.U), in2(i)))
      }
      (out1, out2, depth, wp)
   }
  }

  // cmps(0) = f32(0).cmp, f32(1).cmp, f32(2).cmp, ... 
  //   cmps(0)(0).lt represent whether vsrc2e.f32(0) < vsrc1e.f32(0)
  // cmps(1) = f64(0).cmp, f64(1).cmp, f64(2).cmp, ...
  //   cmps(1)(0).lt represent whether vsrc2e.f64(0) < vsrc1e.f64(0)
  val cmps = ins map {
      case (input1, input2, depth, (exp, sig)) => {
       val results = for(i <- 0 until depth) yield {
         val comp = Module(new hardfloat.CompareRecFN(exp, sig))
         val name = "compInst_float" + (exp+sig).toString
         comp.suggestName(name)
         comp.io.a := input2(i)
         comp.io.b := input1(i)
         comp.io.signaling := !fn.op_is(CmpFun_EQ, CmpFun_NE)
         comp.io
       }
       results
     }
    }

  
  val minmax_results = 
   List(          (f32_in1, f32_in2, FType.S)) ++ 
     (F16).option((f16_in1, f16_in2, FType(whp._1, whp._2))) ++
     (F64).option((f64_in1, f64_in2, FType.D)) ++
    (F128).option((f128_in1,f128_in2,FType(wqp._1, wqp._2))) zip cmps zip ins map {
      case (((vsrc1e, vsrc2e, fType), cmp), (input1, input2, depth, (exp, sig))) => {
        val minmax = Wire(Vec(depth, UInt((exp+sig+1).W)))
        val hasSNaN = Wire(Vec(depth, Bool()))
        val hasNaN = Wire(Vec(depth, Bool()))
        for(i <- 0 until depth) {
          val less = cmp(i).lt || (input2(i).asSInt < 0.S && input1(i).asSInt >= 0.S)
          val in1_nan = fType.isNaN(input1(i))
          val in2_nan = fType.isNaN(input2(i))
          //val isInvalid = fType.isSNaN(input1(i)) || fType.isSNaN(input2(i))
          val isNaNOut = (in1_nan && in2_nan)
          val want_min = in1_nan || (fn.op_is(CmpFun_Min, CmpFun_LT) === less) && !in2_nan
          val in1_minmax = dgate(io.req.valid, vsrc1e(i))
          val in2_minmax = dgate(io.req.valid, vsrc2e(i))
          val qnan = fType.qNaN
          //val ieeeNaN = if(fType == FType.S || fType == FType.D) ieee(qnan, fType) else qnan
          val ieeeNaN =  qnan
          minmax(i) := Mux(isNaNOut, ieeeNaN, Mux(want_min, in2_minmax, in1_minmax))
          hasSNaN(i) := fType.isSNaN(input1(i)) || fType.isSNaN(input2(i))
          hasNaN(i) := in1_nan || in2_nan
       }
       (minmax, hasSNaN, hasNaN, depth)
     }
    }

  // cmps_results(0) = f32(0).cmp_result, f32(1).cmp_result, f32(2).cmp_results, ... 
  // cmps_results(1) = f64(0).cmp_result, f64(1).cmp_result, f64(2).cmp_results, ...
  val cmp_results = cmps zip minmax_results map {
    case (cmp, (minmax, hasSNaN, hasNaN, depth)) => {
      val flags = Wire(Vec(depth, UInt(1.W)))
      val exc = Wire(Vec(depth, UInt(VPUConstants.FFLAGS_SZ.W)))
      for(i <- 0 until depth)
      {
        val less = cmp(i).lt
        val equal = cmp(i).eq
        val greater = cmp(i).gt
        val inValid = (fn.op_is(CmpFun_EQ, CmpFun_NE) && hasSNaN(i)) || 
                      (fn.op_is(CmpFun_Min, CmpFun_Max) && hasSNaN(i)) || 
                      (fn.op_is(CmpFun_LT, CmpFun_GT, CmpFun_GE, CmpFun_LE) && hasNaN(i))
        val sel = List(CmpFun_EQ, CmpFun_NE, CmpFun_LT, CmpFun_GT, CmpFun_GE, CmpFun_LE).map(fn.op_is(_))
        val in = List(
              (equal && !hasNaN(i)),             // CmpFun_EQ
             !(equal && !hasNaN(i)),             // CmpFun_NE
              (less  && !hasNaN(i)),             // CmpFun_LT
            (greater && !hasNaN(i)),             // CmpFun_GT
          ((greater || equal) && !hasNaN(i)),    // CmpFun_GE
             ((less || equal) && !hasNaN(i))     // CmpFun_LE
            )

        flags(i) := Mux1H(sel, in)    
        exc(i) := Mux(fn.op_is(CmpFun_Min, CmpFun_Max), inValid << 4, cmp(i).exceptionFlags)
      }
      (flags, exc)
    }
  }

  val mux = Wire(new VFCmpResult)

  (F32, F16, F64, F128) match {
    case (true, false, false, false) => { // F32
       mux.vFMinMaxCmpOut.f32 := minmax_results(0)._1
       mux.vFCmpOut.f32  := cmp_results(0)._1
       mux.exc.exc32 := cmp_results(0)._2
    }
    case (true, true, false, false) => { // F32, F16
       mux.vFMinMaxCmpOut.f32 := minmax_results(0)._1
       mux.vFMinMaxCmpOut.f16 := minmax_results(1)._1
       mux.vFCmpOut.f16  := cmp_results(0)._1
       mux.vFCmpOut.f32  := cmp_results(1)._1
       mux.exc.exc32 := cmp_results(0)._2
       mux.exc.exc16 := cmp_results(1)._2
    }
    case (true, false, true, false) => { // F32, F64
       mux.vFMinMaxCmpOut.f32 := minmax_results(0)._1
       mux.vFMinMaxCmpOut.f64 := minmax_results(1)._1
       mux.vFCmpOut.f32  := cmp_results(0)._1
       mux.vFCmpOut.f64  := cmp_results(1)._1
       mux.exc.exc32 := cmp_results(0)._2
       mux.exc.exc64 := cmp_results(1)._2
    }
    case (true, true, true, false) => { // F32, F16, F64
       mux.vFMinMaxCmpOut.f32 := minmax_results(0)._1
       mux.vFMinMaxCmpOut.f16 := minmax_results(1)._1
       mux.vFMinMaxCmpOut.f64 := minmax_results(2)._1
       mux.vFCmpOut.f32  := cmp_results(0)._1
       mux.vFCmpOut.f16  := cmp_results(1)._1
       mux.vFCmpOut.f64  := cmp_results(2)._1
       mux.exc.exc32 := cmp_results(0)._2
       mux.exc.exc16 := cmp_results(1)._2
       mux.exc.exc64 := cmp_results(2)._2
    }
    case (true, false, true, true) => { // F32, F64, F128
       mux.vFMinMaxCmpOut.f32 := minmax_results(0)._1
       mux.vFMinMaxCmpOut.f64 := minmax_results(1)._1
       mux.vFMinMaxCmpOut.f128 := minmax_results(2)._1
       mux.vFCmpOut.f32  := cmp_results(0)._1
       mux.vFCmpOut.f64  := cmp_results(1)._1
       mux.vFCmpOut.f128 := cmp_results(2)._1
       mux.exc.exc32 := cmp_results(0)._2
       mux.exc.exc64 := cmp_results(1)._2
       mux.exc.exc128 := cmp_results(2)._2
    }
    case (true, true, true, true) => { // F32, F16, F64, F128
       mux.vFMinMaxCmpOut.f32 := minmax_results(0)._1
       mux.vFMinMaxCmpOut.f16 := minmax_results(1)._1
       mux.vFMinMaxCmpOut.f64 := minmax_results(2)._1
       mux.vFMinMaxCmpOut.f128 := minmax_results(3)._1
       mux.vFCmpOut.f32  := cmp_results(0)._1
       mux.vFCmpOut.f16  := cmp_results(1)._1
       mux.vFCmpOut.f64  := cmp_results(2)._1
       mux.vFCmpOut.f128 := cmp_results(3)._1
       mux.exc.exc32 := cmp_results(0)._2
       mux.exc.exc16 := cmp_results(1)._2
       mux.exc.exc64 := cmp_results(2)._2
       mux.exc.exc128 := cmp_results(3)._2
    }
    case _ => {
      // do nothing
    }
  }

  io.resp <> Pipe(io.req.valid, mux, stagesFCmp)
}


