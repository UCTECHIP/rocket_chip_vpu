// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFWidthConvert.scala
*       Author          :       liangzh
*       Revision        :       2020/01/09
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       convert floating-point vector elements' width
*
*       io.reqValid     :       input, control, request valid
*       io.isSrc12SEW   :       input, control, showing whether source 1 is double SEW width or not
*       io.isSrc22SEW   :       input, control, showing whether source 2 is double SEW width or not
*       io.isVd2SEW     :       input, control, showing whether destination is double SEW width or not
*       io.src1Typ      :       ipnut[Src1Typ_SZ-1:0], control, showing the type of source 1 is which type: vector, scalar or immediate
*       io.majFun       :       input[MajFun_SZ-1:0], control, showing which major function the inst is
*       io.fcvtFun      :       input[FCvtFun_SZ-1:0], control, showing function of a floating-point convert inst
*       io.frm          :       input[FRM_SZ-1:0], control, floating-point rounding mode
*       io.fromXData1   :       input[XLEN-1:0], data, from rocket GPR data
*       io.fromFData    :       input[FLEN:0], data, from FPU floating-point registers data
*       io.vs1e         :       input, bundle of vectors, data, SEW relative, to be width convert source 1
*       io.vs2e         :       input, bundle of vectors, data, SEW relative, to be width convert source 2
*       io.vdvs3e       :       input, bundle of vectors, data, SEW relative, to be recode to UCB pattern destination
*       io.respValid    :       output, control, showing output data valid
*       io.vsrc1f       :       output, bundle of vectors, data, UCB relative, width converted source 1
*       io.vsrc2f       :       output, bundle of vectors, data, UCB relative, width converted source 2
*       io.vdvs3f       :       output, bundle of vectors, data, UCB relative, UCB recoded destination
*       io.vFtoFExc     :       output, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from width convert process
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VFWidthConvert(params: VPUParams) extends VModule(params) with HasFPUParameters {
  val io = IO(new Bundle {
    val reqValid   = Input(Bool())
    val isSrc12SEW = Input(Bool())
    val isSrc22SEW = Input(Bool())
    val isVd2SEW   = Input(Bool())
    val src1Typ    = Input(UInt(Src1Typ_SZ.W))
    val majFun     = Input(UInt(MajFun_SZ.W))
    val fcvtFun    = Input(UInt(FCvtFun_SZ.W))
    val frm        = Input(UInt(VPUConstants.FRM_SZ.W))

    val fromXData1 = Input(UInt(XLEN.W))
    val fromFData  = Input(UInt((FLEN+1).W))
    val vs1e       = Input(new SEWVec)
    val vs2e       = Input(new SEWVec)
    val vdvs3e     = Input(new SEWVec)

    val respValid  = Output(new Bool())
    val vsrc1f     = Output(new FUCBVec)
    val vsrc2f     = Output(new FUCBVec)
    val vdvs3f     = Output(new FUCBVec)
    val vFtoFExc   = Output(new FFLAGSVec)
  })

  val isV1Origin  = io.src1Typ === Src1_Vs && (!io.isVd2SEW || io.isSrc12SEW)
  val isV1to2SEW  = io.src1Typ === Src1_Vs && io.isVd2SEW
  val isF1toSEW   = io.src1Typ === Src1_Fs && !io.isVd2SEW
  val isF1to2SEW  = io.src1Typ === Src1_Fs && io.isVd2SEW

  val isV2Origin  = io.majFun === IsFCvt && (io.fcvtFun =/= FCvtFun_F2WF && io.fcvtFun =/= FCvtFun_F2NF) || 
                    io.majFun =/= IsFCvt && (io.isSrc22SEW || !io.isVd2SEW)
  val isV2to2SEW  = io.majFun === IsFCvt && io.fcvtFun === FCvtFun_F2WF || 
                    io.majFun =/= IsFCvt && !io.isSrc22SEW && io.isVd2SEW
  val isV2toHSEW  = io.majFun === IsFCvt && io.fcvtFun === FCvtFun_F2NF

  val vsrc1f    = RegInit(0.U.asTypeOf(new FUCBVec))
  val vsrc2f    = RegInit(0.U.asTypeOf(new FUCBVec))
  val vFtoFExc  = RegInit(0.U.asTypeOf(new FFLAGSVec))
  val respValid = RegNext(io.reqValid, false.B)

  val qNaN16  = Cat(~(0.U(7.W)), 0.U(9.W))
  val qNaN32  = Cat(~(0.U(10.W)), 0.U(22.W))
  val qNaN64  = Cat(~(0.U(13.W)), 0.U(51.W))
  val qNaN128 = Cat(~(0.U(17.W)), 0.U(111.W))
  val ucb33   = unbox(io.fromFData, false.B, Some(FType.S))
  val ucb65   = unbox(io.fromFData, false.B, Some(FType.D))

  def F33ToH1: UInt = unbox(io.fromFData, false.B, Some(FType.H))
  def F65ToH1: UInt = unbox(io.fromFData, false.B, Some(FType.H))
  def F32ToH1: UInt = FType.H.recode(Mux(io.fromXData1(31,16).andR, io.fromXData1(15,0), qNaN16))
  def F64ToH1: UInt = FType.H.recode(Mux(io.fromXData1(63,16).andR, io.fromXData1(15,0), qNaN16))
  def FH1: UInt = if(!ZFINX && FLEN == 32)      F33ToH1
                  else if(!ZFINX && FLEN == 64) F65ToH1
                  else if(ZFINX && XLEN == 32)  F32ToH1
                  else                          F64ToH1

  def F33ToS1: UInt = ucb33
  def F65ToS1: UInt = ucb33
  def F32ToS1: UInt = FType.S.recode(io.fromXData1)
  def F64ToS1: UInt = FType.S.recode(Mux(io.fromXData1(63,32).andR, io.fromXData1(31,0), qNaN32))
  def FS1: UInt = if(!ZFINX && FLEN == 32)      F33ToS1
                  else if(!ZFINX && FLEN == 64) F65ToS1
                  else if(ZFINX && XLEN == 32)  F32ToS1
                  else                          F64ToS1

  def F33ToD1: UInt = box(ucb33, FType.D)
  def F65ToD1: UInt = ucb65
  def F32ToD1: UInt = FType.D.recode(Cat(~(0.U(32.W)), io.fromXData1))
  def F64ToD1: UInt = FType.D.recode(io.fromXData1)
  def FD1: UInt = if(!ZFINX && FLEN == 32)      F33ToD1
                  else if(!ZFINX && FLEN == 64) F65ToD1
                  else if(ZFINX && XLEN == 32)  F32ToD1
                  else                          F64ToD1

  def F33ToQ1: UInt = box(ucb33, FType.Q)
  def F65ToQ1: UInt = box(ucb65, FType.Q)
  def F32ToQ1: UInt = FType.Q.recode(Cat(~(0.U(96.W)), io.fromXData1))
  def F64ToQ1: UInt = FType.Q.recode(Cat(~(0.U(64.W)), io.fromXData1))
  def FQ1: UInt = if(!ZFINX && FLEN == 32)      F33ToQ1
                  else if(!ZFINX && FLEN == 64) F65ToQ1
                  else if(ZFINX && XLEN == 32)  F32ToQ1
                  else                          F64ToQ1

  def HToS(ucbH: UInt): UInt = {
    val htos = Module(new hardfloat.RecFNToRecFN(FType.H.exp, FType.H.sig, FType.S.exp, FType.S.sig))
    htos.io.in := ucbH
    htos.io.roundingMode := io.frm
    htos.io.detectTininess := hardfloat.consts.tininess_afterRounding
    htos.io.out
  }
  def SToD(ucbS: UInt): UInt = {
    val stod = Module(new hardfloat.RecFNToRecFN(FType.S.exp, FType.S.sig, FType.D.exp, FType.D.sig))
    stod.io.in := ucbS
    stod.io.roundingMode := io.frm
    stod.io.detectTininess := hardfloat.consts.tininess_afterRounding
    stod.io.out
  }
  def DToQ(ucbD: UInt): UInt = {
    val dtoq = Module(new hardfloat.RecFNToRecFN(FType.D.exp, FType.D.sig, FType.Q.exp, FType.Q.sig))
    dtoq.io.in := ucbD
    dtoq.io.roundingMode := io.frm
    dtoq.io.detectTininess := hardfloat.consts.tininess_afterRounding
    dtoq.io.out
  }
  def SToH(ucbS: UInt): UInt = {
    val stoh = Module(new hardfloat.RecFNToRecFN(FType.S.exp, FType.S.sig, FType.H.exp, FType.H.sig))
    stoh.io.in := ucbS
    stoh.io.roundingMode := io.frm
    stoh.io.detectTininess := hardfloat.consts.tininess_afterRounding
    Cat(stoh.io.out, stoh.io.exceptionFlags)
  }
  def DToS(ucbD: UInt): UInt = {
    val dtos = Module(new hardfloat.RecFNToRecFN(FType.D.exp, FType.D.sig, FType.S.exp, FType.S.sig))
    dtos.io.in := ucbD
    dtos.io.roundingMode := io.frm
    dtos.io.detectTininess := hardfloat.consts.tininess_afterRounding
    Cat(dtos.io.out, dtos.io.exceptionFlags)
  }
  def QToD(ucbQ: UInt): UInt = {
    val qtod = Module(new hardfloat.RecFNToRecFN(FType.Q.exp, FType.Q.sig, FType.D.exp, FType.D.sig))
    qtod.io.in := ucbQ
    qtod.io.roundingMode := io.frm
    qtod.io.detectTininess := hardfloat.consts.tininess_afterRounding
    Cat(qtod.io.out, qtod.io.exceptionFlags)
  }
/////*convert vs1's width SEW to 2SEW and fs1's width to SEW for floating-point operation*////
  for(i <- 0 until f16Depth)
    vsrc1f.f16(i) := MuxCase(FType.H.qNaN, Array(isV1Origin -> FType.H.recode(io.vs1e.e16(i)),
                                                 isF1toSEW  -> FH1))

  for(i <- 0 until f32Depth)
    vsrc1f.f32(i) := MuxCase(FType.S.qNaN,
                       Array(isV1Origin -> FType.S.recode(io.vs1e.e32(i)),
                             isF1toSEW  -> FS1)
                     ++ (if(FSEW16) Array(isV1to2SEW -> HToS(FType.H.recode(io.vs1e.e16(i))),
                                          isF1to2SEW -> HToS(FH1))
                         else Nil))

  for(i <- 0 until f64Depth)
    vsrc1f.f64(i) := MuxCase(FType.D.qNaN,
                       Array(isV1Origin -> FType.D.recode(io.vs1e.e64(i)),
                             isF1toSEW  -> FD1,
                             isV1to2SEW -> SToD(FType.S.recode(io.vs1e.e32(i))),
                             isF1to2SEW -> SToD(FS1)))

  for(i <- 0 until f128Depth)
    vsrc1f.f128(i) := MuxCase(FType.Q.qNaN,
                        Array(isV1Origin -> FType.Q.recode(io.vs1e.e128(i)),
                              isF1toSEW  -> FQ1,
                              isV1to2SEW -> DToQ(FType.D.recode(io.vs1e.e64(i))),
                              isF1to2SEW -> DToQ(FD1)))

////////*convert vs2's width SEW to 2SEW or 2SEW to SEW for floating-point operation*/////////
//half precision
  for(i <- 0 until f16Depth/2) {
    val fexc17 = MuxCase(Cat(FType.H.qNaN, 0.U(5.W)),
                   Array(isV2Origin -> Cat(FType.H.recode(io.vs2e.e16(i)), 0.U(5.W)),
                         isV2toHSEW -> SToH(FType.S.recode(io.vs2e.e32(i)))))
    vsrc2f.f16(i)     := fexc17(21,5)
    vFtoFExc.exc16(i) := fexc17(4,0)
  }
  for(i <- f16Depth/2 until f16Depth) {
    vsrc2f.f16(i)     := FType.H.recode(io.vs2e.e16(i))
    vFtoFExc.exc16(i) := 0.U(5.W)
  }
//single precision
  for(i <- 0 until f32Depth/2) {
    val fexc33 = MuxCase(Cat(FType.S.qNaN, 0.U(5.W)), 
                   Array(isV2Origin -> Cat(FType.S.recode(io.vs2e.e32(i)), 0.U(5.W)))
                 ++ (if(FSEW16)        Array(isV2to2SEW -> Cat(HToS(FType.H.recode(io.vs2e.e16(i))), 0.U(5.W))) else Nil)
                 ++ (if(FSEWMAX >= 64) Array(isV2toHSEW -> DToS(FType.D.recode(io.vs2e.e64(i))))                else Nil))
    vsrc2f.f32(i)     := fexc33(37,5)
    vFtoFExc.exc32(i) := fexc33(4,0)
  }
  for(i <- f32Depth/2 until f32Depth) {
    vsrc2f.f32(i) := MuxCase(FType.S.qNaN,
                       Array(isV2Origin -> FType.S.recode(io.vs2e.e32(i)))
                     ++ (if(FSEW16)  Array(isV2to2SEW -> HToS(FType.H.recode(io.vs2e.e16(i)))) else Nil))
    vFtoFExc.exc32(i) := 0.U(5.W)
  }
//double precision
  for(i <- 0 until f64Depth/2) {
    val fexc65 = MuxCase(Cat(FType.D.qNaN, 0.U(5.W)),
                   Array(isV2Origin -> Cat(FType.D.recode(io.vs2e.e64(i)), 0.U(5.W)),
                         isV2to2SEW -> Cat(SToD(FType.S.recode(io.vs2e.e32(i))), 0.U(5.W)))
                 ++ (if(FSEWMAX == 128) Array(isV2toHSEW -> QToD(FType.Q.recode(io.vs2e.e128(i)))) else Nil))
    vsrc2f.f64(i)     := fexc65(68,5)
    vFtoFExc.exc64(i) := fexc65(4,0)
  }
  for(i <- f64Depth/2 until f64Depth) {
    vsrc2f.f64(i) := MuxCase(FType.D.qNaN,
                       Array(isV2Origin -> FType.D.recode(io.vs2e.e64(i)),
                             isV2to2SEW -> SToD(FType.S.recode(io.vs2e.e32(i)))))
    vFtoFExc.exc64(i) := 0.U(5.W)
  }
//quadruple precision
  for(i <- 0 until f128Depth) {
    vsrc2f.f128(i) := MuxCase(FType.Q.qNaN,
                        Array(isV2Origin -> FType.Q.recode(io.vs2e.e128(i)),
                              isV2to2SEW -> DToQ(FType.D.recode(io.vs2e.e64(i)))))
    vFtoFExc.exc128(i) := 0.U(5.W)
  }


  io.vsrc1f    := vsrc1f
  io.vsrc2f    := vsrc2f
  io.vFtoFExc  := vFtoFExc
  io.respValid := respValid
  for(i <- 0 until f16Depth)  io.vdvs3f.f16(i)  := FType.H.recode(io.vdvs3e.e16(i))
  for(i <- 0 until f32Depth)  io.vdvs3f.f32(i)  := FType.S.recode(io.vdvs3e.e32(i))
  for(i <- 0 until f64Depth)  io.vdvs3f.f64(i)  := FType.D.recode(io.vdvs3e.e64(i))
  for(i <- 0 until f128Depth) io.vdvs3f.f128(i) := FType.Q.recode(io.vdvs3e.e128(i))

  
}
