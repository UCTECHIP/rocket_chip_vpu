// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFunSelFront.scala
*       Author          :       liangzh
*       Revision        :       2020/04/05
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       select function modules output
*
*       io.isSEWOut     :       input, control, showing whether results are SEW relative or not
*       io.isMLENOut    :       input, control, showing whether results are MLEN relative or not
*       io.isVd2SEW     :       input, control, showing whether results are double SEW width or not
*       io.majFun       :       input[MajFun_SZ-1:0], control, select module results
*       io.fcvtFun      :       input[FCvtFun_SZ-1:0], control, select floating-point convert type results
*       io.isFullMul    :       input, control, showing whether multiply results are double SEW width
*       io.isFMAReq     :       input, control, showing whether FMA module output or not
*       io.isFCmpReq    :       input, control, showing whether FCompare module output or not
*       io.vsew         :       input[VSEW_SZ-1:0], control, select SEW width results
*       io.vAddCarry    :       input, bundle of vectors, data, SEW relative, results from VFullAdder module
*       io.vAddSum      :       input, bundle of vectors, data, SEW relative, results from VFullAdder module
*       io.vBitwise     :       input, bundle of vectors, data, SEW relative, results from VBitwise module
*       io.vShiftOut    :       input, bundle of vectors, data, SEW relative, results from VShift module
*       io.vCmpOut      :       input, bundle of vectors, data, SEW relative, results from VCompare module
*       io.vMinMaxOut   :       input, bundle of vectors, data, SEW relative, results from VCompare module
*       io.vMergeOut    :       input, bundle of vectors, data, SEW relative, results from VMerge module
*       io.vMulDivOut   :       input, bundle of vectors, data, SEW relative, results from VMulDiv module
*       io.vRedOut      :       input, bundle of vectors, data, SEW relative, results from VReduction module
*       io.vRGatherOut  :       input, bundle of vectors, data, SEW relative, results from VRGather module
*       io.vFClassOut   :       input, bundle of vectors, data, SEW relative, results from VFClass module
*       io.vFtoIOut     :       input, bundle of vectors, data, SEW relative, results from VFIConvert module
*       io.vItoFOut     :       input, bundle of vectors, data, UCB relative, results from VIFConvert module
*       io.vsrc2f       :       input, bundle of vectors, data, UCB relative, results from VFWidthConvert module
*       io.vFMAOut      :       input, bundle of vectors, data, UCB relative, results from VFMA module
*       io.vFDivOut     :       input, bundle of vectors, data, UCB relative, results from VFDivSqrt module
*       io.vFCmpOut     :       input, bundle of vectors, data, SEW relative, results from VFCompare module
*       io.vFMinMaxOut  :       input, bundle of vectors, data, UCB relative, results from VFCompare module
*       io.vFMergeOut   :       input, bundle of vectors, data, UCB relative, results from VMerge module
*       io.vFSgnJOut    :       input, bundle of vectors, data, UCB relative, results from VFSignInject module
*       io.v0fen        :       input, bundle of vectors, data, elements all 1 bit, to mask fflags
*       io.vFMAExc      :       input, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from VFMA module
*       io.vFDivExc     :       input, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from VFDivSqrt module
*       io.vFCmpExc     :       input, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from VFCompare module
*       io.vItoFExc     :       input, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from VIFConvert module
*       io.vFtoIExc     :       input, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from VFIConvert module
*       io.vFtoFExc     :       input, bundle of vectors, data, elements all FFLAGS_SZ bits, fflags from VFWidthConvert module
*       io.vSEWData     :       output[LMULMAX*ELEN-1:0], data, selected, packaged data
*       io.fflags       :       output[FFLAGS_SZ-1:0], data, seleted, OR-together fflag
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VFunSelFront(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val isSEWOut     = Input(Bool())
    val isMLENOut    = Input(Bool())
    val isVd2SEW     = Input(Bool())
    val majFun       = Input(UInt(MajFun_SZ.W))
    val fcvtFun      = Input(UInt(FCvtFun_SZ.W))
    val isFullMul    = Input(Bool())
    val isFMAReq     = Input(Bool())
    val isFCmpReq    = Input(Bool())
    val vsew         = Input(UInt(VSEW_SZ.W))

    val vAddCarry    = Input(new SEWVec)
    val vAddSum      = Input(new SEWVec)
    val vBitwise     = Input(new SEWVec)
    val vShiftOut    = Input(new SEWVec)
    val vCmpOut      = Input(new SEWVec)
    val vMinMaxOut   = Input(new SEWVec)
    val vMergeOut    = Input(new SEWVec)
    val vMulDivOut   = Input(new SEWVec)
    val vRedOut      = Input(new SEWVec)
    val vRGatherOut  = Input(new SEWVec)
    val vFClassOut   = Input(new SEWVec)
    val vFtoIOut     = Input(new SEWVec)

    val vItoFOut     = Input(new FUCBVec)
    val vsrc2f       = Input(new FUCBVec)
    val vFMAOut      = Input(new FUCBVec)
    val vFDivOut     = Input(new FUCBVec)
    val vFCmpOut     = Input(new FSEWVec)
    val vFMinMaxOut  = Input(new FUCBVec)
    val vFMergeOut   = Input(new FUCBVec)
    val vFSgnJOut    = Input(new FUCBVec)

    val v0fen        = Input(new FSEW1wVec)

    val vFMAExc      = Input(new FFLAGSVec)
    val vFDivExc     = Input(new FFLAGSVec)
    val vFCmpExc     = Input(new FFLAGSVec)
    val vItoFExc     = Input(new FFLAGSVec)
    val vFtoIExc     = Input(new FFLAGSVec)
    val vFtoFExc     = Input(new FFLAGSVec)

    val vSEWData     = Output(UInt((LMULMAX*ELEN).W))
    val fflags       = Output(UInt(VPUConstants.FFLAGS_SZ.W))
  })

  val MUL = MULDIV || MULADD || SATMUL || QMULADD

  val needAddCarry = io.majFun === IsAdd && io.isMLENOut
  val needAddSum   = (Array(io.majFun === IsAdd && io.isSEWOut)
                      ++ (if(MULADD || QMULADD)  Array(io.majFun === IsMulAdd) else Nil)).reduce(_||_)
  //////////////////
  val needCmpOut   = io.majFun === IsCmp && io.isMLENOut
  val needMinMax   = io.majFun === IsCmp && io.isSEWOut
  //////////////////
  val needFMAOut   = io.isFMAReq
  val needFCmpOut  = io.majFun === IsFCmp && io.isMLENOut
  val needFMinMax  = io.isFCmpReq && io.isSEWOut
  //////////////////
  val isFtoI       = io.majFun === IsFCvt && io.fcvtFun(2,0) <= 1.U(3.W)
  val isItoF       = io.majFun === IsFCvt && (1.U(3.W) < io.fcvtFun(2,0) && io.fcvtFun(2,0) < 4.U(3.W))
  val isFtoF       = io.majFun === IsFCvt && io.fcvtFun(2,0) >= 4.U(3.W)
//select SEW relative bundle of vectors
  val vSEWOut = MuxCase(0.U.asTypeOf(new SEWVec),
                  Array(needAddCarry            -> io.vAddCarry,
                        needAddSum              -> io.vAddSum,
                        (io.majFun === IsBit)   -> io.vBitwise,
                        (io.majFun === IsShift) -> io.vShiftOut,
                        needCmpOut              -> io.vCmpOut,
                        needMinMax              -> io.vMinMaxOut)
                ++ (if(MERGE)  Array((io.majFun === IsMerge)    -> io.vMergeOut)   else Nil)
                ++ (if(MUL)    Array((io.majFun === IsMulDiv)   -> io.vMulDivOut)  else Nil)
                ++ (if(RED)    Array((io.majFun === IsRed)      -> io.vRedOut)     else Nil)
                ++ (if(GATHER) Array((io.majFun === IsGather)   -> io.vRGatherOut) else Nil)
                ++ (if(FCLASS) Array((io.majFun === IsFClass)   -> io.vFClassOut)  else Nil)
                ++ (if(FCVT)   Array(isFtoI                     -> io.vFtoIOut)    else Nil))
//select FSEW relative bundle of vectors
  val vFUCBOut = MuxCase(0.U.asTypeOf(new FUCBVec), Array()
                 ++ (if(FCVT)     Array(isItoF                   -> io.vItoFOut)    else Nil)
                 ++ (if(FCVT)     Array(isFtoF                   -> io.vsrc2f)      else Nil)
                 ++ (if(FMA)      Array(needFMAOut               -> io.vFMAOut)     else Nil)
                 ++ (if(FDIVSQRT) Array((io.majFun === IsFDiv)   -> io.vFDivOut)    else Nil)
                 ++ (if(FCMP)     Array(needFMinMax              -> io.vFMinMaxOut) else Nil)
                 ++ (if(FMERGE)   Array((io.majFun === IsFMerge) -> io.vFMergeOut)  else Nil)
                 ++ (if(FSGNJ)    Array((io.majFun === IsFSgnJ)  -> io.vFSgnJOut)   else Nil))

  val vFSEWOut = Wire(new FSEWVec)
  for(i <- 0 until f16Depth)  vFSEWOut.f16(i)  := (if(FCMP) Mux(needFCmpOut, io.vFCmpOut.f16(i),  FType.H.ieee(vFUCBOut.f16(i)))  else FType.H.ieee(vFUCBOut.f16(i)))
  for(i <- 0 until f32Depth)  vFSEWOut.f32(i)  := (if(FCMP) Mux(needFCmpOut, io.vFCmpOut.f32(i),  FType.S.ieee(vFUCBOut.f32(i)))  else FType.S.ieee(vFUCBOut.f32(i)))
  for(i <- 0 until f64Depth)  vFSEWOut.f64(i)  := (if(FCMP) Mux(needFCmpOut, io.vFCmpOut.f64(i),  FType.D.ieee(vFUCBOut.f64(i)))  else FType.D.ieee(vFUCBOut.f64(i)))
  for(i <- 0 until f128Depth) vFSEWOut.f128(i) := (if(FCMP) Mux(needFCmpOut, io.vFCmpOut.f128(i), FType.Q.ieee(vFUCBOut.f128(i))) else FType.Q.ieee(vFUCBOut.f128(i)))

  val vSEWData     = Wire(UInt((LMULMAX*ELEN).W))
  val vFSEWData    = Wire(UInt((LMULMAX*ELEN).W))
  val vsewSel      = io.vsew + io.isVd2SEW + io.isFullMul
//first select the exact width of a vector
  vSEWData := MuxCase(vSEWOut.e8.asUInt,
               Array((vsewSel === HWordWidth) -> vSEWOut.e16.asUInt,
                     (vsewSel === WordWidth)  -> vSEWOut.e32.asUInt)
              ++ (if(ELEN >= 64)   Array((vsewSel === DWordWidth) -> vSEWOut.e64.asUInt)   else Nil)
              ++ (if(ELEN >= 128)  Array((vsewSel === QWordWidth) -> vSEWOut.e128.asUInt)  else Nil)
              ++ (if(ELEN >= 256)  Array((vsewSel === OWordWidth) -> vSEWOut.e256.asUInt)  else Nil)
              ++ (if(ELEN >= 512)  Array((vsewSel === SWordWidth) -> vSEWOut.e512.asUInt)  else Nil)
              ++ (if(ELEN == 1024) Array((vsewSel === TWordWidth) -> vSEWOut.e1024.asUInt) else Nil))
  vFSEWData := MuxCase(vFSEWOut.f32.asUInt, Array()
               ++ (if(FSEW16)         Array((vsewSel === HWordWidth) -> vFSEWOut.f16.asUInt)  else Nil)
               ++ (if(FSEWMAX >= 64)  Array((vsewSel === DWordWidth) -> vFSEWOut.f64.asUInt)  else Nil)
               ++ (if(FSEWMAX == 128) Array((vsewSel === QWordWidth) -> vFSEWOut.f128.asUInt) else Nil))

//mix floating-point output and fixed-point output
  val isFSEWOut = io.majFun === IsFMA    || 
                  io.majFun === IsFCvt && io.fcvtFun(2,0) > 1.U(3.W) || 
                  io.majFun === IsFCmp   || 
                  io.majFun === IsFSgnJ  || 
                  io.majFun === IsFMerge || 
                  io.majFun === IsFMv    || 
                  io.majFun === IsFDiv   || 
                  io.majFun === IsFRed
  val needFP  = FMA || FCVT || FCMP || FSGNJ || FMERGE || FMV || FDIVSQRT || FRED
  io.vSEWData := (if(needFP) Mux(isFSEWOut, vFSEWData, vSEWData) else vSEWData)


///////////////////////mask and compose FFLAGS relative elements/////////////////

  val vFFLAGSOut    = Wire(new FFLAGSVec)
  val vFFLAGSMasked = Wire(new FFLAGSVec)

  vFFLAGSOut := MuxCase(0.U.asTypeOf(new FFLAGSVec), Array()
                ++ (if(FMA  || FRED) Array(io.isFMAReq            -> io.vFMAExc)  else Nil)
                ++ (if(FDIVSQRT)     Array((io.majFun === IsFDiv) -> io.vFDivExc) else Nil)
                ++ (if(FCMP || FRED) Array(io.isFCmpReq           -> io.vFCmpExc) else Nil)
                ++ (if(FCVT)         Array(isItoF                 -> io.vItoFExc) else Nil)
                ++ (if(FCVT)         Array(isFtoI                 -> io.vFtoIExc) else Nil)
                ++ (if(FCVT)         Array(isFtoF                 -> io.vFtoFExc) else Nil))

//mask elements TODO specical mask for narrowing convert fflags 
  for(i <- 0 until f16Depth)  vFFLAGSMasked.exc16(i)  := Mux((io.v0fen.f16(i)).asBool, vFFLAGSOut.exc16(i), 0.U(5.W))
  for(i <- 0 until f32Depth)  vFFLAGSMasked.exc32(i)  := Mux((io.v0fen.f32(i)).asBool, vFFLAGSOut.exc32(i), 0.U(5.W))
  for(i <- 0 until f64Depth)  vFFLAGSMasked.exc64(i)  := Mux((io.v0fen.f64(i)).asBool, vFFLAGSOut.exc64(i), 0.U(5.W))
  for(i <- 0 until f128Depth) vFFLAGSMasked.exc128(i) := Mux((io.v0fen.f128(i)).asBool, vFFLAGSOut.exc128(i), 0.U(5.W))
//select vector
  io.fflags := MuxCase(0.U(5.W),      Array()
               ++ (if(FSEW16)         Array((vsewSel === HWordWidth) -> vFFLAGSMasked.exc16.reduce(_|_))  else Nil)
               ++                     Array((vsewSel === WordWidth)  -> vFFLAGSMasked.exc32.reduce(_|_))
               ++ (if(FSEWMAX >= 64)  Array((vsewSel === DWordWidth) -> vFFLAGSMasked.exc64.reduce(_|_))  else Nil)
               ++ (if(FSEWMAX == 128) Array((vsewSel === QWordWidth) -> vFFLAGSMasked.exc128.reduce(_|_)) else Nil))

}
