// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VDecodeExcp.scala
*       Author          :       sujy, liangzh
*       Revision        :       2019/06/25
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       Basic exception judgement module
                                after decode stage in pipeline
*
*       io.coreFire     :       input, control, showing receive a valid vector inst
*       io.vill         :       input, control, show CSR values are illegal
*       io.vediv        :       input[VEDIV_SZ-1:0], data, vediv field of vtype CSR
*       io.vsew         :       input[VSEW_SZ-1:0], data, vsew field of vtype CSR
*       io.vlmul        :       input[VLMUL_SZ-1:0], data, vlmul field of vtype CSR
*       io.vstart       :       input[log2Ceil(VLEN)-1:0], data, vstart CSR value
*       io.src1Field    :       input[4:0], data, source 1 field of an inst
*       io.src2Field    :       input[4:0], data, source 2 field of an inst
*       io.dstField     :       input[4:0], data, dstination field of an inst
*       io.isALInst     :       input, control, show whether the inst is a AL inst or not
*       io.isLdInst     :       input, control, show whether the inst is a load inst or not
*       io.isStInst     :       input, control, show whether the inst is a store inst or not
*       io.isAMOInst    :       input, control, show whether the inst is a AMO inst or not
*       io.isSrc12SEW   :       output, control, show whether source 1 is require double SEW width elements
*       io.isSrc22SEW   :       output, control, show whether source 2 is require double SEW width elements
*       io.idVd2SEW     :       output, control, show whether destination is require double SEW width elements, or quadruple width elements in multiply
*       io.isSEWOut     :       output, control, show an instruction's results are SEW related or not
*       io.isMLENOut    :       output, control, show an instruction's results are MLEN related or not
*       io.src1Typ      :       output[Src1Typ_SZ-1:0], control, show the type of source 1 is vector, scalar or immediate
*       io.majFun       :       output[MajFun_SZ-1:0], control, show which major function the inst is
*       io.mulFun       :       output[MulFun-SZ-1:0], control, show which multiply function the inst is
*       io.slideFun     :       output[SlideFun_SZ-1:0], control, show which slide function the inst is
*       io.nFields      :       output[NFIELDS_SZ-1:0], control, nf fields of a inst
*       io.addrMode     :       output[AddrMode_SZ-1:0], control, show addressing mode in load&store
*       io.ldstWidth    :       output[LdStWidth_SZ-1:0], control, show which width of load&store data is: byte, halfword, word or SEW
*       io.vm           :       output, control, vm field field of an inst, show whether results are masked or unmasked
*       io.isFullMul    :       output, control, show whether the multiply product is double SEW width output or not
*       io.excpFlag     :       output, control, show whether an inst is illegal or not
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VDecodeExcp(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle{
    val coreFire   = Input(Bool())
    //CSR settings
    val vill       = Input(UInt(1.W))
    val vediv      = Input(UInt(VEDIV_SZ.W))
    val vsew       = Input(UInt(VSEW_SZ.W))
    val vlmul      = Input(UInt(VLMUL_SZ.W))
    val vstart     = Input(UInt(log2Ceil(VLEN).W))
    //inst fields
    val src1Field  = Input(UInt(5.W))
    val src2Field  = Input(UInt(5.W))
    val dstField   = Input(UInt(5.W))
    //decode input
    val isALInst   = Input(Bool())
    val isLdInst   = Input(Bool())
    val isStInst   = Input(Bool())
    val isAMOInst  = Input(Bool())
    val isSrc12SEW = Input(Bool())
    val isSrc22SEW = Input(Bool())
    val isVd2SEW   = Input(Bool())
    val isSEWOut   = Input(Bool())
    val isMLENOut  = Input(Bool())
    val src1Typ    = Input(UInt(Src1Typ_SZ.W))
    val majFun     = Input(UInt(MajFun_SZ.W))
    val mulFun     = Input(UInt(MulFun_SZ.W))
    val slideFun   = Input(UInt(SlideFun_SZ.W))
    val nFields    = Input(UInt(NFIELDS_SZ.W))
    val addrMode   = Input(UInt(AddrMode_SZ.W))
    val ldstWidth  = Input(UInt(LdStWidth_SZ.W))
    val vm         = Input(UInt(1.W))
    val isFullMul  = Input(Bool())

    val excpFlag   = Output(Bool())
  })

  //if exception is raised, flag is asserted, otherwise flag is deasserted
  class excpFlag extends Bundle {
    val wrongCSR      = Bool()
    val wrongRegNum   = Bool()
    val wrongVstart   = Bool()
    val non0Vstart    = Bool()
    val exceedLMUL    = Bool()
    val exceedELEN    = Bool()
    val ltMemSize     = Bool()
    val segExceed8    = Bool()
    val segExceed31   = Bool()
//    val wholeExceed31 = Bool() //for future whole load store
    val amoExceed     = Bool()
    val overlapV0     = Bool()
    val overlapVs1    = Bool()
    val overlapVs2    = Bool()
    val noFPWidth     = Bool()
  }

  def IsOverlap (src:UInt, dst:UInt, srcvlmul:UInt, dstvlmul:UInt): Bool = {
    val srcUpperEdge = src + (1.U(5.W) << srcvlmul) - 1.U
    val dstUpperEdge = dst + (1.U(5.W) << dstvlmul) - 1.U

//    overlapFlag := (dst <= src) && (dstLowerEdge > src && dstLowerEdge <= srcLowerEdge) ||    //lower overlap
    val overlapFlag = (dst <= src) && (dstUpperEdge >= src) ||    //lower overlap
                      (dst >= src) && (dst <= srcUpperEdge)       //upper overlap
    overlapFlag
  }

  val vlmulMAX     = 3.U
  val vsewMAX      = (log2Ceil(ELEN)-3).U

  val isFPInst     = IsFMA <= io.majFun && io.majFun <= IsFDot

  val isSEWVs1     = io.src1Typ === Src1_Vs && !(io.majFun >= IsCSR && io.majFun <= IsMBit    || 
                                                 io.majFun >= IsRed && io.majFun <= IsSlide1  || 
                                                 io.majFun >= IsZip && io.majFun <= IsCopy    || 
                                                 io.majFun === IsFCvt || io.majFun === IsFRed || 
                                                 io.majFun >= IsFClass && io.majFun <= IsFMv || 
                                                 io.majFun === IsAMO)
  val isSEWVs2     = io.majFun <= IsMerge || io.majFun >= IsFDiv || 
                     (io.majFun === IsLoad || io.majFun === IsStore) && io.addrMode === Indexed || 
                     io.majFun >= IsMulDiv && io.majFun <= IsRed || 
                     io.majFun >= IsSlide && io.majFun <= IsFMerge
  val isSEWVdVs3   = io.isSEWOut || io.majFun === IsStore || io.majFun === IsAMO

  val src1vlmul    = io.vlmul +& io.isSrc12SEW.asUInt
  val src2vlmul    = io.vlmul +& io.isSrc22SEW.asUInt
  val dstvlmul     = io.vlmul +& io.isVd2SEW.asUInt +& io.isFullMul.asUInt

  val src1TooMuch  = src1vlmul > vlmulMAX && isSEWVs1
  val src2TooMuch  = src2vlmul > vlmulMAX && isSEWVs2
  val dstTooMuch   = dstvlmul > vlmulMAX && isSEWVdVs3

  val src1vsew     = io.vsew +& io.isSrc12SEW.asUInt
  val src2vsew     = io.vsew +& io.isSrc22SEW.asUInt
  val dstvsew      = io.vsew +& io.isVd2SEW.asUInt +& io.isFullMul.asUInt

  val src1TooWide  = src1vsew > vsewMAX && isSEWVs1
  val src2TooWide  = src2vsew > vsewMAX && isSEWVs2
  val dstTooWide   = dstvsew > vsewMAX && isSEWVdVs3

  /* a % b <-> a & (b - 1), only when isPow2(b) === true.B  */
  val src1Illegal  = ((io.src1Field & ((1.U(5.W) << src1vlmul(1,0)) - 1.U)) =/= 0.U) && isSEWVs1
  val src2Illegal  = ((io.src2Field & ((1.U(5.W) << src2vlmul(1,0)) - 1.U)) =/= 0.U) && isSEWVs2
  val dstIllegal   = ((io.dstField & ((1.U(5.W) << dstvlmul(1,0)) - 1.U)) =/= 0.U) && isSEWVdVs3

  val src1OverlapSEW  = IsOverlap(io.src1Field, io.dstField, src1vlmul(1,0), dstvlmul(1,0))
  val src2OverlapSEW  = IsOverlap(io.src2Field, io.dstField, src2vlmul(1,0), dstvlmul(1,0))
  val src1OverlapMLEN = IsOverlap(io.src1Field, io.dstField, src1vlmul(1,0), 0.U)
  val src2OverlapMLEN = IsOverlap(io.src2Field, io.dstField, src2vlmul(1,0), 0.U)
  val src1OverlapZip  = IsOverlap(io.src1Field, io.dstField, 0.U, dstvlmul(1,0))
  val src2OverlapIota = IsOverlap(io.src2Field, io.dstField, 0.U, dstvlmul(1,0))
  val isSlideUp       = (io.majFun === IsSlide || io.majFun === IsSlide1) && io.slideFun === SlideFun_Up

  val excp = Wire(new excpFlag)


  excp.wrongCSR      := io.vill.asBool

  excp.wrongRegNum   := src1Illegal || src2Illegal || dstIllegal

  excp.wrongVstart   := false.B //TODO find some impossible vstart value

  excp.non0Vstart    := io.vstart =/= 0.U && (io.majFun === IsRed || io.majFun === IsFRed || io.majFun === IsZip || 
                                              io.majFun >= IsPopc && io.majFun <= IsIota)

  excp.exceedLMUL    := src1TooMuch || src2TooMuch || dstTooMuch

  excp.exceedELEN    := src1TooWide || src2TooWide || dstTooWide
 
  excp.ltMemSize     := Mux((io.majFun === IsLoad || io.majFun === IsStore) && io.ldstWidth < "b11".U, io.ldstWidth > io.vsew, false.B)

  excp.segExceed8    := (io.majFun === IsLoad || io.majFun === IsStore) && ((io.nFields +& 1.U(3.W)) << io.vlmul) > 8.U

  excp.segExceed31   := (io.majFun === IsLoad || io.majFun === IsStore) && (io.dstField + ((io.nFields +& 1.U(3.W)) << io.vlmul)) > 32.U

  excp.amoExceed     := io.majFun === IsAMO && !((2.U <= io.vsew) && (io.vsew <= (log2Ceil(XLEN)-3).U))
  
  excp.overlapV0     := io.vm === 0.U && io.dstField === 0.U && 
                        (io.vlmul > 0.U && io.isSEWOut ||
                         io.majFun === IsLoad && io.nFields > 0.U && io.addrMode === Indexed || 
                         io.majFun === IsIota || isSlideUp || 
                         (io.isVd2SEW.asUInt +& io.isFullMul.asUInt) > 0.U && io.isSEWOut)

  excp.overlapVs1    := io.src1Typ === Src1_Vs && 
                        (io.isSEWOut && (io.isVd2SEW.asUInt +& io.isFullMul.asUInt) > 0.U && src1OverlapSEW || 
                         io.isMLENOut && (io.majFun =/= IsMIdx && io.majFun =/= IsMBit) && io.vlmul > 0.U && src1OverlapMLEN || 
                         io.majFun === IsZip && src1OverlapZip) 

  excp.overlapVs2    := io.isSEWOut && !io.isSrc22SEW && (io.isVd2SEW.asUInt +& io.isFullMul.asUInt) > 0.U && src2OverlapSEW || 
                        (io.isSrc22SEW && !io.isVd2SEW || io.majFun === IsZip || isSlideUp) && src2OverlapSEW || 
                        io.isMLENOut && (io.majFun =/= IsMIdx && io.majFun =/= IsMBit) && io.vlmul > 0.U && src2OverlapMLEN || 
                        io.majFun === IsIota && src2OverlapIota

  excp.noFPWidth     := (if(!FSEW16) isFPInst && ((io.vsew - io.vediv) > (log2Ceil(FSEWMAX)-3).U || (io.vsew - io.vediv) <= 1.U)
                         else        isFPInst && ((io.vsew - io.vediv) > (log2Ceil(FSEWMAX)-3).U || (io.vsew - io.vediv) < 1.U))

  val isVInst = io.isALInst || io.isLdInst || io.isStInst || io.isAMOInst

  io.excpFlag := excp.asUInt =/= 0.U && isVInst

}
