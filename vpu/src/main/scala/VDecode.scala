// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename             :       VDecode.scala
*       Author               :       liangzh
*       Revision             :       2019/04/09
*       Company              :       UC TECH IP
*       Department           :       MAG
*       Description          :       decode RISC-V vector instructions
*
*       io.inst              :       input[31:0], vector instructions, to be decoded
*       io.sigs.isUnmasked   :       output, control, showing whether result elements can be masked or not
*       io.sigs.isSrc12SEW   :       output, control, showing whether source 1 is require double SEW width elements
*       io.sigs.isSrc22SEW   :       output, control, showing whether source 2 is require double SEW width elements
*       io.sigs.idVd2SEW     :       output, control, showing whether destination is require double SEW width elements, or quadruple width elements in multiply
*       io.sigs.sign         :       output, control, showing operands are unsigned or signed
*       io.sigs.isSEWOut     :       output, control, showing an instruction's results are SEW related or not
*       io.sigs.isMLENOut    :       output, control, showing an instruction's results are MLEN related or not
*       io.sigs.isScalarXOut :       output, control, showing an instruction's results are fixed-point scalar to write in Rocket GPR or not
*       io.sigs.src1Typ      :       output[Src1Typ_SZ-1:0], control, showing the type of source 1 is which type: vector, scalar or immediate
*       io.sigs.majFun       :       output[MajFun_SZ-1:0], control, showing which major function the inst is
*       io.sigs.addFun       :       output[AddFun_SZ-1:0], control, showing which add function the inst is
*       io.sigs.shiftFun     :       output[ShiftFun_SZ-1:0], control, showing which shift function the inst is
*       io.sigs.cmpFun       :       output[CmpFun_SZ-1:0], control, showing which compare function the inst is
*       io.sigs.prodAddWhat  :       output, control, showing multiply product add what
*       io.sigs.mulFun       :       output[MulFun-SZ-1:0], control, showing which multiply function the inst is
*       io.sigs.isRND        :       output, control, showing whether the results need rounding or not
*       io.sigs.isSAT        :       output, control, showing whether the results need saturate or not
*       io.sigs.wflags       :       output, control, showing whether need to write fflags or not
*       io.sigs.fmaFun       :       output[FMAFun_SZ-1:0], control, showing which fma(floating-point multiply add) function the inst is
*       io.sigs.isFullMul    :       output, control, showing whether the multiply product is double SEW width output or not
*       io.sigs.funct6       :       output[5:0], control, funct6 field of an inst
*       io.sigs.vm           :       output, control, vm field field of an inst, show whether results are masked or unmasked
*       io.sigs.src2Field    :       output[4:0], source 2 field of an inst
*       io.sigs.src1Field    :       output[4:0], source 1 field of an inst
*       io.sigs.ldstWidth    :       output[LdStWidth_SZ-1:0], control, show which width of load&store data is: byte, halfword, word or SEW
*       io.sigs.dstField     :       output[4:0], destination field of an inst
*       io.sigs.isCSRInst    :       output, control, showing whether the inst is a CSR inst or not
*       io.sigs.isALInst     :       output, control, showing whether the inst is a AL inst or not
*       io.sigs.isLdInst     :       output, control, showing whether the inst is a load inst or not
*       io.sigs.isStInst     :       output, control, showing whether the inst is a store inst or not
*       io.sigs.isAMOInst    :       output, control, showing whether the inst is a AMO inst or not
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import VInstructions._
import util._

//control signals from decoder
class VPUCtrlSigs extends Bundle with VConsts {
  val isUnmasked    = Bool()
  val isSrc12SEW    = Bool()
  val isSrc22SEW    = Bool()
  val isVd2SEW      = Bool()
  val sign          = Bool()
  val isSEWOut      = Bool()
  val isMLENOut     = Bool()
  val isScalarXOut  = Bool()
  val src1Typ       = UInt(Src1Typ_SZ.W)
  val majFun        = UInt(MajFun_SZ.W)
  val addFun        = UInt(AddFun_SZ.W)
  val shiftFun      = UInt(ShiftFun_SZ.W)
  val cmpFun        = UInt(CmpFun_SZ.W)
  val prodAddWhat   = Bool()
  val mulFun        = UInt(MulFun_SZ.W)
  val isRND         = Bool()
  val isSAT         = Bool()
  val wflags        = Bool()
  val fmaFun        = UInt(FMAFun_SZ.W)

  val isFullMul     = Bool()

  val funct6        = UInt(6.W)
  val vm            = UInt(1.W)
  val src2Field     = UInt(5.W)
  val src1Field     = UInt(5.W)
  val ldstWidth     = UInt(LdStWidth_SZ.W)
  val dstField      = UInt(5.W)
  val isCSRInst     = Bool()
  val isALInst      = Bool()
  val isLdInst      = Bool()
  val isStInst      = Bool()
  val isAMOInst     = Bool()
}



class VDecode(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val inst      = Input(UInt(32.W))
    val sigs      = Output(new VPUCtrlSigs)
  })


  val default: List[BitPat] = 
                      //   isUnmasked
                      //   | isSrc12SEW
                      //   | | isSrc22SEW
                      //   | | | isVd2SEW
                      //   | | | | sign
                      //   | | | | |  isSEWOut
                      //   | | | | |  | isMLENOut
                      //   | | | | |  | | isScalarXOut
                      //   | | | | |  | | |                                                          prodAddWhat                          wflags
                      //   | | | | |  | | | src1Typ    majFun    addFun      shiftFun     cmpFun     |        mulFun         isRND isSAT  |  fmaFun
                      //   | | | | |  | | | |          |         |           |            |          |        |              |     |      |  |
                      List(X,X,X,X,X, X,X,X,Src1_X,    MajFun,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     X, FMAFun_X)
//integer arithmetic and logic insts
  val al: Array[(BitPat, List[BitPat])] = Array(
    //integer add sub insts
    VADD_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADD_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADD_VI        -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSUB_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSUB_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VRSUB_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Rev, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VRSUB_VI       -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsAdd,    AddFun_Rev, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_VV      -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_VX      -> List(N,N,N,Y,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_VV       -> List(N,N,N,Y,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_VX       -> List(N,N,N,Y,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_VV      -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_VX      -> List(N,N,N,Y,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_VV       -> List(N,N,N,Y,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_VX       -> List(N,N,N,Y,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_WV      -> List(N,N,Y,Y,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_WX      -> List(N,N,Y,Y,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_WV       -> List(N,N,Y,Y,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_WX       -> List(N,N,Y,Y,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_WV      -> List(N,N,Y,Y,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_WX      -> List(N,N,Y,Y,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_WV       -> List(N,N,Y,Y,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_WX       -> List(N,N,Y,Y,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer add-with-carry and sub-with-borrow insts
    VADC_VVM       -> List(Y,N,N,N,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Adc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADC_VXM       -> List(Y,N,N,N,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Adc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADC_VIM       -> List(Y,N,N,N,N, Y,N,N,Src1_Imm,  IsAdd,    AddFun_Adc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VVM      -> List(Y,N,N,N,N, N,Y,N,Src1_Vs,   IsAdd,    AddFun_Adc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VXM      -> List(Y,N,N,N,N, N,Y,N,Src1_Xs,   IsAdd,    AddFun_Adc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VIM      -> List(Y,N,N,N,N, N,Y,N,Src1_Imm,  IsAdd,    AddFun_Adc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VV       -> List(Y,N,N,N,N, N,Y,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VX       -> List(Y,N,N,N,N, N,Y,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VI       -> List(Y,N,N,N,N, N,Y,N,Src1_Imm,  IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSBC_VVM       -> List(Y,N,N,N,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sbc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSBC_VXM       -> List(Y,N,N,N,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sbc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VVM      -> List(Y,N,N,N,N, N,Y,N,Src1_Vs,   IsAdd,    AddFun_Sbc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VXM      -> List(Y,N,N,N,N, N,Y,N,Src1_Xs,   IsAdd,    AddFun_Sbc, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VV       -> List(Y,N,N,N,N, N,Y,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VX       -> List(Y,N,N,N,N, N,Y,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer bitwise logical insts
    VAND_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VAND_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VAND_VI        -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VOR_VV         -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VOR_VX         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VOR_VI         -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VXOR_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VXOR_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VXOR_VI        -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsBit,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //integer shift insts
    VSLL_VV        -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_LL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSLL_VX        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_LL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSLL_VI        -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_LL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRL_VV        -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRL_VX        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRL_VI        -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRA_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRA_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRA_VI        -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer narrowing right shift insts
    VNSRL_WV       -> List(N,N,Y,N,N, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRL_WX       -> List(N,N,Y,N,N, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRL_WI       -> List(N,N,Y,N,N, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRA_WV       -> List(N,N,Y,N,Y, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRA_WX       -> List(N,N,Y,N,Y, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRA_WI       -> List(N,N,Y,N,Y, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer comparison insts
    VMSEQ_VV       -> List(N,N,N,N,Y, N,Y,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_EQ, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSEQ_VX       -> List(N,N,N,N,Y, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_EQ, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSEQ_VI       -> List(N,N,N,N,Y, N,Y,N,Src1_Imm,  IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_EQ, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSNE_VV       -> List(N,N,N,N,Y, N,Y,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_NE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSNE_VX       -> List(N,N,N,N,Y, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_NE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSNE_VI       -> List(N,N,N,N,Y, N,Y,N,Src1_Imm,  IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_NE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLTU_VV      -> List(N,N,N,N,N, N,Y,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLTU_VX      -> List(N,N,N,N,N, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLT_VV       -> List(N,N,N,N,Y, N,Y,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLT_VX       -> List(N,N,N,N,Y, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLEU_VV      -> List(N,N,N,N,N, N,Y,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLEU_VX      -> List(N,N,N,N,N, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLEU_VI      -> List(N,N,N,N,N, N,Y,N,Src1_Imm,  IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLE_VV       -> List(N,N,N,N,Y, N,Y,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLE_VX       -> List(N,N,N,N,Y, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLE_VI       -> List(N,N,N,N,Y, N,Y,N,Src1_Imm,  IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGTU_VX      -> List(N,N,N,N,N, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGTU_VI      -> List(N,N,N,N,N, N,Y,N,Src1_Imm,  IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGT_VX       -> List(N,N,N,N,Y, N,Y,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGT_VI       -> List(N,N,N,N,Y, N,Y,N,Src1_Imm,  IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //integer min-max insts
    VMINU_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMINU_VX       -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMIN_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMIN_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMAXU_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMAXU_VX       -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMAX_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMAX_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsCmp,    AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     N, FMAFun_X))
//CSR insts
  val csr: Array[(BitPat, List[BitPat])] = Array(
    VSETVLI        -> List(N,N,N,N,N, N,N,Y,Src1_Xs,   IsCSR,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSETVL         -> List(N,N,N,N,N, N,N,Y,Src1_Xs,   IsCSR,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//load insts
  val load: Array[(BitPat, List[BitPat])] = Array(
    VLB_V          -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLH_V          -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLW_V          -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLBU_V         -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLHU_V         -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLWU_V         -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLE_V          -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSB_V         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSH_V         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSW_V         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSBU_V        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSHU_V        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSWU_V        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLSE_V         -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXB_V         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXH_V         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXW_V         -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXBU_V        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXHU_V        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXWU_V        -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VLXE_V         -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsLoad,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//store insts
  val store: Array[(BitPat, List[BitPat])] = Array(
    VSB_V          -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSH_V          -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSW_V          -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSE_V          -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSSB_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSSH_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSSW_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSSE_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSXB_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSXH_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSXW_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSXE_V         -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSUXB_V        -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSUXH_V        -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSUXW_V        -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSUXE_V        -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsStore,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//mask insts
  val mask: Array[(BitPat, List[BitPat])] = Array(
    //mask population count inst
    VPOPC_M        -> List(N,N,N,N,Y, N,N,Y,Src1_Code, IsPopc,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //find-first-set mask bit inst
    VFIRST_M       -> List(N,N,N,N,Y, N,N,Y,Src1_Code, IsFirst,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //set-before/including/only-first mask bit insts
    VMSBF_M        -> List(N,N,N,N,Y, N,Y,N,Src1_Code, IsMIdx,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSOF_M        -> List(N,N,N,N,Y, N,Y,N,Src1_Code, IsMIdx,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMSIF_M        -> List(N,N,N,N,Y, N,Y,N,Src1_Code, IsMIdx,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //iota inst
    VIOTA_M        -> List(N,N,N,N,Y, Y,N,N,Src1_Code, IsIota,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //element index inst
    VID_V          -> List(N,N,N,N,Y, Y,N,N,Src1_Code, IsIdx,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    //mask-register logical insts
    VMANDNOT_MM    -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMAND_MM       -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMOR_MM        -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMXOR_MM       -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMORNOT_MM     -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMNAND_MM      -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMNOR_MM       -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMXNOR_MM      -> List(Y,N,N,N,Y, N,Y,N,Src1_Vs,   IsMBit,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//integer merge insts
  def merge: Array[(BitPat, List[BitPat])] = Array(
    VMERGE_VVM     -> List(Y,N,N,N,Y, Y,N,N,Src1_Vs,   IsMerge,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMERGE_VXM     -> List(Y,N,N,N,Y, Y,N,N,Src1_Xs,   IsMerge,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMERGE_VIM     -> List(Y,N,N,N,Y, Y,N,N,Src1_Imm,  IsMerge,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//integer multiply and divide insts
  def mul: Array[(BitPat, List[BitPat])] = Array(
    VMUL_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMUL_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMULH_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VMULH_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VMULHU_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VMULHU_VX      -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VMULHSU_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VMULHSU_VX     -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VWMUL_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulF,   RND_N,SAT_N, N, FMAFun_X),
    VWMUL_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulF,   RND_N,SAT_N, N, FMAFun_X),
    VWMULU_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulFU,  RND_N,SAT_N, N, FMAFun_X),
    VWMULU_VX      -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulFU,  RND_N,SAT_N, N, FMAFun_X),
    VWMULSU_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulFSU, RND_N,SAT_N, N, FMAFun_X),
    VWMULSU_VX     -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulFSU, RND_N,SAT_N, N, FMAFun_X),
    VDIV_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_Div,    RND_N,SAT_N, N, FMAFun_X),
    VDIV_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_Div,    RND_N,SAT_N, N, FMAFun_X),
    VDIVU_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_DivU,   RND_N,SAT_N, N, FMAFun_X),
    VDIVU_VX       -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_DivU,   RND_N,SAT_N, N, FMAFun_X),
    VREM_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_Rem,    RND_N,SAT_N, N, FMAFun_X),
    VREM_VX        -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_Rem,    RND_N,SAT_N, N, FMAFun_X),
    VREMU_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_RemU,   RND_N,SAT_N, N, FMAFun_X),
    VREMU_VX       -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_RemU,   RND_N,SAT_N, N, FMAFun_X))
//integer multiply-add insts
  def madd: Array[(BitPat, List[BitPat])] = Array(
    VMADD_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMADD_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSUB_VV      -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Sub, ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSUB_VX      -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Sub, ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMACC_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMACC_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSAC_VV      -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Sub, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSAC_VX      -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Sub, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VWMACCU_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFU,  RND_N,SAT_N, N, FMAFun_X),
    VWMACCU_VX     -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFU,  RND_N,SAT_N, N, FMAFun_X),
    VWMACC_VV      -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulF,   RND_N,SAT_N, N, FMAFun_X),
    VWMACC_VX      -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulF,   RND_N,SAT_N, N, FMAFun_X),
    VWMACCSU_VV    -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFUS, RND_N,SAT_N, N, FMAFun_X),
    VWMACCSU_VX    -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFUS, RND_N,SAT_N, N, FMAFun_X),
    VWMACCUS_VX    -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFSU, RND_N,SAT_N, N, FMAFun_X))
//integer quad-widening multiply-add insts
  def qmacc: Array[(BitPat, List[BitPat])] = Array(
    VQMACCU_VV     -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFU,  RND_N,SAT_N, N, FMAFun_X),
    VQMACCU_VX     -> List(N,N,N,Y,N, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFU,  RND_N,SAT_N, N, FMAFun_X),
    VQMACC_VV      -> List(N,N,N,Y,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulF,   RND_N,SAT_N, N, FMAFun_X),
    VQMACC_VX      -> List(N,N,N,Y,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulF,   RND_N,SAT_N, N, FMAFun_X),
    VQMACCSU_VV    -> List(N,N,N,Y,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFUS, RND_N,SAT_N, N, FMAFun_X),
    VQMACCSU_VX    -> List(N,N,N,Y,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFUS, RND_N,SAT_N, N, FMAFun_X),
    VQMACCUS_VX    -> List(N,N,N,Y,Y, Y,N,N,Src1_Xs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_MulFSU, RND_N,SAT_N, N, FMAFun_X))
//integer reduction insts
  def red: Array[(BitPat, List[BitPat])] = Array(
    VWREDSUMU_VS   -> List(N,Y,N,Y,N, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VWREDSUM_VS    -> List(N,Y,N,Y,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDSUM_VS     -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDAND_VS     -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDOR_VS      -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDXOR_VS     -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMINU_VS    -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMIN_VS     -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMAXU_VS    -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMAX_VS     -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsRed,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//integer scalar move insts 
  def mv: Array[(BitPat, List[BitPat])] = Array(
    VMV_X_S        -> List(N,N,N,N,N, N,N,Y,Src1_Code, IsMv,     AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VMV_S_X        -> List(Y,N,N,N,N, Y,N,N,Src1_Xs,   IsMv,     AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//fixed-point saturating add and subtract insts
  def sadd: Array[(BitPat, List[BitPat])] = Array(
    VSADDU_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADDU_VX      -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADDU_VI      -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADD_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADD_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADD_VI       -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUBU_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUBU_VX      -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUB_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUB_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_N,SAT_Y, N, FMAFun_X))
//fixed-point averaging add and subtract insts
  def aadd: Array[(BitPat, List[BitPat])] = Array(
    VAADDU_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VAADDU_VX      -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VAADD_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VAADD_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Add, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUBU_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUBU_VX      -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUB_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUB_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAdd,    AddFun_Sub, ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X))
//fractional mul insts
  def smul: Array[(BitPat, List[BitPat])] = Array(
    VSMUL_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulH,   RND_Y,SAT_Y, N, FMAFun_X),
    VSMUL_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsMulDiv, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_MulH,   RND_Y,SAT_Y, N, FMAFun_X))
//scaling shift insts
  def ssr: Array[(BitPat, List[BitPat])] = Array(
    VSSRL_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRL_VX       -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRL_VI       -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRA_VV       -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRA_VX       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRA_VI       -> List(N,N,N,N,Y, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_N, N, FMAFun_X))
//narrowing fixed-point clip insts
  def nclip: Array[(BitPat, List[BitPat])] = Array(
    VNCLIPU_WV     -> List(N,N,Y,N,N, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIPU_WX     -> List(N,N,Y,N,N, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIPU_WI     -> List(N,N,Y,N,N, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RL, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIP_WV      -> List(N,N,Y,N,Y, Y,N,N,Src1_Vs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIP_WX      -> List(N,N,Y,N,Y, Y,N,N,Src1_Xs,   IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIP_WI      -> List(N,N,Y,N,Y, Y,N,N,Src1_Imm,  IsShift,  AddFun_X,   ShiftFun_RA, CmpFun_X,  X,       MulFun_X,      RND_Y,SAT_Y, N ,FMAFun_X))
//slide insts
  def slide: Array[(BitPat, List[BitPat])] = Array(
    VSLIDEUP_VX    -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsSlide,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDEUP_VI    -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsSlide,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDEDOWN_VX  -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsSlide,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDEDOWN_VI  -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsSlide,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDE1UP_VX   -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsSlide1, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDE1DOWN_VX -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsSlide1, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//register gather insts
  def gather: Array[(BitPat, List[BitPat])] = Array(
    VRGATHER_VV    -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsGather, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VRGATHER_VX    -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsGather, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VRGATHER_VI    -> List(N,N,N,N,N, Y,N,N,Src1_Imm,  IsGather, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//compress elements inst
  def zip: Array[(BitPat, List[BitPat])] = Array(
    VCOMPRESS_VM   -> List(Y,N,N,N,N, Y,N,N,Src1_Vs,   IsZip,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//copy registers inst
  def copy: Array[(BitPat, List[BitPat])] = Array(
    VMVR_V         -> List(Y,N,N,N,N, Y,N,N,Src1_Code, IsCopy,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//integer dot-product insts
  def dot: Array[(BitPat, List[BitPat])] = Array( //TODO release majFun for vdot.vv
    VDOTU_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VDOT_VV        -> List(N,N,N,N,Y, Y,N,N,Src1_Vs,   IsMulAdd, AddFun_Add, ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X))
//floating-point add and multiply multiply-add insts
  def fma: Array[(BitPat, List[BitPat])] = Array(
    //floating-point add sub insts
    VFADD_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFADD_VF       -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFSUB_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFSUB_VF       -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFRSUB_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_RSub),
    VFWADD_VV      -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWADD_VF      -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWSUB_VV      -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWSUB_VF      -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWADD_WV      -> List(N,N,Y,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWADD_WF      -> List(N,N,Y,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWSUB_WV      -> List(N,N,Y,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWSUB_WF      -> List(N,N,Y,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_Sub),
    //floating-point multiply insts
    VFMUL_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_Mul),
    VFMUL_VF       -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_Mul),
    VFWMUL_VV      -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_Mul),
    VFWMUL_VF      -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_Mul),
    //floating-point multiply-add insts
    VFMACC_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MAdd),
    VFMACC_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MAdd),
    VFNMACC_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMAdd),
    VFNMACC_VF     -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMAdd),
    VFMSAC_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MSub),
    VFMSAC_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MSub),
    VFNMSAC_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMSub),
    VFNMSAC_VF     -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMSub),
    VFMADD_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_MAdd),
    VFMADD_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_MAdd),
    VFNMADD_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_NMAdd),
    VFNMADD_VF     -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_NMAdd),
    VFMSUB_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_MSub),
    VFMSUB_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_MSub),
    VFNMSUB_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_NMSub),
    VFNMSUB_VF     -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vs2, MulFun_X,      X,    X,     Y, FMAFun_NMSub),
    VFWMACC_VV     -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MAdd),
    VFWMACC_VF     -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MAdd),
    VFWNMACC_VV    -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMAdd),
    VFWNMACC_VF    -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMAdd),
    VFWMSAC_VV     -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MSub),
    VFWMSAC_VF     -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MSub),
    VFWNMSAC_VV    -> List(N,N,N,Y,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMSub),
    VFWNMSAC_VF    -> List(N,N,N,Y,N, Y,N,N,Src1_Fs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_NMSub))
//floating-point/integer type-convert insts
  def fcvt: Array[(BitPat, List[BitPat])] = Array(
    VFCVT_XU_F_V   -> List(N,N,N,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFCVT_X_F_V    -> List(N,N,N,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFCVT_F_XU_V   -> List(N,N,N,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFCVT_F_X_V    -> List(N,N,N,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWCVT_XU_F_V  -> List(N,N,N,Y,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWCVT_X_F_V   -> List(N,N,N,Y,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWCVT_F_XU_V  -> List(N,N,N,Y,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWCVT_F_X_V   -> List(N,N,N,Y,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWCVT_F_F_V   -> List(N,N,N,Y,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_XU_F_W  -> List(N,N,Y,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_X_F_W   -> List(N,N,Y,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_F_XU_W  -> List(N,N,Y,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_F_X_W   -> List(N,N,Y,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_F_F_W   -> List(N,N,Y,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_ROD_FFW -> List(N,N,Y,N,N, Y,N,N,Src1_Code, IsFCvt,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X))
//floating-point compare insts
  def fcmp: Array[(BitPat, List[BitPat])] = Array(
    VMFEQ_VV       -> List(N,N,N,N,N, N,Y,N,Src1_Vs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_EQ, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFEQ_VF       -> List(N,N,N,N,N, N,Y,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_EQ, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFNE_VV       -> List(N,N,N,N,N, N,Y,N,Src1_Vs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_NE, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFNE_VF       -> List(N,N,N,N,N, N,Y,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_NE, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLT_VV       -> List(N,N,N,N,N, N,Y,N,Src1_Vs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLT_VF       -> List(N,N,N,N,N, N,Y,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_LT, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLE_VV       -> List(N,N,N,N,N, N,Y,N,Src1_Vs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLE_VF       -> List(N,N,N,N,N, N,Y,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_LE, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFGT_VF       -> List(N,N,N,N,N, N,Y,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_GT, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFGE_VF       -> List(N,N,N,N,N, N,Y,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_GE, X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    //floating-point min-max insts
    VFMIN_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_Min,X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFMIN_VF       -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_Min,X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFMAX_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_Max,X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFMAX_VF       -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFCmp,   AddFun_X,   ShiftFun_X,  CmpFun_Max,X,       MulFun_X,      X,    X,     Y, FMAFun_X))
//floating-point sign-injection insts
  def fsgnj: Array[(BitPat, List[BitPat])] = Array(
    VFSGNJ_VV      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFSgnJ,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJ_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFSgnJ,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJN_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFSgnJ,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJN_VF     -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFSgnJ,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJX_VV     -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFSgnJ,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJX_VF     -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFSgnJ,  AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//floating-point classify inst
  def fclass: Array[(BitPat, List[BitPat])] = Array(
    VFCLASS_V      -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFClass, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//floating-point merge inst
  def fmerge: Array[(BitPat, List[BitPat])] = Array(
    VFMERGE_VFM    -> List(Y,N,N,N,N, Y,N,N,Src1_Fs,   IsFMerge, AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//floating-point scalar move insts
  def fmv: Array[(BitPat, List[BitPat])] = Array(
    VFMV_F_S       -> List(N,N,N,N,N, N,N,Y,Src1_Code, IsFMv,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VFMV_S_F       -> List(Y,N,N,N,N, Y,N,N,Src1_Fs,   IsFMv,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))
//floating-point divide and square root insts
  def fdiv: Array[(BitPat, List[BitPat])] = Array(
    //floating-point divide insts
    VFDIV_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFDiv,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFDIV_VF       -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFDiv,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFRDIV_VF      -> List(N,N,N,N,N, Y,N,N,Src1_Fs,   IsFDiv,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    //floating-point square root inst
    VFSQRT_V       -> List(N,N,N,N,N, Y,N,N,Src1_Code, IsFDiv,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_X))
//floating-point reduction insts
  def fred: Array[(BitPat, List[BitPat])] = Array(
    VFREDSUM_VS    -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFRed,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFREDOSUM_VS   -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFRed,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFREDMIN_VS    -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFRed,   AddFun_X,   ShiftFun_X,  CmpFun_Min,X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFREDMAX_VS    -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFRed,   AddFun_X,   ShiftFun_X,  CmpFun_Max,X,       MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWREDSUM_VS   -> List(N,Y,N,Y,N, Y,N,N,Src1_Vs,   IsFRed,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWREDOSUM_VS  -> List(N,Y,N,Y,N, Y,N,N,Src1_Vs,   IsFRed,   AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     Y, FMAFun_Add))
//floating-point dot-product inst
  def fdot: Array[(BitPat, List[BitPat])] = Array( //TODO release majFun for vfdot.vv
    VFDOT_VV       -> List(N,N,N,N,N, Y,N,N,Src1_Vs,   IsFMA,    AddFun_X,   ShiftFun_X,  CmpFun_X,  Add_Vd,  MulFun_X,      X,    X,     Y, FMAFun_MAdd))
//AMO insts
  def amo: Array[(BitPat, List[BitPat])] = Array(
    VAMOYW_V       -> List(N,N,N,N,Y, Y,N,N,Src1_Xs,   IsAMO,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VAMOYE_V       -> List(N,N,N,N,N, Y,N,N,Src1_Xs,   IsAMO,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VAMONW_V       -> List(N,N,N,N,Y, N,N,N,Src1_Xs,   IsAMO,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X),
    VAMONE_V       -> List(N,N,N,N,N, N,N,N,Src1_Xs,   IsAMO,    AddFun_X,   ShiftFun_X,  CmpFun_X,  X,       MulFun_X,      X,    X,     N, FMAFun_X))



  val MUL = MULDIV || MULADD || QMULADD || SATMUL
  val MADD = MULADD || QMULADD
  val insns = al ++ csr ++ load ++ store ++ mask ++ 
              (if(MERGE)    merge  else Nil) ++ 
              (if(MUL)      mul    else Nil) ++ 
              (if(MADD)     madd   else Nil) ++ 
              (if(QMULADD)  qmacc  else Nil) ++ 
              (if(RED)      red    else Nil) ++ 
              (if(MV)       mv     else Nil) ++ 
              (if(SATADD)   sadd   else Nil) ++ 
              (if(AVERADD)  aadd   else Nil) ++ 
              (if(SATMUL)   smul   else Nil) ++ 
              (if(SCALESR)  ssr    else Nil) ++ 
              (if(NCLIP)    nclip  else Nil) ++ 
              (if(SLIDE)    slide  else Nil) ++ 
              (if(GATHER)   gather else Nil) ++ 
              (if(COMPRESS) zip    else Nil) ++ 
              (if(COPY)     copy   else Nil) ++ 
              (if(DOT)      dot    else Nil) ++ 
              (if(FMA)      fma    else Nil) ++ 
              (if(FCVT)     fcvt   else Nil) ++ 
              (if(FCMP)     fcmp   else Nil) ++ 
              (if(FSGNJ)    fsgnj  else Nil) ++ 
              (if(FCLASS)   fclass else Nil) ++ 
              (if(FMERGE)   fmerge else Nil) ++ 
              (if(FMV)      fmv    else Nil) ++ 
              (if(FDIVSQRT) fdiv   else Nil) ++ 
              (if(FRED)     fred   else Nil) ++ 
              (if(FDOT)     fdot   else Nil) ++ 
              (if(AMO)      amo    else Nil)
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.isUnmasked,
                 s.isSrc12SEW, s.isSrc22SEW, s.isVd2SEW, 
                 s.sign, 
                 s.isSEWOut, s.isMLENOut, s.isScalarXOut, 
                 s.src1Typ, s.majFun, 
                 s.addFun, s.shiftFun, s.cmpFun, 
                 s.prodAddWhat, s.mulFun, 
                 s.isRND, s.isSAT, 
                 s.wflags, 
                 s.fmaFun)

  sigs zip decoder map { case(s,d) => s := d }

  s.funct6     := io.inst(31,26)
  s.vm         := io.inst(25)
  s.src2Field  := io.inst(24,20)
  s.src1Field  := io.inst(19,15)
  s.ldstWidth  := io.inst(13,12)
  s.dstField   := io.inst(11,7)
  s.isCSRInst  := io.inst(6,0) === "b1010111".U(7.W) && io.inst(14,12) === "b111".U(3.W)
  s.isALInst   := io.inst(6,0) === "b1010111".U(7.W) && io.inst(14,12) =/= "b111".U(3.W)
  s.isLdInst   := io.inst(6,0) === "b0000111".U(7.W)
  s.isStInst   := io.inst(6,0) === "b0100111".U(7.W)
  s.isAMOInst  := (if(AMO) io.inst(6,0) === "b0101111".U(7.W) else false.B)

  s.isFullMul := (if(MULDIV || MULADD || QMULADD)
                   (s.majFun === IsMulDiv      || s.majFun === IsMulAdd) && 
                   (s.mulFun === MulFun_MulF   || s.mulFun === MulFun_MulFSU || 
                    s.mulFun === MulFun_MulFUS || s.mulFun === MulFun_MulFU)
                 else false.B)

}
