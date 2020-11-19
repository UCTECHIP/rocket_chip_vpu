// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename             :       VDecode.scala
*       Author               :       liangzh
*       Revision             :       2020/10/28
*       Company              :       UC TECH IP
*       Description          :       decode RISC-V vector instructions
*
*       io.inst                :       input[31:0], vector instructions, to be decoded
*       io.sigs.isUnmasked     :       output, control, shows whether result elements can be masked or not
*       io.sigs.swap12         :       output, control, shows whether source 1 and source 2 swap position in an operation
*       io.sigs.swap23         :       output, control, shows whether source 2 and source 3 swap position in an operation
*       io.sigs.src1EEW        :       output[2:0], control, shows source 1 EEW
*       io.sigs.isSrc1t2SEW    :       output, control, shows whether source 1 need to be expanded to 2SEW width
*       io.sigs.src2EEW        :       output[2:0], control, shows source 2 EEW
*       io.sigs.isSrc2t2SEW    :       output, control, shows whether source 2 need to be expanded to 2SEW width
*       io.sigs.destEEW        :       output[1:0], control, shows destination EEW
*       io.sigs.sign           :       output, control, shows sources sign
*       io.sigs.majFun         :       output[5:0], control, shows major function of insts
*       io.sigs.addFun         :       output[1:0], control, shows add function of insts
*       io.sigs.shiftDir       :       output, control, shows shift direction
*       io.sigs.mulFun         :       output[3:0], control, shows multiply function of insts
*       io.sigs.isRND          :       output, control, shows whether results need rounding or not
*       io.sigs.isSAT          :       output, control, shows whether results need saturating or not
*       io.sigs.wflags         :       output, control, shows whether need to write flags or not
*       io.sigs.fmaFun         :       output[2:0], control, shows floating-point multiply-add function
*       io.sigs.funct6         :       output[5:0], control, funct6 field of insts
*       io.sigs.vm             :       output, control, vm field of insts
*       io.sigs.src2Field      :       output[4:0], control, source 2 field of insts
*       io.sigs.src1Field      :       output[4:0], control, source 1 field of insts
*       io.sigs.widthField     :       output[2:0], control, width field of insts
*       io.sigs.dstField       :       output[4:0], control, dest field of insts
*       io.sigs.isCSRInst      :       output, control, shows whether the inst is a CSR inst or not
*       io.sigs.isALInst       :       output, control, shows whether the inst is a AL inst or not
*       io.sigs.isLdInst       :       output, control, shows whether the inst is a load inst or not
*       io.sigs.isStInst       :       output, control, shows whether the inst is a store inst or not
*       io.sigs.isAMOInst      :       output, control, shows whether the inst is a AMO inst or not
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import VInstructions._
import util._

class VDecode(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val sigs = Output(new VPUCtrlSigs)
  })

  val default: List[BitPat] = 
                       //   isUnmasked
                       //   | swap12                                                     
                       //   | | swap23        isSrc1t2SEW  isSrc2t2SEW                                                                            wflags
                       //   | | |  src1EEW    |  src2EEW   |  destEEW    sign      majFun     addFun      shiftDir    mulFun         isRND isSAT  |  fmaFun
                       //   | | |  |          |  |         |  |          |         |          |           |           |              |     |      |  |
                       List(X,X,X, Src1_X,    X, Src2_X,   X, Dest_X,    Sign_X,   MajFun_X,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     X, FMAFun_X)
//CSR insts
  def csr: Array[(BitPat, List[BitPat])] = Array(
    VSETVLI         -> List(N,N,N, Src1_N,    X, Src2_XLEN,X, Dest_XLEN, Sign_X,   IsCSR,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSETVL          -> List(N,N,N, Src1_N,    X, Src2_XLEN,X, Dest_XLEN, Sign_X,   IsCSR,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//integer arithmetic and logic insts
  def al: Array[(BitPat, List[BitPat])] = Array(
    //integer add sub insts
    VADD_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADD_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADD_VI         -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSUB_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSUB_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VRSUB_VX        -> List(N,Y,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VRSUB_VI        -> List(N,Y,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_VV       -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_VX       -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_VV        -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_VX        -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_VV       -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_VX       -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_VV        -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_VX        -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_WV       -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADDU_WX       -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_WV        -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWADD_WX        -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_WV       -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUBU_WX       -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_WV        -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VWSUB_WX        -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer add-with-carry and sub-with-borrow insts
    VADC_VVM        -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADC_VXM        -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VADC_VIM        -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VVM       -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VXM       -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VIM       -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VV        -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VX        -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMADC_VI        -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSBC_VVM        -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSBC_VXM        -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VVM       -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VXM       -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VV        -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VMSBC_VX        -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer extension insts
    VZEXT_VF8       -> List(N,N,N, Src1_SEWf8,N, Src2_N,   N, Dest_SEW,  Unsigned, IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSEXT_VF8       -> List(N,N,N, Src1_SEWf8,N, Src2_N,   N, Dest_SEW,  Signed,   IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VZEXT_VF4       -> List(N,N,N, Src1_SEWf4,N, Src2_N,   N, Dest_SEW,  Unsigned, IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSEXT_VF4       -> List(N,N,N, Src1_SEWf4,N, Src2_N,   N, Dest_SEW,  Signed,   IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VZEXT_VF2       -> List(N,N,N, Src1_SEWf2,N, Src2_N,   N, Dest_SEW,  Unsigned, IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSEXT_VF2       -> List(N,N,N, Src1_SEWf2,N, Src2_N,   N, Dest_SEW,  Signed,   IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    //integer bitwise logical insts
    VAND_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VAND_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VAND_VI         -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VOR_VV          -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VOR_VX          -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VOR_VI          -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VXOR_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VXOR_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VXOR_VI         -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    //integer shift insts
    VSLL_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftLeft,  MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSLL_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftLeft,  MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSLL_VI         -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftLeft,  MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRL_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRL_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRL_VI         -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRA_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRA_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VSRA_VI         -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer narrowing right shift insts
    VNSRL_WV        -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRL_WX        -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRL_WI        -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRA_WV        -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRA_WX        -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    VNSRA_WI        -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X),
    //integer comparison insts
    VMSEQ_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSEQ_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSEQ_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSNE_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSNE_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSNE_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLTU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLTU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLT_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLT_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLEU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLEU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLEU_VI       -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLE_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLE_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSLE_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGTU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGTU_VI       -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGT_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSGT_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    //integer min-max insts
    VMINU_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMINU_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMIN_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMIN_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMAXU_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMAXU_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMAX_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMAX_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//integer merge insts
  def merge: Array[(BitPat, List[BitPat])] = Array(
    VMERGE_VVM      -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsMerge,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMERGE_VXM      -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsMerge,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMERGE_VIM      -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsMerge,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//integer multiply and divide insts
  def mul: Array[(BitPat, List[BitPat])] = Array(
    VMUL_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMUL_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMULH_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VMULH_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VMULHU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VMULHU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VMULHSU_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VMULHSU_VX      -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VWMUL_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VWMUL_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VWMULU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VWMULU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VWMULSU_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VWMULSU_VX      -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VDIV_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Div,    RND_N,SAT_N, N, FMAFun_X),
    VDIV_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Div,    RND_N,SAT_N, N, FMAFun_X),
    VDIVU_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_DivU,   RND_N,SAT_N, N, FMAFun_X),
    VDIVU_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_DivU,   RND_N,SAT_N, N, FMAFun_X),
    VREM_VV         -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Rem,    RND_N,SAT_N, N, FMAFun_X),
    VREM_VX         -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Rem,    RND_N,SAT_N, N, FMAFun_X),
    VREMU_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_RemU,   RND_N,SAT_N, N, FMAFun_X),
    VREMU_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_RemU,   RND_N,SAT_N, N, FMAFun_X)
  )
//integer multiply-add insts
  def madd: Array[(BitPat, List[BitPat])] = Array(
    VMADD_VV        -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMADD_VX        -> List(N,N,Y, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSUB_VV       -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSUB_VX       -> List(N,N,Y, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMACC_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VMACC_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSAC_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VNMSAC_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X),
    VWMACCU_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VWMACCU_VX      -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X),
    VWMACC_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VWMACC_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X),
    VWMACCSU_VV     -> List(N,Y,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VWMACCSU_VX     -> List(N,Y,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X),
    VWMACCUS_VX     -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X)
  )
//fixed-point saturating add and subtract insts
  def sadd: Array[(BitPat, List[BitPat])] = Array(
    VSADDU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADDU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADDU_VI       -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADD_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADD_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSADD_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUBU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUBU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUB_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X),
    VSSUB_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X)
  )
//fixed-point averaging add and subtract insts
  def aadd: Array[(BitPat, List[BitPat])] = Array(
    VAADDU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VAADDU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VAADD_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VAADD_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUBU_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUBU_VX       -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUB_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VASUB_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X)
  )
//fractional mul insts
  def smul: Array[(BitPat, List[BitPat])] = Array(
    VSMUL_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_Y,SAT_Y, N, FMAFun_X),
    VSMUL_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_Y,SAT_Y, N, FMAFun_X)
  )
//scaling shift insts
  def ssr: Array[(BitPat, List[BitPat])] = Array(
    VSSRL_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRL_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRL_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRA_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRA_VX        -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X),
    VSSRA_VI        -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X)
  )
//narrowing fixed-point clip insts
  def nclip: Array[(BitPat, List[BitPat])] = Array(
    VNCLIPU_WV      -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIPU_WX      -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIPU_WI      -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIP_WV       -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIP_WX       -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X),
    VNCLIP_WI       -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X)
  )
//floating-point add and multiply multiply-add insts
  def fma: Array[(BitPat, List[BitPat])] = Array(
    //floating-point add sub insts
    VFADD_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFADD_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFSUB_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFSUB_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFRSUB_VF       -> List(N,Y,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWADD_VV       -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWADD_VF       -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWSUB_VV       -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWSUB_VF       -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWADD_WV       -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWADD_WF       -> List(N,N,N, Src1_2SEW, N, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add),
    VFWSUB_WV       -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    VFWSUB_WF       -> List(N,N,N, Src1_2SEW, N, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub),
    //floating-point multiply insts
    VFMUL_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul),
    VFMUL_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul),
    VFWMUL_VV       -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul),
    VFWMUL_VF       -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul),
    //floating-point multiply-add insts
    VFMACC_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc),
    VFMACC_VF       -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc),
    VFNMACC_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc),
    VFNMACC_VF      -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc),
    VFMSAC_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac),
    VFMSAC_VF       -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac),
    VFNMSAC_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac),
    VFNMSAC_VF      -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac),
    VFMADD_VV       -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc),
    VFMADD_VF       -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc),
    VFNMADD_VV      -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc),
    VFNMADD_VF      -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc),
    VFMSUB_VV       -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac),
    VFMSUB_VF       -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac),
    VFNMSUB_VV      -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac),
    VFNMSUB_VF      -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac),
    VFWMACC_VV      -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc),
    VFWMACC_VF      -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc),
    VFWNMACC_VV     -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc),
    VFWNMACC_VF     -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc),
    VFWMSAC_VV      -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac),
    VFWMSAC_VF      -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac),
    VFWNMSAC_VV     -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac),
    VFWNMSAC_VF     -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac)
  )
//floating-point/integer type-convert insts
  def fcvt: Array[(BitPat, List[BitPat])] = Array(
    VFCVT_V         -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWCVT_V        -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFNCVT_V        -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
  )
//floating-point compare insts
  def fcmp: Array[(BitPat, List[BitPat])] = Array(
    VMFEQ_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFEQ_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFNE_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFNE_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLT_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLT_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLE_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFLE_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFGT_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VMFGE_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
  )
//floating-point min-max insts
  def fminmax: Array[(BitPat, List[BitPat])] = Array(
    VFMIN_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFMIN_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFMAX_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFMAX_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
  )
//floating-point sign-injection insts
  def fsgnj: Array[(BitPat, List[BitPat])] = Array(
    VFSGNJ_VV       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJ_VF       -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJN_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJN_VF      -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJX_VV      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSGNJX_VF      -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//floating-point classify inst
  def fclass: Array[(BitPat, List[BitPat])] = Array(
    VFCLASS_V       -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFClass,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//floating-point merge inst
  def fmerge: Array[(BitPat, List[BitPat])] = Array(
    VFMERGE_VFM     -> List(Y,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMerge,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//floating-point divide and square root insts
  def fdiv: Array[(BitPat, List[BitPat])] = Array(
    //floating-point divide insts
    VFDIV_VV        -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFDIV_VF        -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFRDIV_VF       -> List(N,Y,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    //floating-point square root inst
    VFSQRT_V        -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
)
//floating-point reciprocal square root inst
  def frsqrt: Array[(BitPat, List[BitPat])] = Array(
    VFRSQRTE7_V     -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFRSqrt,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
  )
//floating-point reciprocal inst
  def frece: Array[(BitPat, List[BitPat])] = Array(
    VFRECE7_V       -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFRece,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
  )
//integer reduction insts
  def red: Array[(BitPat, List[BitPat])] = Array(
    VREDSUM_VS      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDAND_VS      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDOR_VS       -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDXOR_VS      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMINU_VS     -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMIN_VS      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMAXU_VS     -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VREDMAX_VS      -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VWREDSUMU_VS    -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Unsigned, IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VWREDSUM_VS     -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
  )
//floating-point reduction insts
  def fred: Array[(BitPat, List[BitPat])] = Array(
    VFREDSUM_VS     -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFREDOSUM_VS    -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFREDMIN_VS     -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFREDMAX_VS     -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWREDSUM_VS    -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X),
    VFWREDOSUM_VS   -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X)
  )
//mask population count inst
  def popc: Array[(BitPat, List[BitPat])] = Array(
    VPOPC_M         -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_XLEN, Sign_X,   IsPopc,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//find-first-set mask bit inst
  def first: Array[(BitPat, List[BitPat])] = Array(
    VFIRST_M        -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_XLEN, Sign_X,   IsFirst,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//set-before/including/only-first mask bit insts
  def mindex: Array[(BitPat, List[BitPat])] = Array(
    VMSBF_M         -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_1b,   Sign_X,   IsMIdx,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSOF_M         -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_1b,   Sign_X,   IsMIdx,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMSIF_M         -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_1b,   Sign_X,   IsMIdx,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//iota inst
  def iota: Array[(BitPat, List[BitPat])] = Array(
    VIOTA_M         -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_SEW,  Sign_X,   IsIota,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//element index inst
  def index: Array[(BitPat, List[BitPat])] = Array(
    VID_V           -> List(N,N,N, Src1_N,    N, Src2_N,   N, Dest_SEW,  Sign_X,   IsIdx,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//mask-register logical insts
  def mbit: Array[(BitPat, List[BitPat])] = Array(
    VMANDNOT_MM     -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMAND_MM        -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMOR_MM         -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMXOR_MM        -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMORNOT_MM      -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMNAND_MM       -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMNOR_MM        -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMXNOR_MM       -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//integer scalar move insts 
  def mv: Array[(BitPat, List[BitPat])] = Array(
    VMV_X_S         -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_XLEN, Sign_X,   IsMv,      AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VMV_S_X         -> List(Y,N,N, Src1_N,    N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsMv,      AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//floating-point scalar move insts
  def fmv: Array[(BitPat, List[BitPat])] = Array(
    VFMV_F_S        -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_FLEN, Sign_X,   IsFMv,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFMV_S_F        -> List(Y,N,N, Src1_N,    N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMv,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//slide insts
  def slide: Array[(BitPat, List[BitPat])] = Array(
    VSLIDEUP_VX     -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDEUP_VI     -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDEDOWN_VX   -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDEDOWN_VI   -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDE1UP_VX    -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VSLIDE1DOWN_VX  -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSLIDE1UP_VF   -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VFSLIDE1DOWN_VF -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//register gather insts
  def gather: Array[(BitPat, List[BitPat])] = Array(
    VRGATHER_VV     -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VRGATHEREI16_VV -> List(N,N,N, Src1_SEW,  N, Src2_16b, N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VRGATHER_VX     -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X),
    VRGATHER_VI     -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//compress elements inst
  def compress: Array[(BitPat, List[BitPat])] = Array(
    VCOMPRESS_VM    -> List(Y,N,N, Src1_SEW,  N, Src2_1b,  N, Dest_SEW,  Sign_X,   IsZip,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )
//copy registers inst
  def copy: Array[(BitPat, List[BitPat])] = Array(
    VMVR_V          -> List(Y,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsCopy,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X)
  )



  val MUL = MULDIV || MULADD || SATMUL
  val insns = csr ++ al ++ 
              (if(MERGE)    merge    else Nil) ++ 
              (if(MUL)      mul      else Nil) ++ 
              (if(MULADD)   madd     else Nil) ++ 
              (if(SATADD)   sadd     else Nil) ++ 
              (if(AVERADD)  aadd     else Nil) ++ 
              (if(SATMUL)   smul     else Nil) ++ 
              (if(SCALESR)  ssr      else Nil) ++ 
              (if(NCLIP)    nclip    else Nil) ++ 
              (if(FMA)      fma      else Nil) ++ 
              (if(FCVT)     fcvt     else Nil) ++ 
              (if(FCMP)     fcmp     else Nil) ++ 
              (if(FMINMAX)  fminmax  else Nil) ++ 
              (if(FSGNJ)    fsgnj    else Nil) ++ 
              (if(FCLASS)   fclass   else Nil) ++ 
              (if(FMERGE)   fmerge   else Nil) ++ 
              (if(FDIVSQRT) fdiv     else Nil) ++ 
              (if(FRSQRT)   frsqrt   else Nil) ++ 
              (if(FRECE)    frece    else Nil) ++ 
              (if(RED)      red      else Nil) ++ 
              (if(FRED)     fred     else Nil) ++ 
              (if(POPC)     popc     else Nil) ++ 
              (if(FIRST)    first    else Nil) ++ 
              (if(MINDEX)   mindex   else Nil) ++ 
              (if(IOTA)     iota     else Nil) ++ 
              (if(INDEX)    index    else Nil) ++ 
              (if(MBITWISE) mbit     else Nil) ++ 
              (if(MV)       mv       else Nil) ++ 
              (if(FMV)      fmv      else Nil) ++ 
              (if(SLIDE)    slide    else Nil) ++ 
              (if(GATHER)   gather   else Nil) ++ 
              (if(COMPRESS) compress else Nil) ++ 
              (if(COPY)     copy     else Nil)
 
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.isUnmasked, s.swap12, s.swap23, 
                 s.src1EEW, s.isSrc1t2SEW, 
                 s.src2EEW, s.isSrc2t2SEW, 
                 s.destEEW, 
                 s.sign, 
                 s.majFun, 
                 s.addFun, s.shiftDir, s.mulFun, 
                 s.isRND, s.isSAT, 
                 s.wflags, 
                 s.fmaFun)

  sigs zip decoder map { case(s,d) => s := d }

  s.funct6     := io.inst(31,26)
  s.vm         := io.inst(25)
  s.src2Field  := io.inst(24,20)
  s.src1Field  := io.inst(19,15)
  s.widthField := io.inst(14,12)
  s.dstField   := io.inst(11,7)
  s.isCSRInst  := io.inst(6,0) === "b1010111".U(7.W) && io.inst(14,12) === "b111".U(3.W)
  s.isALInst   := io.inst(6,0) === "b1010111".U(7.W) && io.inst(14,12) =/= "b111".U(3.W)
  s.isLdInst   := io.inst(6,0) === "b0000111".U(7.W)
  s.isStInst   := io.inst(6,0) === "b0100111".U(7.W)
  s.isAMOInst  := io.inst(6,0) === "b0101111".U(7.W)
}
