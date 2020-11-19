// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VConsts.scala
*       Author          :       liangzh
*       Revision        :       2020/10/28
*       Company         :       UC TECH IP
*       Description     :       define constants for VPU
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

object VPUIOConstants {
  val XRM_SZ  = 2
  val XSAT_SZ = 1
  val FRM_SZ = 3
  val FFLAGS_SZ = 5
}

trait VConsts {
//////////////////////control signals encoding pattern//////////////////////
  //for bool encoding pattern
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  //for source1 type encoding pattern
  val Src1EEW_SZ = 3
  def Src1_X     = BitPat("b???")
  def Src1_SEW   = "b000".U(Src1EEW_SZ.W)
  def Src1_2SEW  = "b001".U(Src1EEW_SZ.W)
  def Src1_SEWf8 = "b010".U(Src1EEW_SZ.W)
  def Src1_SEWf4 = "b011".U(Src1EEW_SZ.W)
  def Src1_SEWf2 = "b100".U(Src1EEW_SZ.W)
  def Src1_1b    = "b101".U(Src1EEW_SZ.W)
  def Src1_N     = "b110".U(Src1EEW_SZ.W)
  //for source2 type encoding pattern
  val Src2EEW_SZ = 3
  def Src2_X    = BitPat("b???")
  def Src2_SEW  = "b000".U(Src2EEW_SZ.W)
  def Src2_2SEW = "b001".U(Src2EEW_SZ.W)
  def Src2_XLEN = "b010".U(Src2EEW_SZ.W)
  def Src2_FLEN = "b010".U(Src2EEW_SZ.W)
  def Src2_5b   = "b011".U(Src2EEW_SZ.W)
  def Src2_1b   = "b100".U(Src2EEW_SZ.W)
  def Src2_16b  = "b101".U(Src2EEW_SZ.W)
  def Src2_N    = "b110".U(Src2EEW_SZ.W)
  //for dest type encoding pattern
  val DestEEW_SZ = 2
  def Dest_X    = BitPat("b??")
  def Dest_SEW  = "b00".U(DestEEW_SZ.W)
  def Dest_2SEW = "b01".U(DestEEW_SZ.W)
  def Dest_XLEN = "b10".U(DestEEW_SZ.W)
  def Dest_FLEN = "b10".U(DestEEW_SZ.W)
  def Dest_1b   = "b11".U(DestEEW_SZ.W)
  //for sign encoding pattern
  def Sign_X   = BitPat("b?")
  def Signed   = BitPat("b0")
  def Unsigned = BitPat("b1")
  //for vector major function encoding pattern
  val MajFun_SZ = 6
  def MajFun_X  = BitPat("b??????")
  def IsCSR     = "b000000".U(MajFun_SZ.W)
  def IsAdd     = "b000001".U(MajFun_SZ.W)
  def IsExt     = "b000010".U(MajFun_SZ.W)
  def IsBit     = "b000011".U(MajFun_SZ.W)
  def IsShift   = "b000100".U(MajFun_SZ.W)
  def IsCmp     = "b000101".U(MajFun_SZ.W)
  def IsMinMax  = "b000110".U(MajFun_SZ.W)

  def IsMerge   = "b000111".U(MajFun_SZ.W)
  def IsMulDiv  = "b001000".U(MajFun_SZ.W)
  def IsMulAdd  = "b001001".U(MajFun_SZ.W)

  def IsFMA     = "b001010".U(MajFun_SZ.W)
  def IsFCvt    = "b001011".U(MajFun_SZ.W)
  def IsFCmp    = "b001100".U(MajFun_SZ.W)
  def IsFMinMax = "b001101".U(MajFun_SZ.W)
  def IsFSgnJ   = "b001110".U(MajFun_SZ.W)
  def IsFClass  = "b001111".U(MajFun_SZ.W)
  def IsFMerge  = "b010000".U(MajFun_SZ.W)
  def IsFDiv    = "b010001".U(MajFun_SZ.W)
  def IsFRSqrt  = "b010010".U(MajFun_SZ.W)
  def IsFRece   = "b010011".U(MajFun_SZ.W)

  def IsRed     = "b010100".U(MajFun_SZ.W)
  def IsFRed    = "b010101".U(MajFun_SZ.W)

  def IsPopc    = "b010110".U(MajFun_SZ.W)
  def IsFirst   = "b010111".U(MajFun_SZ.W)
  def IsMIdx    = "b011000".U(MajFun_SZ.W)
  def IsIota    = "b011001".U(MajFun_SZ.W)
  def IsIdx     = "b011010".U(MajFun_SZ.W)
  def IsMBit    = "b011011".U(MajFun_SZ.W)

  def IsMv      = "b011100".U(MajFun_SZ.W)
  def IsFMv     = "b011101".U(MajFun_SZ.W)
  def IsSlide   = "b011110".U(MajFun_SZ.W)
  def IsSlide1  = "b011111".U(MajFun_SZ.W)
  def IsGather  = "b100000".U(MajFun_SZ.W)
  def IsZip     = "b100001".U(MajFun_SZ.W)
  def IsCopy    = "b100010".U(MajFun_SZ.W)
  //for adder module function encoding pattern
  val AddFun_SZ = 2
  def AddFun_X   = BitPat("b??")
  def AddFun_Add = "b00".U(AddFun_SZ.W)
  def AddFun_Sub = "b01".U(AddFun_SZ.W)
  def AddFun_Adc = "b10".U(AddFun_SZ.W)
  def AddFun_Sbc = "b11".U(AddFun_SZ.W)
  //for shift direction encoding pattern
  def Shift_X    = BitPat("b?")
  def ShiftLeft  = BitPat("b0")
  def ShiftRight = BitPat("b1")
  //for muldiv module function encoding pattern
  val MulFun_SZ = 4
  def MulFun_X      = BitPat("b????")
  def MulFun_Mul    = "b0000".U(MulFun_SZ.W)
  def MulFun_MulH   = "b0001".U(MulFun_SZ.W)
  def MulFun_Div    = "b0100".U(MulFun_SZ.W)
  def MulFun_DivU   = "b0101".U(MulFun_SZ.W)
  def MulFun_Rem    = "b0110".U(MulFun_SZ.W)
  def MulFun_RemU   = "b0111".U(MulFun_SZ.W)
  def MulFun_MulHSU = "b1100".U(MulFun_SZ.W)
  def MulFun_MulHU  = "b1110".U(MulFun_SZ.W)
  //for rounding or not
  def RND_N = BitPat("b0")
  def RND_Y = BitPat("b1")
  //for saturate or not
  def SAT_N = BitPat("b0")
  def SAT_Y = BitPat("b1")
  //for floating-point fma module function encoding pattern
  val FMAFun_SZ = 3
  def FMAFun_X     = BitPat("b???")
  def FMAFun_Add   = "b000".U(FMAFun_SZ.W)
  def FMAFun_Sub   = "b001".U(FMAFun_SZ.W)
  def FMAFun_Mul   = "b011".U(FMAFun_SZ.W)
  def FMAFun_MAcc  = "b100".U(FMAFun_SZ.W)
  def FMAFun_NMAcc = "b101".U(FMAFun_SZ.W)
  def FMAFun_MSac  = "b110".U(FMAFun_SZ.W)
  def FMAFun_NMSac = "b111".U(FMAFun_SZ.W)
//////////////////////control signals encoding pattern//////////////////////

/////////////////////////inst field encoding pattern////////////////////////
  //for compare module function encoding pattern
  val CmpFun_SZ = 3
  def CmpFun_X   = BitPat("b???")
  def CmpFun_EQ  = "b000".U(CmpFun_SZ.W)
  def CmpFun_NE  = "b001".U(CmpFun_SZ.W)
  def CmpFun_LTU = "b010".U(CmpFun_SZ.W)
  def CmpFun_LT  = "b011".U(CmpFun_SZ.W)
  def CmpFun_LEU = "b100".U(CmpFun_SZ.W)
  def CmpFun_LE  = "b101".U(CmpFun_SZ.W)
  def CmpFun_GTU = "b110".U(CmpFun_SZ.W)
  def CmpFun_GT  = "b111".U(CmpFun_SZ.W)
  //for bitwise module function encoding pattern
  val BitFun_SZ = 3
  def BitFun_AndNot = "b000".U(BitFun_SZ.W)
  def BitFun_And    = "b001".U(BitFun_SZ.W)
  def BitFun_Or     = "b010".U(BitFun_SZ.W)
  def BitFun_Xor    = "b011".U(BitFun_SZ.W)
  def BitFun_OrNot  = "b100".U(BitFun_SZ.W)
  def BitFun_Nand   = "b101".U(BitFun_SZ.W)
  def BitFun_Nor    = "b110".U(BitFun_SZ.W)
  def BitFun_Xnor   = "b111".U(BitFun_SZ.W)
  //for min index module function pattern
  val MinIdxFun_SZ = 2
  def MinIdxFun_SBF = "b01".U(MinIdxFun_SZ.W)
  def MinIdxFun_SOF = "b10".U(MinIdxFun_SZ.W)
  def MinIdxFun_SIF = "b11".U(MinIdxFun_SZ.W)
  //for reduction module function encoding pattern
  val RedFun_SZ = 5
  def RedFun_Sum    = "b00000".U(RedFun_SZ.W)
  def RedFun_And    = "b00001".U(RedFun_SZ.W)
  def RedFun_Or     = "b00010".U(RedFun_SZ.W)
  def RedFun_Xor    = "b00011".U(RedFun_SZ.W)
  def RedFun_Minu   = "b00100".U(RedFun_SZ.W)
  def RedFun_Min    = "b00101".U(RedFun_SZ.W)
  def RedFun_Maxu   = "b00110".U(RedFun_SZ.W)
  def RedFun_Max    = "b00111".U(RedFun_SZ.W)
  def RedFun_WSumu  = "b10000".U(RedFun_SZ.W)
  def RedFun_WSum   = "b10001".U(RedFun_SZ.W)
  def RedFun_FSum   = "b00001".U(RedFun_SZ.W)
  def RedFun_FOSum  = "b00011".U(RedFun_SZ.W)
  def RedFun_FMin   = "b00101".U(RedFun_SZ.W)
  def RedFun_FMax   = "b00111".U(RedFun_SZ.W)
  def RedFun_FWSum  = "b10001".U(RedFun_SZ.W)
  def RedFun_FWOSum = "b10011".U(RedFun_SZ.W)
  //for slide module function encoding pattern
  val SlideFun_SZ = 1
  def SlideFun_Up  = "b0".U(SlideFun_SZ.W)
  def SlideFun_Dn  = "b1".U(SlideFun_SZ.W)
  //for floating-point signinject module function pattern
  val FSgnJFun_SZ = 2
  def FSgnJFun_Inj = "b00".U(FSgnJFun_SZ.W)
  def FSgnJFun_Not = "b01".U(FSgnJFun_SZ.W)
  def FSgnJFun_Xor = "b10".U(FSgnJFun_SZ.W)
  //for floating-point divsqrt module function pattern
  val FDivFun_SZ = 1
  def FDivFun_Div  = "b0".U(FDivFun_SZ.W)
  def FDivFun_Sqrt = "b1".U(FDivFun_SZ.W)
  //for floating-point fcvt module function encoding pattern
  val FCvtFun_SZ = 5
  def FCvtFun_F2XU     = "b00000".U(FCvtFun_SZ.W)
  def FCvtFun_F2X      = "b00001".U(FCvtFun_SZ.W)
  def FCvtFun_XU2F     = "b00010".U(FCvtFun_SZ.W)
  def FCvtFun_X2F      = "b00011".U(FCvtFun_SZ.W)
  def FCvtFun_F2XUrtz  = "b00110".U(FCvtFun_SZ.W)
  def FCvtFun_F2Xrtz   = "b00111".U(FCvtFun_SZ.W)

  def FCvtFun_F2WXU    = "b01000".U(FCvtFun_SZ.W)
  def FCvtFun_F2WX     = "b01001".U(FCvtFun_SZ.W)
  def FCvtFun_XU2WF    = "b01010".U(FCvtFun_SZ.W)
  def FCvtFun_X2WF     = "b01011".U(FCvtFun_SZ.W)
  def FCvtFun_F2WF     = "b01100".U(FCvtFun_SZ.W)
  def FCvtFun_F2WXUrtz = "b01110".U(FCvtFun_SZ.W)
  def FCvtFun_F2WXrtz  = "b01111".U(FCvtFun_SZ.W)
  
  def FCvtFun_F2NXU    = "b10000".U(FCvtFun_SZ.W)
  def FCvtFun_F2NX     = "b10001".U(FCvtFun_SZ.W)
  def FCvtFun_XU2NF    = "b10010".U(FCvtFun_SZ.W)
  def FCvtFun_X2NF     = "b10011".U(FCvtFun_SZ.W)
  def FCvtFun_F2NF     = "b10100".U(FCvtFun_SZ.W)
  def FCvtFun_F2NFrod  = "b10101".U(FCvtFun_SZ.W)
  def FCvtFun_F2NXUrtz = "b10110".U(FCvtFun_SZ.W)
  def FCvtFun_F2NXrtz  = "b10111".U(FCvtFun_SZ.W)
  //for segment load&store and copy pattern
  val NFIELDS_SZ = 3
  def OneField = "b000".U(NFIELDS_SZ.W)
  def TwoField = "b001".U(NFIELDS_SZ.W)
  def ThrField = "b010".U(NFIELDS_SZ.W)
  def FouField = "b011".U(NFIELDS_SZ.W)
  def FivField = "b100".U(NFIELDS_SZ.W)
  def SixField = "b101".U(NFIELDS_SZ.W)
  def SevField = "b110".U(NFIELDS_SZ.W)
  def EigField = "b111".U(NFIELDS_SZ.W)
  //for load&store addressing mode pattern
  val AddrMode_SZ = 2
  def UStride  = "b00".U(AddrMode_SZ.W)
  def Strided  = "b10".U(AddrMode_SZ.W)
  def Indexed  = "b11".U(AddrMode_SZ.W)
  //for unit-stride load&store additional addressing mode pattern
  def UStrideOnly  = "b00000".U(5.W)
  def UStrideWhole = "b01000".U(5.W)
  def UStrideLFF   = "b10000".U(5.W)
  //for load&store and AMO elements' width
  val LdStEEW_SZ = 4
  def EEW8    = "b0000".U(LdStEEW_SZ.W)
  def EEW16   = "b0101".U(LdStEEW_SZ.W)
  def EEW32   = "b0110".U(LdStEEW_SZ.W)
  def EEW64   = "b0111".U(LdStEEW_SZ.W)
  def EEW128  = "b1000".U(LdStEEW_SZ.W)
  def EEW256  = "b1101".U(LdStEEW_SZ.W)
  def EEW512  = "b1110".U(LdStEEW_SZ.W)
  def EEW1024 = "b1111".U(LdStEEW_SZ.W)
  //for AMO operations pattern
  val AMOOP_SZ = 5
  def AMOswap = "b00001".U(AMOOP_SZ.W)
  def AMOadd  = "b00000".U(AMOOP_SZ.W)
  def AMOxor  = "b00100".U(AMOOP_SZ.W)
  def AMOand  = "b01100".U(AMOOP_SZ.W)
  def AMOor   = "b01000".U(AMOOP_SZ.W)
  def AMOmin  = "b10000".U(AMOOP_SZ.W)
  def AMOmax  = "b10100".U(AMOOP_SZ.W)
  def AMOminu = "b11000".U(AMOOP_SZ.W)
  def AMOmaxu = "b11100".U(AMOOP_SZ.W)
  //for AMO insts write vd or not
  def WriteVd = "b"

/////////////////////////inst field encoding pattern////////////////////////

////////////////////////////CSR encoding pattern////////////////////////////
  //vector csr io size
  val VEDIV_SZ = 2
  val VMA_SZ   = 1
  val VTA_SZ   = 1
  val VSEW_SZ  = 3
  val VLMUL_SZ = 3
  //for elements divide pattern
  def EDIV1   = "b00".U(VEDIV_SZ.W)
  def EDIV2   = "b01".U(VEDIV_SZ.W)
  def EDIV4   = "b10".U(VEDIV_SZ.W)
  def EDIV8   = "b11".U(VEDIV_SZ.W)
  //for handling inactive elements
  def MaskU   = "b0".U(VMA_SZ.W) //inactive elements undisturbed
  def MaskA   = "b1".U(VMA_SZ.W) //inactive elements agnostic
  //for handling tail elements
  def TailU   = "b0".U(VTA_SZ.W) //tail elements undisturbed
  def TailA   = "b1".U(VTA_SZ.W) //tail elements agnostic
  //for elements' width pattern
  def SEW8    = "b000".U(VSEW_SZ.W)
  def SEW16   = "b001".U(VSEW_SZ.W)
  def SEW32   = "b010".U(VSEW_SZ.W)
  def SEW64   = "b011".U(VSEW_SZ.W)
  def SEW128  = "b100".U(VSEW_SZ.W)
  def SEW256  = "b101".U(VSEW_SZ.W)
  def SEW512  = "b110".U(VSEW_SZ.W)
  def SEW1024 = "b111".U(VSEW_SZ.W)
  //for register set number pattern
  def LMUL1f8 = "b101".U(VLMUL_SZ.W)
  def LMUL1f4 = "b110".U(VLMUL_SZ.W)
  def LMUL1f2 = "b111".U(VLMUL_SZ.W)
  def LMUL1   = "b000".U(VLMUL_SZ.W)
  def LMUL2   = "b001".U(VLMUL_SZ.W)
  def LMUL4   = "b010".U(VLMUL_SZ.W)
  def LMUL8   = "b011".U(VLMUL_SZ.W)
  //for vxrm pattern
  def RNU = "b00".U(VPUIOConstants.XRM_SZ.W)
  def RNE = "b01".U(VPUIOConstants.XRM_SZ.W)
  def RDN = "b10".U(VPUIOConstants.XRM_SZ.W)
  def ROD = "b11".U(VPUIOConstants.XRM_SZ.W)
////////////////////////////CSR encoding pattern////////////////////////////

}
