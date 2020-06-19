// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VConsts.scala
*       Author          :       liangzh
*       Revision        :       2019/04/26
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       define constants for VPU
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

object VPUConstants {
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
  val Src1Typ_SZ   = 2
  def Src1_X    = BitPat("b??")
  def Src1_Vs   = "b00".U(Src1Typ_SZ.W)
  def Src1_Xs   = "b01".U(Src1Typ_SZ.W)
  def Src1_Fs   = "b01".U(Src1Typ_SZ.W)
  def Src1_Imm  = "b10".U(Src1Typ_SZ.W)
  def Src1_Code = "b11".U(Src1Typ_SZ.W)
  //for vector major function encoding pattern
  val MajFun_SZ = 6
  def MajFun   = BitPat("b??????")
  def IsAdd    = "b000000".U(MajFun_SZ.W)
  def IsBit    = "b000001".U(MajFun_SZ.W)
  def IsShift  = "b000010".U(MajFun_SZ.W)
  def IsCmp    = "b000011".U(MajFun_SZ.W)
  def IsMerge  = "b000100".U(MajFun_SZ.W)
  def IsCSR    = "b000101".U(MajFun_SZ.W)
  def IsLoad   = "b000110".U(MajFun_SZ.W)
  def IsStore  = "b000111".U(MajFun_SZ.W)
  def IsPopc   = "b001000".U(MajFun_SZ.W)
  def IsFirst  = "b001001".U(MajFun_SZ.W)
  def IsMIdx   = "b001010".U(MajFun_SZ.W)
  def IsIota   = "b001011".U(MajFun_SZ.W)
  def IsIdx    = "b001100".U(MajFun_SZ.W)
  def IsMBit   = "b001101".U(MajFun_SZ.W)
  def IsMulDiv = "b001110".U(MajFun_SZ.W)
  def IsMulAdd = "b001111".U(MajFun_SZ.W)
  def IsRed    = "b010000".U(MajFun_SZ.W)
  def IsMv     = "b010001".U(MajFun_SZ.W)
  def IsSlide  = "b010010".U(MajFun_SZ.W)
  def IsSlide1 = "b010011".U(MajFun_SZ.W)
  def IsGather = "b010100".U(MajFun_SZ.W)
  def IsZip    = "b010101".U(MajFun_SZ.W)
  def IsCopy   = "b010110".U(MajFun_SZ.W)
  def IsDot    = "b010111".U(MajFun_SZ.W)
  def IsFMA    = "b011000".U(MajFun_SZ.W)
  def IsFCvt   = "b011001".U(MajFun_SZ.W)
  def IsFCmp   = "b011010".U(MajFun_SZ.W)
  def IsFSgnJ  = "b011011".U(MajFun_SZ.W)
  def IsFClass = "b011100".U(MajFun_SZ.W)
  def IsFMerge = "b011101".U(MajFun_SZ.W)
  def IsFMv    = "b011110".U(MajFun_SZ.W)
  def IsFDiv   = "b011111".U(MajFun_SZ.W)
  def IsFRed   = "b100000".U(MajFun_SZ.W)
  def IsFDot   = "b100001".U(MajFun_SZ.W)
  def IsAMO    = "b100010".U(MajFun_SZ.W)

  //for adder module function encoding pattern
  val AddFun_SZ = 3
  def AddFun_X   = BitPat("b???")
  def AddFun_Add = "b000".U(AddFun_SZ.W)
  def AddFun_Sub = "b001".U(AddFun_SZ.W)
  def AddFun_Rev = "b010".U(AddFun_SZ.W)
  def AddFun_Adc = "b011".U(AddFun_SZ.W)
  def AddFun_Sbc = "b100".U(AddFun_SZ.W)
  //for shift module function encoding pattern
  val ShiftFun_SZ = 2
  def ShiftFun_X   = BitPat("b??")
  def ShiftFun_LL  = "b00".U(ShiftFun_SZ.W)
  def ShiftFun_RL  = "b01".U(ShiftFun_SZ.W)
  def ShiftFun_RA  = "b10".U(ShiftFun_SZ.W)
  //for compare module function encoding pattern
  val CmpFun_SZ = 3
  def CmpFun_X   = BitPat("b???")
  def CmpFun_EQ  = "b000".U(CmpFun_SZ.W)
  def CmpFun_NE  = "b001".U(CmpFun_SZ.W)
  def CmpFun_LT  = "b010".U(CmpFun_SZ.W)
  def CmpFun_LE  = "b011".U(CmpFun_SZ.W)
  def CmpFun_GT  = "b100".U(CmpFun_SZ.W)
  def CmpFun_GE  = "b101".U(CmpFun_SZ.W)
  def CmpFun_Min = "b110".U(CmpFun_SZ.W)
  def CmpFun_Max = "b111".U(CmpFun_SZ.W)
  //for three operand multiply-add order
  def Add_Vs2 = BitPat("b0")
  def Add_Vd  = BitPat("b1")
  //for muldiv module function encoding pattern
  val MulFun_SZ = 4
  def MulFun_X      = BitPat("b????")
  def MulFun_Mul    = "b0000".U(MulFun_SZ.W)
  def MulFun_MulH   = "b0001".U(MulFun_SZ.W)
  def MulFun_MulF   = "b0010".U(MulFun_SZ.W)
  def MulFun_Div    = "b0100".U(MulFun_SZ.W)
  def MulFun_DivU   = "b0101".U(MulFun_SZ.W)
  def MulFun_Rem    = "b0110".U(MulFun_SZ.W)
  def MulFun_RemU   = "b0111".U(MulFun_SZ.W)
  def MulFun_MulFSU = "b1000".U(MulFun_SZ.W)
  def MulFun_MulFUS = "b1001".U(MulFun_SZ.W)
  def MulFun_MulFU  = "b1010".U(MulFun_SZ.W)
  def MulFun_MulHSU = "b1100".U(MulFun_SZ.W)
  def MulFun_MulHU  = "b1110".U(MulFun_SZ.W)
  //for rounding or not
  def RND_N = BitPat("b0")
  def RND_Y = BitPat("b1")
  //for saturate or not
  def SAT_N = BitPat("b0")
  def SAT_Y = BitPat("b1")

  //for float
  def FPH = "b001".U(3.W)
  def FPS = "b010".U(3.W)
  def FPD = "b011".U(3.W)
  def FPQ = "b100".U(3.W)

  val SZ_Q = 128
  val SZ_D = 64
  val SZ_W = 32
  val SZ_H = 16
  val SZ_B = 8

  //for floating-point fma module function encoding pattern
  val FMAFun_SZ = 3
  def FMAFun_X     = BitPat("b???")
  def FMAFun_Add   = "b000".U(FMAFun_SZ.W)
  def FMAFun_Sub   = "b001".U(FMAFun_SZ.W)
  def FMAFun_RSub  = "b010".U(FMAFun_SZ.W)
  def FMAFun_Mul   = "b011".U(FMAFun_SZ.W)
  def FMAFun_MAdd  = "b100".U(FMAFun_SZ.W)
  def FMAFun_MSub  = "b101".U(FMAFun_SZ.W)
  def FMAFun_NMSub = "b110".U(FMAFun_SZ.W)
  def FMAFun_NMAdd = "b111".U(FMAFun_SZ.W)
  //for floating-point fcvt module function encoding pattern
  val FCvtFun_SZ = 5
  def FCvtFun_F2XU  = "b00000".U(FCvtFun_SZ.W)
  def FCvtFun_F2X   = "b00001".U(FCvtFun_SZ.W)
  def FCvtFun_XU2F  = "b00010".U(FCvtFun_SZ.W)
  def FCvtFun_X2F   = "b00011".U(FCvtFun_SZ.W)
  def FCvtFun_F2WXU = "b01000".U(FCvtFun_SZ.W)
  def FCvtFun_F2WX  = "b01001".U(FCvtFun_SZ.W)
  def FCvtFun_XU2WF = "b01010".U(FCvtFun_SZ.W)
  def FCvtFun_X2WF  = "b01011".U(FCvtFun_SZ.W)
  def FCvtFun_F2WF  = "b01100".U(FCvtFun_SZ.W)
  def FCvtFun_F2NXU = "b10000".U(FCvtFun_SZ.W)
  def FCvtFun_F2NX  = "b10001".U(FCvtFun_SZ.W)
  def FCvtFun_XU2NF = "b10010".U(FCvtFun_SZ.W)
  def FCvtFun_X2NF  = "b10011".U(FCvtFun_SZ.W)
  def FCvtFun_F2NF  = "b10100".U(FCvtFun_SZ.W)
  def FCvtFun_F2NFR = "b10101".U(FCvtFun_SZ.W)
//////////////////////control signals encoding pattern//////////////////////

/////////////////////////inst field encoding pattern////////////////////////
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
  val FDivFun_SZ = 2
  def FDivFun_Div  = "b00".U(FDivFun_SZ.W)
  def FDivFun_RDiv = "b01".U(FDivFun_SZ.W)
  def FDivFun_Sqrt = "b11".U(FDivFun_SZ.W)
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
  //for load&store and AMO memory elements' width
  val LdStWidth_SZ = 2
  def LdStByte = "b00".U(LdStWidth_SZ.W)
  def LdStHalf = "b01".U(LdStWidth_SZ.W)
  def LdStWord = "b10".U(LdStWidth_SZ.W)
  def LdStVSEW = "b11".U(LdStWidth_SZ.W)
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

  def M_XA_SWAP = "b00100".U(AMOOP_SZ.W)
  def M_XA_ADD  = "b01000".U(AMOOP_SZ.W)
  def M_XA_XOR  = "b01001".U(AMOOP_SZ.W)
  def M_XA_OR   = "b01010".U(AMOOP_SZ.W)
  def M_XA_AND  = "b01011".U(AMOOP_SZ.W)
  def M_XA_MIN  = "b01100".U(AMOOP_SZ.W)
  def M_XA_MAX  = "b01101".U(AMOOP_SZ.W)
  def M_XA_MINU = "b01110".U(AMOOP_SZ.W)
  def M_XA_MAXU = "b01111".U(AMOOP_SZ.W)
/////////////////////////inst field encoding pattern////////////////////////

////////////////////////////CSR encoding pattern////////////////////////////
  //vector csr io size
  val VEDIV_SZ = 2
  val VSEW_SZ = 3
  val VLMUL_SZ = 2
  //for elements divide pattern
  def NoDiv      = "b00".U(VEDIV_SZ.W)
  def TwoDiv     = "b01".U(VEDIV_SZ.W)
  def FourDiv    = "b10".U(VEDIV_SZ.W)
  def EightDiv   = "b11".U(VEDIV_SZ.W)
  //for elements' width pattern
  def ByteWidth  = "b000".U(VSEW_SZ.W)  //Byte                            8    bits
  def HWordWidth = "b001".U(VSEW_SZ.W)  //HWord = Half Word,              16   bits
  def WordWidth  = "b010".U(VSEW_SZ.W)  //Word                            32   bits
  def DWordWidth = "b011".U(VSEW_SZ.W)  //DWord = Double Word,            64   bits
  def QWordWidth = "b100".U(VSEW_SZ.W)  //QWord = Quadruple Word,         128  bits
  def OWordWidth = "b101".U(VSEW_SZ.W)  //OWord = Octuple Word,           256  bits
  def SWordWidth = "b110".U(VSEW_SZ.W)  //SWord = Sixteen times Words,    512  bits
  def TWordWidth = "b111".U(VSEW_SZ.W)  //TWord = Thirty-two times Words, 1024 bits
  //for register set number pattern
  def SingReg = "b00".U(VLMUL_SZ.W)
  def DoubReg = "b01".U(VLMUL_SZ.W)
  def QuadReg = "b10".U(VLMUL_SZ.W)
  def OctuReg = "b11".U(VLMUL_SZ.W)
  //for sub-element divide
  def SingSub = "b00".U(VEDIV_SZ.W)
  def DoubSub = "b01".U(VEDIV_SZ.W)
  def QuadSub = "b10".U(VEDIV_SZ.W)
  def OctuSub = "b11".U(VEDIV_SZ.W)
  //for vxrm pattern
  def RNU = "b00".U(VPUConstants.XRM_SZ.W)
  def RNE = "b01".U(VPUConstants.XRM_SZ.W)
  def RDN = "b10".U(VPUConstants.XRM_SZ.W)
  def ROD = "b11".U(VPUConstants.XRM_SZ.W)
////////////////////////////CSR encoding pattern////////////////////////////

}
