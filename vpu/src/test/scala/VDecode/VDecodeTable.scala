// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

import chisel3.iotesters._

class VDecodeTable(c: VDecode) extends PeekPokeTester(c) {
  def X             = 9999
  def Y             = 1
  def N             = 0
  def Src1_SEW      = 0
  def Src1_2SEW     = 1
  def Src1_SEWf8    = 2
  def Src1_SEWf4    = 3
  def Src1_SEWf2    = 4
  def Src1_1b       = 5
  def Src1_N        = 6
  def Src2_SEW      = 0
  def Src2_2SEW     = 1
  def Src2_XLEN     = 2
  def Src2_FLEN     = 2
  def Src2_5b       = 3
  def Src2_1b       = 4
  def Src2_16b      = 5
  def Src2_N        = 6
  def Dest_SEW      = 0
  def Dest_2SEW     = 1
  def Dest_XLEN     = 2
  def Dest_FLEN     = 2
  def Dest_1b       = 3
  def Sign_X        = 9999
  def Signed        = 0
  def Unsigned      = 1
  def IsCSR         = 0
  def IsAdd         = 1
  def IsExt         = 2
  def IsBit         = 3
  def IsShift       = 4
  def IsCmp         = 5
  def IsMinMax      = 6
  def IsMerge       = 7
  def IsMulDiv      = 8
  def IsMulAdd      = 9
  def IsFMA         = 10
  def IsFCvt        = 11
  def IsFCmp        = 12
  def IsFMinMax     = 13
  def IsFSgnJ       = 14
  def IsFClass      = 15
  def IsFMerge      = 16
  def IsFDiv        = 17
  def IsFRSqrt      = 18
  def IsFRece       = 19
  def IsRed         = 20
  def IsFRed        = 21
  def IsPopc        = 22
  def IsFirst       = 23
  def IsMIdx        = 24
  def IsIota        = 25
  def IsIdx         = 26
  def IsMBit        = 27
  def IsMv          = 28
  def IsFMv         = 29
  def IsSlide       = 30
  def IsSlide1      = 31
  def IsGather      = 32
  def IsZip         = 33
  def IsCopy        = 34
  def AddFun_X      = 9999
  def AddFun_Add    = 0
  def AddFun_Sub    = 1
  def AddFun_Adc    = 2
  def AddFun_Sbc    = 3
  def Shift_X       = 9999
  def ShiftLeft     = 0
  def ShiftRight    = 1
  def MulFun_X      = 9999
  def MulFun_Mul    = 0
  def MulFun_MulH   = 1
  def MulFun_Div    = 4
  def MulFun_DivU   = 5
  def MulFun_Rem    = 6
  def MulFun_RemU   = 7
  def MulFun_MulHSU = 12
  def MulFun_MulHU  = 14
  def RND_N         = 0
  def RND_Y         = 1
  def SAT_N         = 0
  def SAT_Y         = 1
  def wflags_Y      = 1
  def wflags_N      = 0
  def FMAFun_X      = 9999
  def FMAFun_Add    = 0
  def FMAFun_Sub    = 1
  def FMAFun_Mul    = 3
  def FMAFun_MAcc   = 4
  def FMAFun_NMAcc  = 5
  def FMAFun_MSac   = 6
  def FMAFun_NMSac  = 7

  def ExpectDecode(decodeTable: List[Int]) = {
    expect(c.io.sigs.isUnmasked,  decodeTable(0))
    expect(c.io.sigs.swap12,      decodeTable(1))
    expect(c.io.sigs.swap23,      decodeTable(2))
    expect(c.io.sigs.src1EEW,     decodeTable(3))
    expect(c.io.sigs.src2EEW,     decodeTable(5))
    expect(c.io.sigs.destEEW,     decodeTable(7))
    expect(c.io.sigs.majFun,      decodeTable(9))
    expect(c.io.sigs.wflags,      decodeTable(15))
    expect(c.io.sigs.isCSRInst,   decodeTable(17))
    expect(c.io.sigs.isALInst,    decodeTable(18))
    expect(c.io.sigs.isLdInst,    0)
    expect(c.io.sigs.isStInst,    0)
    expect(c.io.sigs.isAMOInst,   0)
    if(decodeTable(4) != 9999)
      expect(c.io.sigs.isSrc1t2SEW, decodeTable(4))
    if(decodeTable(6) != 9999)
      expect(c.io.sigs.isSrc2t2SEW, decodeTable(6))
    if(decodeTable(8) != 9999)
      expect(c.io.sigs.sign,        decodeTable(8))
    if(decodeTable(10) != 9999)
      expect(c.io.sigs.addFun,      decodeTable(10))
    if(decodeTable(11) != 9999)
      expect(c.io.sigs.shiftDir,    decodeTable(11))
    if(decodeTable(12) != 9999)
      expect(c.io.sigs.mulFun,      decodeTable(12))
    if(decodeTable(13) != 9999)
      expect(c.io.sigs.isRND,       decodeTable(13))
    if(decodeTable(14) != 9999)
      expect(c.io.sigs.isSAT,       decodeTable(14))
    if(decodeTable(16) != 9999)
      expect(c.io.sigs.fmaFun,      decodeTable(16))
  }
  val csr_decode = Map(
    "VSETVLI          " -> List(N,N,N, Src1_N,    X, Src2_XLEN,X, Dest_XLEN, Sign_X,   IsCSR,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     Y,N),
    "VSETVL           " -> List(N,N,N, Src1_N,    X, Src2_XLEN,X, Dest_XLEN, Sign_X,   IsCSR,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     Y,N)
  )
//integer arithmetic and logic insts
  val al_decode = Map(
    //integer add sub insts
    "VADD_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VADD_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VADD_VI          " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSUB_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSUB_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VRSUB_VX         " -> List(N,Y,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VRSUB_VI         " -> List(N,Y,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADDU_VV        " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADDU_VX        " -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADD_VV         " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADD_VX         " -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUBU_VV        " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUBU_VX        " -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUB_VV         " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUB_VX         " -> List(N,N,N, Src1_SEW,  Y, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADDU_WV        " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADDU_WX        " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADD_WV         " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWADD_WX         " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUBU_WV        " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUBU_WX        " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUB_WV         " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWSUB_WX         " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,Y, Dest_2SEW, Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    //integer add-with-carry and sub-with-borrow insts
    "VADC_VVM         " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VADC_VXM         " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VADC_VIM         " -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADC_VVM        " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADC_VXM        " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADC_VIM        " -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsAdd,     AddFun_Adc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADC_VV         " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADC_VX         " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADC_VI         " -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSBC_VVM         " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSBC_VXM         " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMSBC_VVM        " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMSBC_VXM        " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sbc, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMSBC_VV         " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMSBC_VX         " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    //integer extension insts
    "VZEXT_VF8        " -> List(N,N,N, Src1_SEWf8,N, Src2_N,   N, Dest_SEW,  Unsigned, IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSEXT_VF8        " -> List(N,N,N, Src1_SEWf8,N, Src2_N,   N, Dest_SEW,  Signed,   IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VZEXT_VF4        " -> List(N,N,N, Src1_SEWf4,N, Src2_N,   N, Dest_SEW,  Unsigned, IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSEXT_VF4        " -> List(N,N,N, Src1_SEWf4,N, Src2_N,   N, Dest_SEW,  Signed,   IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VZEXT_VF2        " -> List(N,N,N, Src1_SEWf2,N, Src2_N,   N, Dest_SEW,  Unsigned, IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSEXT_VF2        " -> List(N,N,N, Src1_SEWf2,N, Src2_N,   N, Dest_SEW,  Signed,   IsExt,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    //integer bitwise logical insts
    "VAND_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VAND_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VAND_VI          " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VOR_VV           " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VOR_VX           " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VOR_VI           " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VXOR_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VXOR_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VXOR_VI          " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsBit,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    //integer shift insts
    "VSLL_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftLeft,  MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSLL_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftLeft,  MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSLL_VI          " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftLeft,  MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSRL_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSRL_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSRL_VI          " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSRA_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSRA_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VSRA_VI          " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    //integer narrowing right shift insts
    "VNSRL_WV         " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNSRL_WX         " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNSRL_WI         " -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNSRA_WV         " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNSRA_WX         " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNSRA_WI         " -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_N,SAT_N, N, FMAFun_X,     N,Y),
    //integer comparison insts
    "VMSEQ_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSEQ_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSEQ_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSNE_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSNE_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSNE_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Sign_X,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLTU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLTU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLT_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLT_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLEU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLEU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLEU_VI        " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLE_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLE_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSLE_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSGTU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSGTU_VI        " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Unsigned, IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSGT_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSGT_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_1b,   Signed,   IsCmp,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    //integer min-max insts
    "VMINU_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMINU_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMIN_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMIN_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMAXU_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMAXU_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMAX_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMAX_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMinMax,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//integer merge insts
  val merge_decode = Map(
    "VMERGE_VVM       " -> List(Y,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsMerge,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMERGE_VXM       " -> List(Y,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsMerge,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMERGE_VIM       " -> List(Y,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Sign_X,   IsMerge,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//integer multiply and divide insts
  val mul_decode = Map(
    "VMUL_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMUL_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMULH_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMULH_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMULHU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMULHU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMULHSU_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMULHSU_VX       " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMUL_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMUL_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMULU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMULU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMULSU_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMULSU_VX       " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VDIV_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Div,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VDIV_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Div,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VDIVU_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_DivU,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VDIVU_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_DivU,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VREM_VV          " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Rem,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VREM_VX          " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_Rem,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VREMU_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_RemU,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VREMU_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsMulDiv,  AddFun_X,   Shift_X,    MulFun_RemU,   RND_N,SAT_N, N, FMAFun_X,     N,Y)
  )
//integer multiply-add insts
  val madd_decode = Map(
    "VMADD_VV         " -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMADD_VX         " -> List(N,N,Y, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNMSUB_VV        " -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNMSUB_VX        " -> List(N,N,Y, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMACC_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VMACC_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNMSAC_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VNMSAC_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulAdd,  AddFun_Sub, Shift_X,    MulFun_Mul,    RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACCU_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACCU_VX       " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHU,  RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACC_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACC_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Signed,   IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulH,   RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACCSU_VV      " -> List(N,Y,N, Src1_SEW,  N, Src2_SEW, N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACCSU_VX      " -> List(N,Y,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y),
    "VWMACCUS_VX      " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_2SEW, Unsigned, IsMulAdd,  AddFun_Add, Shift_X,    MulFun_MulHSU, RND_N,SAT_N, N, FMAFun_X,     N,Y)
  )
//fixed-point saturating add and subtract insts
  val sadd_decode = Map(
    "VSADDU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSADDU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSADDU_VI        " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSADD_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSADD_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSADD_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSSUBU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSSUBU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSSUB_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y),
    "VSSUB_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_N,SAT_Y, N, FMAFun_X,     N,Y)
  )
//fixed-point averaging add and subtract insts
  val aadd_decode = Map(
    "VAADDU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VAADDU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VAADD_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VAADD_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Add, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VASUBU_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VASUBU_VX        " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VASUB_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VASUB_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsAdd,     AddFun_Sub, Shift_X,    MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y)
  )
//fractional mul insts
  val smul_decode = Map(
    "VSMUL_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_Y,SAT_Y, N, FMAFun_X,     N,Y),
    "VSMUL_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsMulDiv,  AddFun_X,   Shift_X,    MulFun_MulH,   RND_Y,SAT_Y, N, FMAFun_X,     N,Y)
  )
//scaling shift insts
  val ssr_decode = Map(
    "VSSRL_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VSSRL_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VSSRL_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VSSRA_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VSSRA_VX         " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y),
    "VSSRA_VI         " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_N, N, FMAFun_X,     N,Y)
  )
//narrowing fixed-point clip insts
  val nclip_decode = Map(
    "VNCLIPU_WV       " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X,     N,Y),
    "VNCLIPU_WX       " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X,     N,Y),
    "VNCLIPU_WI       " -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Unsigned, IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X,     N,Y),
    "VNCLIP_WV        " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X,     N,Y),
    "VNCLIP_WX        " -> List(N,N,N, Src1_2SEW, N, Src2_XLEN,N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X,     N,Y),
    "VNCLIP_WI        " -> List(N,N,N, Src1_2SEW, N, Src2_5b,  N, Dest_SEW,  Signed,   IsShift,   AddFun_X,   ShiftRight, MulFun_X,      RND_Y,SAT_Y, N, FMAFun_X,     N,Y)
  )
//floating-point add and multiply multiply-add insts
  val fma_decode = Map(
    //floating-point add sub insts
    "VFADD_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add,   N,Y),
    "VFADD_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add,   N,Y),
    "VFSUB_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    "VFSUB_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    "VFRSUB_VF        " -> List(N,Y,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    "VFWADD_VV        " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add,   N,Y),
    "VFWADD_VF        " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add,   N,Y),
    "VFWSUB_VV        " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    "VFWSUB_VF        " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    "VFWADD_WV        " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add,   N,Y),
    "VFWADD_WF        " -> List(N,N,N, Src1_2SEW, N, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Add,   N,Y),
    "VFWSUB_WV        " -> List(N,N,N, Src1_2SEW, N, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    "VFWSUB_WF        " -> List(N,N,N, Src1_2SEW, N, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Sub,   N,Y),
    //floating-point multiply insts
    "VFMUL_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul,   N,Y),
    "VFMUL_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul,   N,Y),
    "VFWMUL_VV        " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul,   N,Y),
    "VFWMUL_VF        " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_Mul,   N,Y),
    //floating-point multiply-add insts
    "VFMACC_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc,  N,Y),
    "VFMACC_VF        " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc,  N,Y),
    "VFNMACC_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc, N,Y),
    "VFNMACC_VF       " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc, N,Y),
    "VFMSAC_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac,  N,Y),
    "VFMSAC_VF        " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac,  N,Y),
    "VFNMSAC_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac, N,Y),
    "VFNMSAC_VF       " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac, N,Y),
    "VFMADD_VV        " -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc,  N,Y),
    "VFMADD_VF        " -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc,  N,Y),
    "VFNMADD_VV       " -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc, N,Y),
    "VFNMADD_VF       " -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc, N,Y),
    "VFMSUB_VV        " -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac,  N,Y),
    "VFMSUB_VF        " -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac,  N,Y),
    "VFNMSUB_VV       " -> List(N,N,Y, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac, N,Y),
    "VFNMSUB_VF       " -> List(N,N,Y, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac, N,Y),
    "VFWMACC_VV       " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc,  N,Y),
    "VFWMACC_VF       " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MAcc,  N,Y),
    "VFWNMACC_VV      " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc, N,Y),
    "VFWNMACC_VF      " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMAcc, N,Y),
    "VFWMSAC_VV       " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac,  N,Y),
    "VFWMSAC_VF       " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_MSac,  N,Y),
    "VFWNMSAC_VV      " -> List(N,N,N, Src1_SEW,  Y, Src2_SEW, Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac, N,Y),
    "VFWNMSAC_VF      " -> List(N,N,N, Src1_SEW,  Y, Src2_FLEN,Y, Dest_2SEW, Sign_X,   IsFMA,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_NMSac, N,Y)
  )
//floating-point/integer type-convert insts
  val fcvt_decode = Map(
    "VFCVT_XU_F_V     " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFCVT_X_F_V      " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFCVT_F_XU_V     " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFCVT_F_X_V      " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFCVT_RTZ_XU_F_V " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFCVT_RTZ_X_F_V  " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_XU_F_V    " -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_X_F_V     " -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_F_XU_V    " -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_F_X_V     " -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_F_F_V     " -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_RTZ_XU_F_V" -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWCVT_RTZ_X_F_V " -> List(N,N,N, Src1_SEW,  Y, Src2_N,   N, Dest_2SEW, Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_XU_F_W    " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_X_F_W     " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_F_XU_W    " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_F_X_W     " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_F_F_W     " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_ROD_F_F_W " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_RTZ_XU_F_W" -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFNCVT_RTZ_X_F_W " -> List(N,N,N, Src1_2SEW, N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFCvt,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
  )
//floating-point compare insts
  val fcmp_decode = Map(
    "VMFEQ_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFEQ_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFNE_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFNE_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFLT_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFLT_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFLE_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFLE_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFGT_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VMFGE_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_1b,   Sign_X,   IsFCmp,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y)
  )
//floating-point min-max insts
  val fminmax_decode = Map(
    "VFMIN_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFMIN_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFMAX_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFMAX_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMinMax, AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y)
  )
//floating-point sign-injection insts
  val fsgnj_decode = Map(
    "VFSGNJ_VV        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSGNJ_VF        " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSGNJN_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSGNJN_VF       " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSGNJX_VV       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSGNJX_VF       " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFSgnJ,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//floating-point classify inst
  val fclass_decode = Map(
    "VFCLASS_V        " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFClass,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//floating-point merge inst
  val fmerge_decode = Map(
    "VFMERGE_VFM      " -> List(Y,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMerge,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//floating-point divide and square root insts
  val fdiv_decode = Map(
    //floating-point divide insts
    "VFDIV_VV         " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFDIV_VF         " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFRDIV_VF        " -> List(N,Y,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    //floating-point square root inst
    "VFSQRT_V         " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFDiv,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y)
)
//floating-point reciprocal square root inst
  val frsqrt_decode = Map(
    "VFRSQRTE7_V      " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFRSqrt,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y)
  )
//floating-point reciprocal inst
  val frece_decode = Map(
    "VFRECE7_V        " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsFRece,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y)
  )
//integer reduction insts
  val red_decode = Map(
    "VREDSUM_VS       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDAND_VS       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDOR_VS        " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDXOR_VS       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDMINU_VS      " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDMIN_VS       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDMAXU_VS      " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VREDMAX_VS       " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VWREDSUMU_VS     " -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Unsigned, IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VWREDSUM_VS      " -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Signed,   IsRed,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
  )
//floating-point reduction insts
  val fred_decode = Map(
    "VFREDSUM_VS      " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFREDOSUM_VS     " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFREDMIN_VS      " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFREDMAX_VS      " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWREDSUM_VS     " -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y),
    "VFWREDOSUM_VS    " -> List(N,N,N, Src1_SEW,  Y, Src2_2SEW,N, Dest_2SEW, Sign_X,   IsFRed,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     Y, FMAFun_X,     N,Y)
  )
//mask population count inst
  val popc_decode = Map(
    "VPOPC_M          " -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_XLEN, Sign_X,   IsPopc,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//find-first-set mask bit inst
  val first_decode = Map(
    "VFIRST_M         " -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_XLEN, Sign_X,   IsFirst,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//set-before/including/only-first mask bit insts
  val mindex_decode = Map(
    "VMSBF_M          " -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_1b,   Sign_X,   IsMIdx,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSOF_M          " -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_1b,   Sign_X,   IsMIdx,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMSIF_M          " -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_1b,   Sign_X,   IsMIdx,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//iota inst
  val iota_decode = Map(
    "VIOTA_M          " -> List(N,N,N, Src1_1b,   N, Src2_N,   N, Dest_SEW,  Sign_X,   IsIota,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//element index inst
  val index_decode = Map(
    "VID_V            " -> List(N,N,N, Src1_N,    N, Src2_N,   N, Dest_SEW,  Sign_X,   IsIdx,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//mask-register logical insts
  val mbit_decode = Map(
    "VMANDNOT_MM      " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMAND_MM         " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMOR_MM          " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMXOR_MM         " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMORNOT_MM       " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMNAND_MM        " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMNOR_MM         " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMXNOR_MM        " -> List(Y,N,N, Src1_1b,   N, Src2_1b,  N, Dest_1b,   Sign_X,   IsMBit,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//integer scalar move insts 
  val mv_decode = Map(
    "VMV_X_S          " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_XLEN, Sign_X,   IsMv,      AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMV_S_X          " -> List(Y,N,N, Src1_N,    N, Src2_XLEN,N, Dest_SEW,  Sign_X,   IsMv,      AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//floating-point scalar move insts
  val fmv_decode = Map(
    "VFMV_F_S         " -> List(N,N,N, Src1_SEW,  N, Src2_N,   N, Dest_FLEN, Sign_X,   IsFMv,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFMV_S_F         " -> List(Y,N,N, Src1_N,    N, Src2_FLEN,N, Dest_SEW,  Sign_X,   IsFMv,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//slide insts
  val slide_decode = Map(
    "VSLIDEUP_VX      " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSLIDEUP_VI      " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSLIDEDOWN_VX    " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSLIDEDOWN_VI    " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsSlide,   AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSLIDE1UP_VX     " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VSLIDE1DOWN_VX   " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSLIDE1UP_VF    " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VFSLIDE1DOWN_VF  " -> List(N,N,N, Src1_SEW,  N, Src2_FLEN,N, Dest_SEW,  Signed,   IsSlide1,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//register gather insts
  val gather_decode = Map(
    "VRGATHER_VV      " -> List(N,N,N, Src1_SEW,  N, Src2_SEW, N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VRGATHEREI16_VV  " -> List(N,N,N, Src1_SEW,  N, Src2_16b, N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VRGATHER_VX      " -> List(N,N,N, Src1_SEW,  N, Src2_XLEN,N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VRGATHER_VI      " -> List(N,N,N, Src1_SEW,  N, Src2_5b,  N, Dest_SEW,  Unsigned, IsGather,  AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//compress elements inst
  val compress_decode = Map(
    "VCOMPRESS_VM     " -> List(Y,N,N, Src1_SEW,  N, Src2_1b,  N, Dest_SEW,  Sign_X,   IsZip,     AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
//copy registers inst
  val copy_decode = Map(
    "VMV1R_V          " -> List(Y,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsCopy,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMV2R_V          " -> List(Y,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsCopy,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMV4R_V          " -> List(Y,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsCopy,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y),
    "VMV8R_V          " -> List(Y,N,N, Src1_SEW,  N, Src2_N,   N, Dest_SEW,  Sign_X,   IsCopy,    AddFun_X,   Shift_X,    MulFun_X,      X,    X,     N, FMAFun_X,     N,Y)
  )
}
