// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VWidthConvert.scala
*       Author          :       liangzh
*       Revision        :       2019/04/23
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       convert some data to log2(SEW) or log2(2SEW) or 2SEW width data
*
*       io.isSrc22SEW   :       input, control, affect source1 width conversion
*       io.isVd2SEW     :       input, control, affect source1 or source2 width conversion
*       io.sign         :       input, control, unsigned or signed conversion
*       io.src1Typ      :       input[Src1Typ_SZ-1:0], control, showing source 1 type is vector scalar or immediate
*       io.majFun       :       input[MajFun_SZ-1:0], control, help to determine which insts
*       io.fromXData1   :       input[XLEN-1:0], data, scalar for source 1
*       io.src1Field    :       input[4:0], data, immediate for source 1
*       io.vs1e         :       input bundle of vectors, data, source 1, to be width converted
*       io.vs2e         :       input bundle of vectors, data, source 2, to be width converted
*       io.vsrc1t       :       output bundle of vectors, data, converted source 1 for shift operations
*       io.vsrc1e       :       output bundle of vectors, data, converted source 1 for other operations
*       io.vsrc2e       :       output bundle of vectors, data, convertde source 2
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import vpu.DataExtend._

class VWidthConvert(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val isSrc22SEW = Input(Bool())
    val isVd2SEW   = Input(Bool())
    val sign       = Input(Bool())
    val src1Typ    = Input(UInt(Src1Typ_SZ.W))
    val majFun     = Input(UInt(MajFun_SZ.W))
    val vm         = Input(UInt(1.W))
    val vlmul      = Input(UInt(VLMUL_SZ.W))

    val fromXData1 = Input(UInt(XLEN.W))
    val src1Field  = Input(UInt(5.W))

    val vs1e       = Input(new SEWVec)
    val vs2e       = Input(new SEWVec)

    val vsrc1t     = Output(new Log2SEWVec)
    val vsrc1e     = Output(new SEWVec)
    val vsrc2e     = Output(new SEWVec)
    val vsrc2d     = Output(new SEWDoubVec)
  })

//select signals for changing source 1 to shift amount
  val isVLogSEW  = io.src1Typ === Src1_Vs && !io.isSrc22SEW
  val isVLog2SEW = io.src1Typ === Src1_Vs && io.isSrc22SEW
  val isXLogSEW  = io.src1Typ === Src1_Xs
  val isILogSEW  = io.src1Typ === Src1_Imm

//select signals for changing vs1 to double width
  val isV1Origin  = io.src1Typ === Src1_Vs && (!io.isSrc22SEW && !io.isVd2SEW || 
                    io.majFun === IsMulAdd || io.majFun === IsRed)
  val isV1toU2SEW = io.src1Typ === Src1_Vs && io.isVd2SEW && io.majFun === IsAdd && !io.sign
  val isV1toS2SEW = io.src1Typ === Src1_Vs && io.isVd2SEW && io.majFun === IsAdd && io.sign
//select signals for changing rs1 to single width or double width
  val isX1to1SEW  = io.src1Typ === Src1_Xs && (!io.isVd2SEW || io.majFun =/= IsAdd)
  val isX1toU2SEW = io.src1Typ === Src1_Xs && io.isVd2SEW && io.majFun === IsAdd && !io.sign
  val isX1toS2SEW = io.src1Typ === Src1_Xs && io.isVd2SEW && io.majFun === IsAdd && io.sign
  val isX1toSEW   = io.src1Typ === Src1_Xs
  val isX1toUSEW  = isX1toSEW && !io.sign
  val isX1toSSEW  = isX1toSEW && io.sign
//select signals for changing imm1 to single width or double width
  val isI1toUSEW  = io.src1Typ === Src1_Imm && !io.sign
  val isI1toSSEW  = io.src1Typ === Src1_Imm && io.sign

//select signals for changing vs2 to double width
  val isV2to2SEW  = !io.isSrc22SEW && io.isVd2SEW && (io.majFun === IsAdd || io.majFun === IsRed)
  val isV2Origin  = !isV2to2SEW
  val isV2toU2SEW = isV2to2SEW && !io.sign
  val isV2toS2SEW = isV2to2SEW && io.sign

//prepare value for vsrc1e output
  val xDataToS8   = io.fromXData1(7,0)
  val xDataToS16  = io.fromXData1(15,0)
  val xDataToS32  = io.fromXData1(31,0)

  val xDataToD16S = Cat(Fill(8, xDataToS8(7)), xDataToS8)
  val xDataToD16U = Cat(0.U(8.W), xDataToS8)
  val xDataToD32S = Cat(Fill(16, xDataToS16(15)), xDataToS16)
  val xDataToD32U = Cat(0.U(16.W), xDataToS16)
  val xDataToD64S = Cat(Fill(32, xDataToS32(31)), xDataToS32)
  val xDataToD64U = Cat(0.U(32.W), xDataToS32)

  val x32ToS64S   = Cat(Fill(32, io.fromXData1(31)), io.fromXData1)
  val x32ToS64U   = Cat(0.U(32.W), io.fromXData1)
  def x32ToA128S  = Cat(Fill(96, io.fromXData1(31)), io.fromXData1)
  def x32ToA128U  = Cat(0.U(96.W), io.fromXData1)
  def x32ToA256S  = Cat(Fill(224, io.fromXData1(31)), io.fromXData1)
  def x32ToA256U  = Cat(0.U(224.W), io.fromXData1)
  def x32ToA512S  = Cat(Fill(480, io.fromXData1(31)), io.fromXData1)
  def x32ToA512U  = Cat(0.U(480.W), io.fromXData1)
  def x32ToA1024S = Cat(Fill(992, io.fromXData1(31)), io.fromXData1)
  def x32ToA1024U = Cat(0.U(992.W), io.fromXData1)

  val x64ToS64    = io.fromXData1
  def x64ToA128S  = Cat(Fill(64, io.fromXData1(63)), io.fromXData1)
  def x64ToA128U  = Cat(0.U(64.W), io.fromXData1)
  def x64ToA256S  = Cat(Fill(192, io.fromXData1(63)), io.fromXData1)
  def x64ToA256U  = Cat(0.U(192.W), io.fromXData1)
  def x64ToA512S  = Cat(Fill(448, io.fromXData1(63)), io.fromXData1)
  def x64ToA512U  = Cat(0.U(448.W), io.fromXData1)
  def x64ToA1024S = Cat(Fill(960, io.fromXData1(63)), io.fromXData1)
  def x64ToA1024U = Cat(0.U(960.W), io.fromXData1)

  val xDataToA128S  = if(XLEN == 32) x32ToA128S  else x64ToA128S
  val xDataToA128U  = if(XLEN == 32) x32ToA128U  else x64ToA128U
  val xDataToA256S  = if(XLEN == 32) x32ToA256S  else x64ToA256S
  val xDataToA256U  = if(XLEN == 32) x32ToA256U  else x64ToA256U
  val xDataToA512S  = if(XLEN == 32) x32ToA512S  else x64ToA512S
  val xDataToA512U  = if(XLEN == 32) x32ToA512U  else x64ToA512U
  val xDataToA1024S = if(XLEN == 32) x32ToA1024S else x64ToA1024S
  val xDataToA1024U = if(XLEN == 32) x32ToA1024U else x64ToA1024U

  val immToS8S    = Cat(Fill(3, io.src1Field(4)), io.src1Field)
  val immToS8U    = Cat(0.U(3.W), io.src1Field)
  val immToA16S   = Cat(Fill(11, io.src1Field(4)), io.src1Field)
  val immToA16U   = Cat(0.U(11.W), io.src1Field)
  val immToA32S   = Cat(Fill(27, io.src1Field(4)), io.src1Field)
  val immToA32U   = Cat(0.U(27.W), io.src1Field)
  val immToA64S   = Cat(Fill(59, io.src1Field(4)), io.src1Field)
  val immToA64U   = Cat(0.U(59.W), io.src1Field)
  val immToA128S  = Cat(Fill(123, io.src1Field(4)), io.src1Field)
  val immToA128U  = Cat(0.U(123.W), io.src1Field)
  val immToA256S  = Cat(Fill(251, io.src1Field(4)), io.src1Field)
  val immToA256U  = Cat(0.U(251.W), io.src1Field)
  val immToA512S  = Cat(Fill(507, io.src1Field(4)), io.src1Field)
  val immToA512U  = Cat(0.U(507.W), io.src1Field)
  val immToA1024S = Cat(Fill(1019, io.src1Field(4)), io.src1Field)
  val immToA1024U = Cat(0.U(1019.W), io.src1Field)


/*convert vs1 which width is SEW to log2(SEW) or log2(2SEW) and xs1 & imm to log2(SEW) for shift operation*/
//3-bits width output can from 8-bits vector, XLEN-bits scalar, 5-bits immediate
  for(i <- 0 until e8Depth)
    io.vsrc1t.to3(i) := MuxCase(0.U(3.W),
                          Array(isVLogSEW -> io.vs1e.e8(i)(2,0),
                                isXLogSEW -> io.fromXData1(2,0),
                                isILogSEW -> io.src1Field(2,0)))

//4-bits width output can from 16-bits vector, 8-bits vector, XLEN-bits scalar, 5-bits immediate
  for(i <- 0 until e16Depth)
    io.vsrc1t.to4(i) := MuxCase(0.U(4.W),
                          Array(isVLogSEW -> io.vs1e.e16(i)(3,0),
                                isXLogSEW -> io.fromXData1(3,0),
                                isILogSEW -> io.src1Field(3,0),
                                isVLog2SEW -> io.vs1e.e8(i)(3,0)))

//5-bits width output can from 32-bits vector, 16-bits vector, XLEN-bits scalar, 5-bits immediate
  for(i <- 0 until e32Depth)
    io.vsrc1t.to5(i) := MuxCase(0.U(5.W),
                          Array(isVLogSEW -> io.vs1e.e32(i)(4,0),
                                isXLogSEW -> io.fromXData1(4,0),
                                isILogSEW -> io.src1Field,
                                isVLog2SEW -> io.vs1e.e16(i)(4,0)))

//6-bits width output can from 64-bits vector, 32-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1t.to6 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e64Depth)
    io.vsrc1t.to6(i) := MuxCase(0.U(6.W),
                          Array(isVLogSEW -> io.vs1e.e64(i)(5,0),
                                isXLogSEW -> io.fromXData1(5,0),
                                isILogSEW -> Cat(0.U(1.W), io.src1Field),
                                isVLog2SEW -> io.vs1e.e32(i)(5,0)))

//7-bits width output can from 128-bits vector, 64-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1t.to7 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e128Depth)
    io.vsrc1t.to7(i) := MuxCase(0.U(7.W),
                          Array(isVLogSEW -> io.vs1e.e128(i)(6,0),
                                isXLogSEW -> io.fromXData1(6,0),
                                isILogSEW -> Cat(0.U(2.W), io.src1Field),
                                isVLog2SEW -> io.vs1e.e64(i)(6,0)))

//8-bits width output can from 256-bits vector, 128-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1t.to8 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e256Depth)
    io.vsrc1t.to8(i) := MuxCase(0.U(8.W),
                          Array(isVLogSEW -> io.vs1e.e256(i)(7,0),
                                isXLogSEW -> io.fromXData1(7,0),
                                isILogSEW -> Cat(0.U(3.W), io.src1Field),
                                isVLog2SEW -> io.vs1e.e128(i)(7,0)))

//9-bits width output can from 512-bits vector, 256-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1t.to9 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e512Depth)
    io.vsrc1t.to9(i) := MuxCase(0.U(9.W),
                          Array(isVLogSEW -> io.vs1e.e512(i)(8,0),
                                isXLogSEW -> io.fromXData1(8,0),
                                isILogSEW -> Cat(0.U(4.W), io.src1Field),
                                isVLog2SEW -> io.vs1e.e256(i)(8,0)))

//10-bits width output can from 1024-bits vector, 512-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1t.to10 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e1024Depth)
    io.vsrc1t.to10(i) := MuxCase(0.U(10.W),
                           Array(isVLogSEW -> io.vs1e.e1024(i)(9,0),
                                 isXLogSEW -> io.fromXData1(9,0),
                                 isILogSEW -> Cat(0.U(5.W), io.src1Field),
                                 isVLog2SEW -> io.vs1e.e512(i)(9,0)))





//////////*convert vs1 which width is SEW to 2SEW and xs1 & imm to SEW for other operation except shift*//////////
//8-bits width output can from 8-bits vector, XLEN-bits scalar, 5-bits immediate
  for(i <- 0 until e8Depth)
    io.vsrc1e.e8(i) := MuxCase(0.U(8.W),
                         Array(isV1Origin -> io.vs1e.e8(i),
                               isX1to1SEW -> xDataToS8,
                               isI1toUSEW -> immToS8U,
                               isI1toSSEW -> immToS8S))

//16-bits width output can from 16-bits vector, 8-bits vector, XLEN-bits scalar, 5-bits immediate
  for(i <- 0 until e16Depth)
    io.vsrc1e.e16(i) := MuxCase(0.U(16.W),
                          Array(isV1Origin -> io.vs1e.e16(i),
                                isX1to1SEW -> xDataToS16,
                                isI1toUSEW -> immToA16U,
                                isI1toSSEW -> immToA16S,
                                isV1toU2SEW -> UnsignExtend(io.vs1e.e8(i), 8, 16),
                                isV1toS2SEW -> SignExtend(io.vs1e.e8(i), 8, 16),
                                isX1toU2SEW -> xDataToD16U,
                                isX1toS2SEW -> xDataToD16S))

//32-bits width output can from 32-bits vector, 16-bits vector, XLEN-bits scalar, 5-bits immediate
  for(i <- 0 until e32Depth)
    io.vsrc1e.e32(i) := MuxCase(0.U(32.W),
                          Array(isV1Origin -> io.vs1e.e32(i),
                                isX1to1SEW -> xDataToS32,
                                isI1toUSEW -> immToA32U,
                                isI1toSSEW -> immToA32S,
                                isV1toU2SEW -> UnsignExtend(io.vs1e.e16(i), 16, 32),
                                isV1toS2SEW -> SignExtend(io.vs1e.e16(i), 16, 32),
                                isX1toU2SEW -> xDataToD32U,
                                isX1toS2SEW -> xDataToD32S))

//64-bits width output can from 64-bits vector, 32-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1e.e64 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  if(XLEN == 64)
    for(i <- 0 until e64Depth)
      io.vsrc1e.e64(i) := MuxCase(0.U(64.W),
                            Array(isV1Origin -> io.vs1e.e64(i),
                                  isX1to1SEW -> x64ToS64,
                                  isI1toUSEW -> immToA64U,
                                  isI1toSSEW -> immToA64S,
                                  isV1toU2SEW -> UnsignExtend(io.vs1e.e32(i), 32, 64),
                                  isV1toS2SEW -> SignExtend(io.vs1e.e32(i), 32, 64),
                                  isX1toU2SEW -> xDataToD64U,
                                  isX1toS2SEW -> xDataToD64S))
  else if(XLEN == 32)
    for(i <- 0 until e64Depth)
      io.vsrc1e.e64(i) := MuxCase(0.U(64.W),
                            Array(isV1Origin -> io.vs1e.e64(i),
                                  isX1toUSEW -> x32ToS64U,
                                  isX1toSSEW -> x32ToS64S,
                                  isI1toUSEW -> immToA64U,
                                  isI1toSSEW -> immToA64S,
                                  isV1toU2SEW -> UnsignExtend(io.vs1e.e32(i), 32, 64),
                                  isV1toS2SEW -> SignExtend(io.vs1e.e32(i), 32, 64)))

//128-bits width output can from 128-bits vector, 64-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1e.e128 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e128Depth)
    io.vsrc1e.e128(i) := MuxCase(0.U(128.W),
                          Array(isV1Origin -> io.vs1e.e128(i),
                                isX1toUSEW -> xDataToA128U,
                                isX1toSSEW -> xDataToA128S,
                                isI1toUSEW -> immToA128U,
                                isI1toSSEW -> immToA128S,
                                isV1toU2SEW -> UnsignExtend(io.vs1e.e64(i), 64, 128),
                                isV1toS2SEW -> SignExtend(io.vs1e.e64(i), 64, 128)))

//256-bits width output can from 256-bits vector, 128-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1e.e256 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e256Depth)
    io.vsrc1e.e256(i) := MuxCase(0.U(256.W),
                          Array(isV1Origin -> io.vs1e.e256(i),
                                isX1toUSEW -> xDataToA256U,
                                isX1toSSEW -> xDataToA256S,
                                isI1toUSEW -> immToA256U,
                                isI1toSSEW -> immToA256S,
                                isV1toU2SEW -> UnsignExtend(io.vs1e.e128(i), 128, 256),
                                isV1toS2SEW -> SignExtend(io.vs1e.e128(i), 128, 256)))

//512-bits width output can from 512-bits vector, 256-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1e.e512 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e512Depth)
    io.vsrc1e.e512(i) := MuxCase(0.U(512.W),
                          Array(isV1Origin -> io.vs1e.e512(i),
                                isX1toUSEW -> xDataToA512U,
                                isX1toSSEW -> xDataToA512S,
                                isI1toUSEW -> immToA512U,
                                isI1toSSEW -> immToA512S,
                                isV1toU2SEW -> UnsignExtend(io.vs1e.e256(i), 256, 512),
                                isV1toS2SEW -> SignExtend(io.vs1e.e256(i), 256, 512)))

//1024-bits width output can from 1024-bits vector, 512-bits vector, XLEN-bits scalar, 5-bits immediate
//if io.vsrc1e.e1024 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e1024Depth)
    io.vsrc1e.e1024(i) := MuxCase(0.U(1024.W),
                           Array(isV1Origin -> io.vs1e.e1024(i),
                                 isX1toUSEW -> xDataToA1024U,
                                 isX1toSSEW -> xDataToA1024S,
                                 isI1toUSEW -> immToA1024U,
                                 isI1toSSEW -> immToA1024S,
                                 isV1toU2SEW -> UnsignExtend(io.vs1e.e512(i), 512, 1024),
                                 isV1toS2SEW -> SignExtend(io.vs1e.e512(i), 512, 1024)))





//////////*convert a set of vector -- vs2 which width is SEW to 2SEW for other operation except shift*///////////
//8-bits width output only from 8-bits vector
  io.vsrc2e.e8 := Mux(isV2Origin, io.vs2e.e8, VecInit(Seq.fill(e8Depth){ 0.U(8.W) }))
//16-bits width output can from 16-bits vector, 8-bits vector
  for(i <- 0 until e16Depth)
    io.vsrc2e.e16(i) := MuxCase(0.U(16.W),
                          Array(isV2Origin  -> io.vs2e.e16(i),
                                isV2toU2SEW -> UnsignExtend(io.vs2e.e8(i), 8, 16),
                                isV2toS2SEW -> SignExtend(io.vs2e.e8(i), 8, 16)))

//32-bits width output can from 32-bits vector, 16-bits vector
  for(i <- 0 until e32Depth)
    io.vsrc2e.e32(i) := MuxCase(0.U(32.W),
                          Array(isV2Origin  -> io.vs2e.e32(i),
                                isV2toU2SEW -> UnsignExtend(io.vs2e.e16(i), 16, 32),
                                isV2toS2SEW -> SignExtend(io.vs2e.e16(i), 16, 32)))

//64-bits width output can from 64-bits vector, 32-bits vector
//if io.vsrc2e.e64 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e64Depth)
    io.vsrc2e.e64(i) := MuxCase(0.U(64.W),
                          Array(isV2Origin  -> io.vs2e.e64(i),
                                isV2toU2SEW -> UnsignExtend(io.vs2e.e32(i), 32, 64),
                                isV2toS2SEW -> SignExtend(io.vs2e.e32(i), 32, 64)))

//128-bits width output can from 128-bits vector, 64-bits vector
//if io.vsrc2e.e128 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e128Depth)
    io.vsrc2e.e128(i) := MuxCase(0.U(128.W),
                           Array(isV2Origin -> io.vs2e.e128(i),
                                 isV2toU2SEW -> UnsignExtend(io.vs2e.e64(i), 64, 128),
                                 isV2toS2SEW -> SignExtend(io.vs2e.e64(i), 64, 128)))

//256-bits width output can from 256-bits vector, 128-bits vector
//if io.vsrc2e.e256 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e256Depth)
    io.vsrc2e.e256(i) := MuxCase(0.U(256.W),
                           Array(isV2Origin -> io.vs2e.e256(i),
                                 isV2toU2SEW -> UnsignExtend(io.vs2e.e128(i), 128, 256),
                                 isV2toS2SEW -> SignExtend(io.vs2e.e128(i), 128, 256)))

//512-bits width output can from 512-bits vector, 256-bits vector
//if io.vsrc2e.e512 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e512Depth)
    io.vsrc2e.e512(i) := MuxCase(0.U(512.W),
                           Array(isV2Origin  -> io.vs2e.e512(i),
                                 isV2toU2SEW -> UnsignExtend(io.vs2e.e256(i), 256, 512),
                                 isV2toS2SEW -> SignExtend(io.vs2e.e256(i), 256, 512)))

//1024-bits width output can from 1024-bits vector, 512-bits vector
//if io.vsrc2e.e1024 is a empty vector under parameter ELEN's control, code below won't be interpreted to verilog
  for(i <- 0 until e1024Depth)
    io.vsrc2e.e1024(i) := MuxCase(0.U(128.W),
                            Array(isV2Origin -> io.vs2e.e1024(i),
                                  isV2toU2SEW -> UnsignExtend(io.vs2e.e512(i), 512, 1024),
                                  isV2toS2SEW -> SignExtend(io.vs2e.e512(i), 512, 1024)))





/////////////////////////////////////////signed or unsigned expand vs1////////////////////////////////////////////
  for(i <- 0 until e8Depth)    io.vsrc2d.e16(i)   := Mux(io.sign, SignExtend(io.vs2e.e8(i), 8, 16), UnsignExtend(io.vs2e.e8(i), 8, 16))
  for(i <- 0 until e16Depth)   io.vsrc2d.e32(i)   := Mux(io.sign, SignExtend(io.vs2e.e16(i), 16, 32), UnsignExtend(io.vs2e.e16(i), 16, 32))
  for(i <- 0 until e32Depth)   io.vsrc2d.e64(i)   := Mux(io.sign, SignExtend(io.vs2e.e32(i), 32, 64), UnsignExtend(io.vs2e.e32(i), 32, 64))
  for(i <- 0 until e64Depth)   io.vsrc2d.e128(i)  := Mux(io.sign, SignExtend(io.vs2e.e64(i), 64, 128), UnsignExtend(io.vs2e.e64(i), 64, 128))
  for(i <- 0 until e128Depth)  io.vsrc2d.e256(i)  := Mux(io.sign, SignExtend(io.vs2e.e128(i), 128, 256), UnsignExtend(io.vs2e.e128(i), 128, 256))
  for(i <- 0 until e256Depth)  io.vsrc2d.e512(i)  := Mux(io.sign, SignExtend(io.vs2e.e256(i), 256, 512), UnsignExtend(io.vs2e.e256(i), 256, 512))
  for(i <- 0 until e512Depth)  io.vsrc2d.e1024(i) := Mux(io.sign, SignExtend(io.vs2e.e512(i), 512, 1024), UnsignExtend(io.vs2e.e512(i), 512, 1024))
  for(i <- 0 until e1024Depth) io.vsrc2d.e2048(i) := 0.U
}
