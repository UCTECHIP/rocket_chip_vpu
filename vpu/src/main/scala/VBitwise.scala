// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VBitwise.scala
*       Author          :       sujy, liangzh
*       Revision        :       2019/04/29
*       Company         :       UC TECH IP
*       Department      :       STG
*       Description     :       Bitwise operation of VPU
*
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, operand 2
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, operand 1
*       io.bitFun       :       input[BitFun_SZ-1:0], control, function select
*       io.vBitwise     :       output, bundle of vectors, data, SEW relative, bitwise result
*       io.vMBitwise    :       output, bundle of vectors, data, MLEN relative, bitwise result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VBitwise(params: VPUParams) extends VModule(params) {
  require(e8Depth*8 == VLEN)
  val io = IO(new Bundle {
    val vsrc1e = Input(new SEWVec)
    val vsrc2e = Input(new SEWVec)
    val bitFun = Input(UInt(BitFun_SZ.W))

    val vBitwise   = Output(new SEWVec)
    val vMBitwise  = Output(new MLENVec)
  })

  def Bitwise8(src2:UInt, src1:UInt, sel:UInt) = {
    val result = MuxCase(0.U,
                  Array((sel === BitFun_AndNot) ->  (src2 & ~src1),
                        (sel === BitFun_And)    ->  (src2 &  src1),
                        (sel === BitFun_Or)     ->  (src2 |  src1),
                        (sel === BitFun_Xor)    ->  (src2 ^  src1),
                        (sel === BitFun_OrNot)  ->  (src2 | ~src1),
                        (sel === BitFun_Nand)   -> ~(src2 &  src1),
                        (sel === BitFun_Nor)    -> ~(src2 |  src1),
                        (sel === BitFun_Xnor)   -> ~(src2 ^  src1)))
    result
  }
  def Bitwise(src2:UInt, src1:UInt, sel:UInt) = {
    val result = MuxCase(0.U,
                  Array((sel === BitFun_And) -> (src2 & src1),
                        (sel === BitFun_Or)  -> (src2 | src1),
                        (sel === BitFun_Xor) -> (src2 ^ src1)))
    result
  }


  val e8Data = io.vBitwise.e8.asUInt
  for(i <- 0 until e8Depth)    io.vBitwise.e8(i)    := Bitwise8(io.vsrc2e.e8(i),   io.vsrc1e.e8(i),    io.bitFun)
  for(i <- 0 until e16Depth)   io.vBitwise.e16(i)   := Bitwise(io.vsrc2e.e16(i),   io.vsrc1e.e16(i),   io.bitFun)
  for(i <- 0 until e32Depth)   io.vBitwise.e32(i)   := Bitwise(io.vsrc2e.e32(i),   io.vsrc1e.e32(i),   io.bitFun)
  for(i <- 0 until e64Depth)   io.vBitwise.e64(i)   := Bitwise(io.vsrc2e.e64(i),   io.vsrc1e.e64(i),   io.bitFun)
  for(i <- 0 until e128Depth)  io.vBitwise.e128(i)  := Bitwise(io.vsrc2e.e128(i),  io.vsrc1e.e128(i),  io.bitFun)
  for(i <- 0 until e256Depth)  io.vBitwise.e256(i)  := Bitwise(io.vsrc2e.e256(i),  io.vsrc1e.e256(i),  io.bitFun)
  for(i <- 0 until e512Depth)  io.vBitwise.e512(i)  := Bitwise(io.vsrc2e.e512(i),  io.vsrc1e.e512(i),  io.bitFun)
  for(i <- 0 until e1024Depth) io.vBitwise.e1024(i) := Bitwise(io.vsrc2e.e1024(i), io.vsrc1e.e1024(i), io.bitFun)

  for(i <- 0 until m1Depth)    io.vMBitwise.m1(i)    := e8Data(i)
  for(i <- 0 until m2Depth)    io.vMBitwise.m2(i)    := e8Data(2*i)
  for(i <- 0 until m4Depth)    io.vMBitwise.m4(i)    := e8Data(4*i)
  for(i <- 0 until m8Depth)    io.vMBitwise.m8(i)    := e8Data(8*i)
  for(i <- 0 until m16Depth)   io.vMBitwise.m16(i)   := e8Data(16*i)
  for(i <- 0 until m32Depth)   io.vMBitwise.m32(i)   := e8Data(32*i)
  for(i <- 0 until m64Depth)   io.vMBitwise.m64(i)   := e8Data(64*i)
  for(i <- 0 until m128Depth)  io.vMBitwise.m128(i)  := e8Data(128*i)
  for(i <- 0 until m256Depth)  io.vMBitwise.m256(i)  := e8Data(256*i)
  for(i <- 0 until m512Depth)  io.vMBitwise.m512(i)  := e8Data(512*i)
  for(i <- 0 until m1024Depth) io.vMBitwise.m1024(i) := e8Data(1024*i)


}
