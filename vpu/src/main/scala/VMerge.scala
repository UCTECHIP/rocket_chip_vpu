// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VMerge.scala
*       Author          :       liangzh
*       Revision        :       2020/02/20
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       merge source 1 and source 2
*
*       io.v0merge      :       input, bundle of vectors, control, elements all 1 bit, to merge operands
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, fixed-point operand 2
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, fixed-point operand 1
*       io.vsrc1f       :       input, bundle of vectors, data, UCB relative, floating-point operand 2
*       io.vsrc2f       :       input, bundle of vectors, data, UCB relative, floating-point operand 1
*       io.vMergeOut    :       output, bundle of vectors, data, SEW relative, fixed-point merge results
*       io.vFMergeOut   :       output, bundle of vectors, data, UCB relative, floating-point merge results
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VMerge(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val v0merge    = Input(new SEW1wVec)
    val vsrc1e     = Input(new SEWVec)
    val vsrc2e     = Input(new SEWVec)
    val vsrc1f     = Input(new FUCBVec)
    val vsrc2f     = Input(new FUCBVec)
    val vMergeOut  = Output(new SEWVec)
    val vFMergeOut = Output(new FUCBVec)
  })

  for(i <- 0 until e8Depth)    io.vMergeOut.e8(i)    := Mux((io.v0merge.e8(i)).asBool, io.vsrc1e.e8(i), io.vsrc2e.e8(i))
  for(i <- 0 until e16Depth)   io.vMergeOut.e16(i)   := Mux((io.v0merge.e16(i)).asBool, io.vsrc1e.e16(i),io.vsrc2e.e16(i))
  for(i <- 0 until e32Depth)   io.vMergeOut.e32(i)   := Mux((io.v0merge.e32(i)).asBool, io.vsrc1e.e32(i), io.vsrc2e.e32(i))
  for(i <- 0 until e64Depth)   io.vMergeOut.e64(i)   := Mux((io.v0merge.e64(i)).asBool, io.vsrc1e.e64(i), io.vsrc2e.e64(i))
  for(i <- 0 until e128Depth)  io.vMergeOut.e128(i)  := Mux((io.v0merge.e128(i)).asBool, io.vsrc1e.e128(i), io.vsrc2e.e128(i))
  for(i <- 0 until e256Depth)  io.vMergeOut.e256(i)  := Mux((io.v0merge.e256(i)).asBool, io.vsrc1e.e256(i), io.vsrc2e.e256(i))
  for(i <- 0 until e512Depth)  io.vMergeOut.e512(i)  := Mux((io.v0merge.e512(i)).asBool, io.vsrc1e.e512(i), io.vsrc2e.e512(i))
  for(i <- 0 until e1024Depth) io.vMergeOut.e1024(i) := Mux((io.v0merge.e1024(i)).asBool, io.vsrc1e.e1024(i), io.vsrc2e.e1024(i))

  for(i <- 0 until f16Depth)   io.vFMergeOut.f16(i)  := Mux((io.v0merge.e16(i)).asBool, io.vsrc1f.f16(i), io.vsrc2f.f16(i))
  for(i <- 0 until f32Depth)   io.vFMergeOut.f32(i)  := Mux((io.v0merge.e32(i)).asBool, io.vsrc1f.f32(i), io.vsrc2f.f32(i))
  for(i <- 0 until f64Depth)   io.vFMergeOut.f64(i)  := Mux((io.v0merge.e64(i)).asBool, io.vsrc1f.f64(i), io.vsrc2f.f64(i))
  for(i <- 0 until f128Depth)  io.vFMergeOut.f128(i) := Mux((io.v0merge.e128(i)).asBool, io.vsrc1f.f128(i), io.vsrc2f.f128(i))
}
