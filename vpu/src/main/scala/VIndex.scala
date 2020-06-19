// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VIndex.scala
*       Author          :       liangzh
*       Revision        :       2020/04/01
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       output index
*
*       io.vIndexOut    :       output, bundle of vectors, data, SEW relative, vectors of index
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VIndex(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vIndexOut = Output(new FullSEWVec)
  })

  io.vIndexOut.e8    := VecInit(Seq.range(0, E8Depth).map(e8 => if(e8 < 256) e8.U(8.W) else 0.U(8.W)))
  io.vIndexOut.e16   := VecInit(Seq.range(0, E16Depth).map(_.U(16.W)))
  io.vIndexOut.e32   := VecInit(Seq.range(0, E32Depth).map(_.U(32.W)))
  if(ELEN >= 64)   io.vIndexOut.e64   := VecInit(Seq.range(0, E64Depth).map(_.U(64.W)))
  if(ELEN >= 128)  io.vIndexOut.e128  := VecInit(Seq.range(0, E128Depth).map(_.U(128.W)))
  if(ELEN >= 256)  io.vIndexOut.e256  := VecInit(Seq.range(0, E256Depth).map(_.U(256.W)))
  if(ELEN >= 512)  io.vIndexOut.e512  := VecInit(Seq.range(0, E512Depth).map(_.U(512.W)))
  if(ELEN == 1024) io.vIndexOut.e1024 := VecInit(Seq.range(0, E1024Depth).map(_.U(1024.W)))
}
