// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VDmemSplitSplit.scala
*       Author          :       liangzh
*       Revision        :       2019/06/24
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       split 8VLEN data to SEW or MLEN width data
*
*       io.vs2Data      :       input[8VLEN-1:0], data, to be split
*       io.vdvs3Data    :       input[8VLEN-1:0], data, to be split
*       io.v0Mask       :       input[VLEN-1:0], data, to be split
*       io.vs2e         :       output bundle of vectors, data, split vs2Data into SEW width
*       io.vdvs3e       :       output bundle of vectors, data, split vdvs3Data into SEW width
*       io.v0m          :       output bundle of vectors, data, split v0Mask into MLEN width
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VDmemSplit(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vs2Data   = Input(UInt((8*VLEN).W))
    val vdvs3Data = Input(UInt((8*VLEN).W))
    val v0Mask    = Input(UInt(VLEN.W))

    val vs2e      = Output(new FullSEWVec)
    val vdvs3e    = Output(new FullSEWVec)

    val v0m       = Output(new MLENVec)
  })

//split vs2Data into SEW width vectors
    for(i <- 0 until E8Depth)    io.vs2e.e8(i)      := io.vs2Data(8*(i+1)-1, 8*i)
    for(i <- 0 until E16Depth)   io.vs2e.e16(i)     := io.vs2Data(16*(i+1)-1, 16*i)
    for(i <- 0 until E32Depth)   io.vs2e.e32(i)     := io.vs2Data(32*(i+1)-1, 32*i)
    for(i <- 0 until E64Depth)   io.vs2e.e64(i)     := io.vs2Data(64*(i+1)-1, 64*i)
    for(i <- 0 until E128Depth)  io.vs2e.e128(i)    := io.vs2Data(128*(i+1)-1, 128*i)
    for(i <- 0 until E256Depth)  io.vs2e.e256(i)    := io.vs2Data(256*(i+1)-1, 256*i)
    for(i <- 0 until E512Depth)  io.vs2e.e512(i)    := io.vs2Data(512*(i+1)-1, 512*i)
    for(i <- 0 until E1024Depth) io.vs2e.e1024(i)   := io.vs2Data(1024*(i+1)-1, 1024*i)
//split vdvs3Data into SEW width vectors
    for(i <- 0 until E8Depth)    io.vdvs3e.e8(i)    := io.vdvs3Data(8*(i+1)-1, 8*i)
    for(i <- 0 until E16Depth)   io.vdvs3e.e16(i)   := io.vdvs3Data(16*(i+1)-1, 16*i)
    for(i <- 0 until E32Depth)   io.vdvs3e.e32(i)   := io.vdvs3Data(32*(i+1)-1, 32*i)
    for(i <- 0 until E64Depth)   io.vdvs3e.e64(i)   := io.vdvs3Data(64*(i+1)-1, 64*i)
    for(i <- 0 until E128Depth)  io.vdvs3e.e128(i)  := io.vdvs3Data(128*(i+1)-1, 128*i)
    for(i <- 0 until E256Depth)  io.vdvs3e.e256(i)  := io.vdvs3Data(256*(i+1)-1, 256*i)
    for(i <- 0 until E512Depth)  io.vdvs3e.e512(i)  := io.vdvs3Data(512*(i+1)-1, 512*i)
    for(i <- 0 until E1024Depth) io.vdvs3e.e1024(i) := io.vdvs3Data(1024*(i+1)-1, 1024*i)
//split v0Mask into MLEN width vectors
    for(i <- 0 until m1Depth)    io.v0m.m1(i)       := io.v0Mask(i)
    for(i <- 0 until m2Depth)    io.v0m.m2(i)       := io.v0Mask(2*i)
    for(i <- 0 until m4Depth)    io.v0m.m4(i)       := io.v0Mask(4*i)
    for(i <- 0 until m8Depth)    io.v0m.m8(i)       := io.v0Mask(8*i)
    for(i <- 0 until m16Depth)   io.v0m.m16(i)      := io.v0Mask(16*i)
    for(i <- 0 until m32Depth)   io.v0m.m32(i)      := io.v0Mask(32*i)
    for(i <- 0 until m64Depth)   io.v0m.m64(i)      := io.v0Mask(64*i)
    for(i <- 0 until m128Depth)  io.v0m.m128(i)     := io.v0Mask(128*i)
    for(i <- 0 until m256Depth)  io.v0m.m256(i)     := io.v0Mask(256*i)
    for(i <- 0 until m512Depth)  io.v0m.m512(i)     := io.v0Mask(512*i)
    for(i <- 0 until m1024Depth) io.v0m.m1024(i)    := io.v0Mask(1024*i)
}
