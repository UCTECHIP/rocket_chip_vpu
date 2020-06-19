// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VSplit.scala
*       Author          :       liangzh
*       Revision        :       2019/04/22
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       split 8VLEN data to SEW or MLEN width data
*
*       io.vs1Data      :       input[8VLEN-1:0], data, to be split
*       io.vs2Data      :       input[8VLEN-1:0], data, to be split
*       io.vdvs3Data    :       input[8VLEN-1:0], data, to be split
*       io.v0Mask       :       input[VLEN-1:0], data, to be split
*       io.isSrc12SEW   :       input, control, showing whether source 1 is double SEW width or not
*       io.isSrc22SEW   :       input, control, showing whether source 2 is double SEW width or not
*       io.isVd2SEW     :       input, control, showing whether destination is double SEW width or not
                                                or quadruple SEW width for multiply-add
*       io.isFullMul    :       input, control, showing whether the multiply product is double SEW width or not
*       io.inStep       :       input, vector, control, showing in which step during rolling calculation
*       io.vs1e         :       output, bundle of vectors, data, split vs1Data into SEW width, for rolling calculation
*       io.vs2e         :       output, bundle of vectors, data, split vs2Data into SEW width, for rolling calculation
*       io.vdvs3e       :       output, bundle of vectors, data, split vdvs3Data into SEW width, for rolling calculation
*       io.vs1E         :       output, bundle of vectors, data, split vs1Data into SEW width, for full elements calculation
*       io.vs2E         :       output, bundle of vectors, data, split vs2Data into SEW width, for full elements calculation
*       io.vdvs3E       :       output, bundle of vectors, data, split vdvs3Data into SEW width, for full elements calculation
*       io.vs1m         :       output bundle of vectors, data, split vs1Data into MLEN width
*       io.vs2m         :       output bundle of vectors, data, split vs2Data into MLEN width
*       io.vdm          :       output bundle of vectors, data, split vdvs3Data into MLEN width
*       io.v0m          :       output bundle of vectors, data, split v0Mask into MLEN width
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VSplit(params: VPUParams) extends VModule(params) {
  require(VLEN == ELEN) //temporary require

  val io = IO(new Bundle {
    val vs1Data    = Input(UInt((8*VLEN).W))
    val vs2Data    = Input(UInt((8*VLEN).W))
    val vdvs3Data  = Input(UInt((8*VLEN).W))
    val v0Mask     = Input(UInt(VLEN.W))

    val isSrc12SEW = Input(Bool())
    val isSrc22SEW = Input(Bool())
    val isVd2SEW   = Input(Bool())
    val isFullMul  = Input(Bool())
    val inStep     = Input(Vec(7, Bool()))

    val vs1e       = Output(new SEWVec)
    val vs2e       = Output(new SEWVec)
    val vdvs3e     = Output(new SEWVec)

    val vs1E       = Output(new FullSEWVec)
    val vs2E       = Output(new FullSEWVec)
    val vdvs3E     = Output(new FullSEWVec)

    val vs1m       = Output(new MLENVec)
    val vs2m       = Output(new MLENVec)
    val vdm        = Output(new MLENVec)
    val v0m        = Output(new MLENVec)

    val vs1d       = Output(new SEWEDIVVec)
    val vs2d       = Output(new SEWEDIVVec)
  })

  val vs1Data0     = Wire(UInt(ELEN.W))
  val vs1Data1     = Wire(UInt(ELEN.W))
  val vs1Data2     = Wire(UInt(ELEN.W))
  val needvs1Data1 = !io.isSrc12SEW && (io.isSrc22SEW || io.isVd2SEW) && !io.isFullMul || 
                     !io.isSrc12SEW && !io.isSrc22SEW && !io.isVd2SEW && io.isFullMul
  val needvs1Data2 = !io.isSrc12SEW && !io.isSrc22SEW && io.isVd2SEW && io.isFullMul
  val vs1Data      = Mux(needvs1Data2, vs1Data2, Mux(needvs1Data1, vs1Data1, vs1Data0))
  vs1Data0 := MuxCase(io.vs1Data,
               Array(io.inStep(0) -> (io.vs1Data >> ELEN),
                     io.inStep(1) -> (io.vs1Data >> 2*ELEN),
                     io.inStep(2) -> (io.vs1Data >> 3*ELEN),
                     io.inStep(3) -> (io.vs1Data >> 4*ELEN),
                     io.inStep(4) -> (io.vs1Data >> 5*ELEN),
                     io.inStep(5) -> (io.vs1Data >> 6*ELEN),
                     io.inStep(6) -> (io.vs1Data >> 7*ELEN)))
  vs1Data1 := MuxCase(io.vs1Data,
               Array(io.inStep(0) -> (io.vs1Data >> ELEN/2),
                     io.inStep(1) -> (io.vs1Data >> 2*ELEN/2),
                     io.inStep(2) -> (io.vs1Data >> 3*ELEN/2),
                     io.inStep(3) -> (io.vs1Data >> 4*ELEN/2),
                     io.inStep(4) -> (io.vs1Data >> 5*ELEN/2),
                     io.inStep(5) -> (io.vs1Data >> 6*ELEN/2),
                     io.inStep(6) -> (io.vs1Data >> 7*ELEN/2)))
  vs1Data2 := MuxCase(io.vs1Data,
               Array(io.inStep(0) -> (io.vs1Data >> ELEN/4),
                     io.inStep(1) -> (io.vs1Data >> 2*ELEN/4),
                     io.inStep(2) -> (io.vs1Data >> 3*ELEN/4),
                     io.inStep(3) -> (io.vs1Data >> 4*ELEN/4),
                     io.inStep(4) -> (io.vs1Data >> 5*ELEN/4),
                     io.inStep(5) -> (io.vs1Data >> 6*ELEN/4),
                     io.inStep(6) -> (io.vs1Data >> 7*ELEN/4)))

  val vs2Data0     = Wire(UInt(ELEN.W))
  val vs2Data1     = Wire(UInt(ELEN.W))
  val vs2Data2     = Wire(UInt(ELEN.W))
  val needvs2Data1 = !io.isSrc22SEW && io.isVd2SEW || io.isFullMul
  val needvs2Data2 = !io.isSrc22SEW && io.isVd2SEW && io.isFullMul
  val vs2Data      = Mux(needvs2Data2, vs2Data2, Mux(needvs2Data1, vs2Data1, vs2Data0))
  vs2Data0 := MuxCase(io.vs2Data,
               Array(io.inStep(0) -> (io.vs2Data >> ELEN),
                     io.inStep(1) -> (io.vs2Data >> 2*ELEN),
                     io.inStep(2) -> (io.vs2Data >> 3*ELEN),
                     io.inStep(3) -> (io.vs2Data >> 4*ELEN),
                     io.inStep(4) -> (io.vs2Data >> 5*ELEN),
                     io.inStep(5) -> (io.vs2Data >> 6*ELEN),
                     io.inStep(6) -> (io.vs2Data >> 7*ELEN)))
  vs2Data1 := MuxCase(io.vs2Data,
               Array(io.inStep(0) -> (io.vs2Data >> ELEN/2),
                     io.inStep(1) -> (io.vs2Data >> 2*ELEN/2),
                     io.inStep(2) -> (io.vs2Data >> 3*ELEN/2),
                     io.inStep(3) -> (io.vs2Data >> 4*ELEN/2),
                     io.inStep(4) -> (io.vs2Data >> 5*ELEN/2),
                     io.inStep(5) -> (io.vs2Data >> 6*ELEN/2),
                     io.inStep(6) -> (io.vs2Data >> 7*ELEN/2)))
  vs2Data2 := MuxCase(io.vs2Data,
               Array(io.inStep(0) -> (io.vs2Data >> ELEN/4),
                     io.inStep(1) -> (io.vs2Data >> 2*ELEN/4),
                     io.inStep(2) -> (io.vs2Data >> 3*ELEN/4),
                     io.inStep(3) -> (io.vs2Data >> 4*ELEN/4),
                     io.inStep(4) -> (io.vs2Data >> 5*ELEN/4),
                     io.inStep(5) -> (io.vs2Data >> 6*ELEN/4),
                     io.inStep(6) -> (io.vs2Data >> 7*ELEN/4)))

  val vdData  = Wire(UInt(ELEN.W))
  vdData := MuxCase(io.vdvs3Data,
              Array(io.inStep(0) -> (io.vdvs3Data >> ELEN),
                    io.inStep(1) -> (io.vdvs3Data >> 2*ELEN),
                    io.inStep(2) -> (io.vdvs3Data >> 3*ELEN),
                    io.inStep(3) -> (io.vdvs3Data >> 4*ELEN),
                    io.inStep(4) -> (io.vdvs3Data >> 5*ELEN),
                    io.inStep(5) -> (io.vdvs3Data >> 6*ELEN),
                    io.inStep(6) -> (io.vdvs3Data >> 7*ELEN)))

//split vs1Data into SEW width vectors
  for(i <- 0 until e8Depth)    io.vs1e.e8(i)      := vs1Data(8*(i+1)-1, 8*i)
  for(i <- 0 until e16Depth)   io.vs1e.e16(i)     := vs1Data(16*(i+1)-1, 16*i)
  for(i <- 0 until e32Depth)   io.vs1e.e32(i)     := vs1Data(32*(i+1)-1, 32*i)
  for(i <- 0 until e64Depth)   io.vs1e.e64(i)     := vs1Data(64*(i+1)-1, 64*i)
  for(i <- 0 until e128Depth)  io.vs1e.e128(i)    := vs1Data(128*(i+1)-1, 128*i)
  for(i <- 0 until e256Depth)  io.vs1e.e256(i)    := vs1Data(256*(i+1)-1, 256*i)
  for(i <- 0 until e512Depth)  io.vs1e.e512(i)    := vs1Data(512*(i+1)-1, 512*i)
  for(i <- 0 until e1024Depth) io.vs1e.e1024(i)   := vs1Data(1024*(i+1)-1, 1024*i)
//split vs2Data into SEW width vectors
  for(i <- 0 until e8Depth)    io.vs2e.e8(i)      := vs2Data(8*(i+1)-1, 8*i)
  for(i <- 0 until e16Depth)   io.vs2e.e16(i)     := vs2Data(16*(i+1)-1, 16*i)
  for(i <- 0 until e32Depth)   io.vs2e.e32(i)     := vs2Data(32*(i+1)-1, 32*i)
  for(i <- 0 until e64Depth)   io.vs2e.e64(i)     := vs2Data(64*(i+1)-1, 64*i)
  for(i <- 0 until e128Depth)  io.vs2e.e128(i)    := vs2Data(128*(i+1)-1, 128*i)
  for(i <- 0 until e256Depth)  io.vs2e.e256(i)    := vs2Data(256*(i+1)-1, 256*i)
  for(i <- 0 until e512Depth)  io.vs2e.e512(i)    := vs2Data(512*(i+1)-1, 512*i)
  for(i <- 0 until e1024Depth) io.vs2e.e1024(i)   := vs2Data(1024*(i+1)-1, 1024*i)
//split vdvs3Data into SEW width vectors
  for(i <- 0 until e8Depth)    io.vdvs3e.e8(i)    := vdData(8*(i+1)-1, 8*i)
  for(i <- 0 until e16Depth)   io.vdvs3e.e16(i)   := vdData(16*(i+1)-1, 16*i)
  for(i <- 0 until e32Depth)   io.vdvs3e.e32(i)   := vdData(32*(i+1)-1, 32*i)
  for(i <- 0 until e64Depth)   io.vdvs3e.e64(i)   := vdData(64*(i+1)-1, 64*i)
  for(i <- 0 until e128Depth)  io.vdvs3e.e128(i)  := vdData(128*(i+1)-1, 128*i)
  for(i <- 0 until e256Depth)  io.vdvs3e.e256(i)  := vdData(256*(i+1)-1, 256*i)
  for(i <- 0 until e512Depth)  io.vdvs3e.e512(i)  := vdData(512*(i+1)-1, 512*i)
  for(i <- 0 until e1024Depth) io.vdvs3e.e1024(i) := vdData(1024*(i+1)-1, 1024*i)

//split vs1Data into SEW width vectors
  for(i <- 0 until E8Depth)    io.vs1E.e8(i)      := io.vs1Data(8*(i+1)-1, 8*i)
  for(i <- 0 until E16Depth)   io.vs1E.e16(i)     := io.vs1Data(16*(i+1)-1, 16*i)
  for(i <- 0 until E32Depth)   io.vs1E.e32(i)     := io.vs1Data(32*(i+1)-1, 32*i)
  for(i <- 0 until E64Depth)   io.vs1E.e64(i)     := io.vs1Data(64*(i+1)-1, 64*i)
  for(i <- 0 until E128Depth)  io.vs1E.e128(i)    := io.vs1Data(128*(i+1)-1, 128*i)
  for(i <- 0 until E256Depth)  io.vs1E.e256(i)    := io.vs1Data(256*(i+1)-1, 256*i)
  for(i <- 0 until E512Depth)  io.vs1E.e512(i)    := io.vs1Data(512*(i+1)-1, 512*i)
  for(i <- 0 until E1024Depth) io.vs1E.e1024(i)   := io.vs1Data(1024*(i+1)-1, 1024*i)
//split vs2Data into SEW width vectors
  for(i <- 0 until E8Depth)    io.vs2E.e8(i)      := io.vs2Data(8*(i+1)-1, 8*i)
  for(i <- 0 until E16Depth)   io.vs2E.e16(i)     := io.vs2Data(16*(i+1)-1, 16*i)
  for(i <- 0 until E32Depth)   io.vs2E.e32(i)     := io.vs2Data(32*(i+1)-1, 32*i)
  for(i <- 0 until E64Depth)   io.vs2E.e64(i)     := io.vs2Data(64*(i+1)-1, 64*i)
  for(i <- 0 until E128Depth)  io.vs2E.e128(i)    := io.vs2Data(128*(i+1)-1, 128*i)
  for(i <- 0 until E256Depth)  io.vs2E.e256(i)    := io.vs2Data(256*(i+1)-1, 256*i)
  for(i <- 0 until E512Depth)  io.vs2E.e512(i)    := io.vs2Data(512*(i+1)-1, 512*i)
  for(i <- 0 until E1024Depth) io.vs2E.e1024(i)   := io.vs2Data(1024*(i+1)-1, 1024*i)
//split vdvs3Data into SEW width vectors
  for(i <- 0 until E8Depth)    io.vdvs3E.e8(i)    := io.vdvs3Data(8*(i+1)-1, 8*i)
  for(i <- 0 until E16Depth)   io.vdvs3E.e16(i)   := io.vdvs3Data(16*(i+1)-1, 16*i)
  for(i <- 0 until E32Depth)   io.vdvs3E.e32(i)   := io.vdvs3Data(32*(i+1)-1, 32*i)
  for(i <- 0 until E64Depth)   io.vdvs3E.e64(i)   := io.vdvs3Data(64*(i+1)-1, 64*i)
  for(i <- 0 until E128Depth)  io.vdvs3E.e128(i)  := io.vdvs3Data(128*(i+1)-1, 128*i)
  for(i <- 0 until E256Depth)  io.vdvs3E.e256(i)  := io.vdvs3Data(256*(i+1)-1, 256*i)
  for(i <- 0 until E512Depth)  io.vdvs3E.e512(i)  := io.vdvs3Data(512*(i+1)-1, 512*i)
  for(i <- 0 until E1024Depth) io.vdvs3E.e1024(i) := io.vdvs3Data(1024*(i+1)-1, 1024*i)
//split vs1Data into MLEN width vectors
  for(i <- 0 until m1Depth)    io.vs1m.m1(i)      := io.vs1Data(i)
  for(i <- 0 until m2Depth)    io.vs1m.m2(i)      := io.vs1Data(2*i)
  for(i <- 0 until m4Depth)    io.vs1m.m4(i)      := io.vs1Data(4*i)
  for(i <- 0 until m8Depth)    io.vs1m.m8(i)      := io.vs1Data(8*i)
  for(i <- 0 until m16Depth)   io.vs1m.m16(i)     := io.vs1Data(16*i)
  for(i <- 0 until m32Depth)   io.vs1m.m32(i)     := io.vs1Data(32*i)
  for(i <- 0 until m64Depth)   io.vs1m.m64(i)     := io.vs1Data(64*i)
  for(i <- 0 until m128Depth)  io.vs1m.m128(i)    := io.vs1Data(128*i)
  for(i <- 0 until m256Depth)  io.vs1m.m256(i)    := io.vs1Data(256*i)
  for(i <- 0 until m512Depth)  io.vs1m.m512(i)    := io.vs1Data(512*i)
  for(i <- 0 until m1024Depth) io.vs1m.m1024(i)   := io.vs1Data(1024*i)
//split vs2Data into MLEN width vectors
  for(i <- 0 until m1Depth)    io.vs2m.m1(i)      := io.vs2Data(i)
  for(i <- 0 until m2Depth)    io.vs2m.m2(i)      := io.vs2Data(2*i)
  for(i <- 0 until m4Depth)    io.vs2m.m4(i)      := io.vs2Data(4*i)
  for(i <- 0 until m8Depth)    io.vs2m.m8(i)      := io.vs2Data(8*i)
  for(i <- 0 until m16Depth)   io.vs2m.m16(i)     := io.vs2Data(16*i)
  for(i <- 0 until m32Depth)   io.vs2m.m32(i)     := io.vs2Data(32*i)
  for(i <- 0 until m64Depth)   io.vs2m.m64(i)     := io.vs2Data(64*i)
  for(i <- 0 until m128Depth)  io.vs2m.m128(i)    := io.vs2Data(128*i)
  for(i <- 0 until m256Depth)  io.vs2m.m256(i)    := io.vs2Data(256*i)
  for(i <- 0 until m512Depth)  io.vs2m.m512(i)    := io.vs2Data(512*i)
  for(i <- 0 until m1024Depth) io.vs2m.m1024(i)   := io.vs2Data(1024*i)
//split vdvs3Data into MLEN width vectors
  for(i <- 0 until m1Depth)    io.vdm.m1(i)       := io.vdvs3Data(i)
  for(i <- 0 until m2Depth)    io.vdm.m2(i)       := io.vdvs3Data(2*i)
  for(i <- 0 until m4Depth)    io.vdm.m4(i)       := io.vdvs3Data(4*i)
  for(i <- 0 until m8Depth)    io.vdm.m8(i)       := io.vdvs3Data(8*i)
  for(i <- 0 until m16Depth)   io.vdm.m16(i)      := io.vdvs3Data(16*i)
  for(i <- 0 until m32Depth)   io.vdm.m32(i)      := io.vdvs3Data(32*i)
  for(i <- 0 until m64Depth)   io.vdm.m64(i)      := io.vdvs3Data(64*i)
  for(i <- 0 until m128Depth)  io.vdm.m128(i)     := io.vdvs3Data(128*i)
  for(i <- 0 until m256Depth)  io.vdm.m256(i)     := io.vdvs3Data(256*i)
  for(i <- 0 until m512Depth)  io.vdm.m512(i)     := io.vdvs3Data(512*i)
  for(i <- 0 until m1024Depth) io.vdm.m1024(i)    := io.vdvs3Data(1024*i)
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
  
//split each element of vs1e vector into SEW/EDIV width vectors
  for(j <- 0 until 2){
    for(i <- 0 until e8Depth)    io.vs1d.e8(i).d2(j)    := io.vs1e.e8(i)((8/2)*(j+1)-1,(8/2)*j)          
    for(i <- 0 until e16Depth)   io.vs1d.e16(i).d2(j)   := io.vs1e.e16(i)((16/2)*(j+1)-1,(16/2)*j)       
    for(i <- 0 until e32Depth)   io.vs1d.e32(i).d2(j)   := io.vs1e.e32(i)((32/2)*(j+1)-1,(32/2)*j)       
    for(i <- 0 until e64Depth)   io.vs1d.e64(i).d2(j)   := io.vs1e.e64(i)((64/2)*(j+1)-1,(64/2)*j)       
    for(i <- 0 until e128Depth)  io.vs1d.e128(i).d2(j)  := io.vs1e.e128(i)((128/2)*(j+1)-1,(128/2)*j)    
    for(i <- 0 until e256Depth)  io.vs1d.e256(i).d2(j)  := io.vs1e.e256(i)((256/2)*(j+1)-1,(256/2)*j)    
    for(i <- 0 until e512Depth)  io.vs1d.e512(i).d2(j)  := io.vs1e.e512(i)((512/2)*(j+1)-1,(512/2)*j)    
    for(i <- 0 until e1024Depth) io.vs1d.e1024(i).d2(j) := io.vs1e.e1024(i)((1024/2)*(j+1)-1,(1024/2)*j) 
  }
  for(j <- 0 until 4){
    for(i <- 0 until e8Depth)    io.vs1d.e8(i).d4(j)    := io.vs1e.e8(i)((8/4)*(j+1)-1,(8/4)*j)          
    for(i <- 0 until e16Depth)   io.vs1d.e16(i).d4(j)   := io.vs1e.e16(i)((16/4)*(j+1)-1,(16/4)*j)       
    for(i <- 0 until e32Depth)   io.vs1d.e32(i).d4(j)   := io.vs1e.e32(i)((32/4)*(j+1)-1,(32/4)*j)       
    for(i <- 0 until e64Depth)   io.vs1d.e64(i).d4(j)   := io.vs1e.e64(i)((64/4)*(j+1)-1,(64/4)*j)       
    for(i <- 0 until e128Depth)  io.vs1d.e128(i).d4(j)  := io.vs1e.e128(i)((128/4)*(j+1)-1,(128/4)*j)    
    for(i <- 0 until e256Depth)  io.vs1d.e256(i).d4(j)  := io.vs1e.e256(i)((256/4)*(j+1)-1,(256/4)*j)    
    for(i <- 0 until e512Depth)  io.vs1d.e512(i).d4(j)  := io.vs1e.e512(i)((512/4)*(j+1)-1,(512/4)*j)    
    for(i <- 0 until e1024Depth) io.vs1d.e1024(i).d4(j) := io.vs1e.e1024(i)((1024/4)*(j+1)-1,(1024/4)*j) 
  }
  for(j <- 0 until 8){
    for(i <- 0 until e8Depth)    io.vs1d.e8(i).d8(j)    := io.vs1e.e8(i)((8/8)*(j+1)-1,(8/8)*j)          
    for(i <- 0 until e16Depth)   io.vs1d.e16(i).d8(j)   := io.vs1e.e16(i)((16/8)*(j+1)-1,(16/8)*j)       
    for(i <- 0 until e32Depth)   io.vs1d.e32(i).d8(j)   := io.vs1e.e32(i)((32/8)*(j+1)-1,(32/8)*j)       
    for(i <- 0 until e64Depth)   io.vs1d.e64(i).d8(j)   := io.vs1e.e64(i)((64/8)*(j+1)-1,(64/8)*j)       
    for(i <- 0 until e128Depth)  io.vs1d.e128(i).d8(j)  := io.vs1e.e128(i)((128/8)*(j+1)-1,(128/8)*j)    
    for(i <- 0 until e256Depth)  io.vs1d.e256(i).d8(j)  := io.vs1e.e256(i)((256/8)*(j+1)-1,(256/8)*j)    
    for(i <- 0 until e512Depth)  io.vs1d.e512(i).d8(j)  := io.vs1e.e512(i)((512/8)*(j+1)-1,(512/8)*j)    
    for(i <- 0 until e1024Depth) io.vs1d.e1024(i).d8(j) := io.vs1e.e1024(i)((1024/8)*(j+1)-1,(1024/8)*j) 
  }
//split each element of vs2e vector into SEW/EDIV width vectors
  for(j <- 0 until 2){
    for(i <- 0 until e8Depth)    io.vs2d.e8(i).d2(j)    := io.vs2e.e8(i)((8/2)*(j+1)-1,(8/2)*j)          
    for(i <- 0 until e16Depth)   io.vs2d.e16(i).d2(j)   := io.vs2e.e16(i)((16/2)*(j+1)-1,(16/2)*j)       
    for(i <- 0 until e32Depth)   io.vs2d.e32(i).d2(j)   := io.vs2e.e32(i)((32/2)*(j+1)-1,(32/2)*j)       
    for(i <- 0 until e64Depth)   io.vs2d.e64(i).d2(j)   := io.vs2e.e64(i)((64/2)*(j+1)-1,(64/2)*j)       
    for(i <- 0 until e128Depth)  io.vs2d.e128(i).d2(j)  := io.vs2e.e128(i)((128/2)*(j+1)-1,(128/2)*j)    
    for(i <- 0 until e256Depth)  io.vs2d.e256(i).d2(j)  := io.vs2e.e256(i)((256/2)*(j+1)-1,(256/2)*j)    
    for(i <- 0 until e512Depth)  io.vs2d.e512(i).d2(j)  := io.vs2e.e512(i)((512/2)*(j+1)-1,(512/2)*j)    
    for(i <- 0 until e1024Depth) io.vs2d.e1024(i).d2(j) := io.vs2e.e1024(i)((1024/2)*(j+1)-1,(1024/2)*j) 
  }
  for(j <- 0 until 4){
    for(i <- 0 until e8Depth)    io.vs2d.e8(i).d4(j)    := io.vs2e.e8(i)((8/4)*(j+1)-1,(8/4)*j)          
    for(i <- 0 until e16Depth)   io.vs2d.e16(i).d4(j)   := io.vs2e.e16(i)((16/4)*(j+1)-1,(16/4)*j)       
    for(i <- 0 until e32Depth)   io.vs2d.e32(i).d4(j)   := io.vs2e.e32(i)((32/4)*(j+1)-1,(32/4)*j)       
    for(i <- 0 until e64Depth)   io.vs2d.e64(i).d4(j)   := io.vs2e.e64(i)((64/4)*(j+1)-1,(64/4)*j)       
    for(i <- 0 until e128Depth)  io.vs2d.e128(i).d4(j)  := io.vs2e.e128(i)((128/4)*(j+1)-1,(128/4)*j)    
    for(i <- 0 until e256Depth)  io.vs2d.e256(i).d4(j)  := io.vs2e.e256(i)((256/4)*(j+1)-1,(256/4)*j)    
    for(i <- 0 until e512Depth)  io.vs2d.e512(i).d4(j)  := io.vs2e.e512(i)((512/4)*(j+1)-1,(512/4)*j)    
    for(i <- 0 until e1024Depth) io.vs2d.e1024(i).d4(j) := io.vs2e.e1024(i)((1024/4)*(j+1)-1,(1024/4)*j) 
  }
  for(j <- 0 until 8){
    for(i <- 0 until e8Depth)    io.vs2d.e8(i).d8(j)    := io.vs2e.e8(i)((8/8)*(j+1)-1,(8/8)*j)          
    for(i <- 0 until e16Depth)   io.vs2d.e16(i).d8(j)   := io.vs2e.e16(i)((16/8)*(j+1)-1,(16/8)*j)       
    for(i <- 0 until e32Depth)   io.vs2d.e32(i).d8(j)   := io.vs2e.e32(i)((32/8)*(j+1)-1,(32/8)*j)       
    for(i <- 0 until e64Depth)   io.vs2d.e64(i).d8(j)   := io.vs2e.e64(i)((64/8)*(j+1)-1,(64/8)*j)       
    for(i <- 0 until e128Depth)  io.vs2d.e128(i).d8(j)  := io.vs2e.e128(i)((128/8)*(j+1)-1,(128/8)*j)    
    for(i <- 0 until e256Depth)  io.vs2d.e256(i).d8(j)  := io.vs2e.e256(i)((256/8)*(j+1)-1,(256/8)*j)    
    for(i <- 0 until e512Depth)  io.vs2d.e512(i).d8(j)  := io.vs2e.e512(i)((512/8)*(j+1)-1,(512/8)*j)    
    for(i <- 0 until e1024Depth) io.vs2d.e1024(i).d8(j) := io.vs2e.e1024(i)((1024/8)*(j+1)-1,(1024/8)*j) 
  }


}
