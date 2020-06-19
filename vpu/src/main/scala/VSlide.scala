// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VSlide.scala
*       Author          :       yexc, liangzh
*       Revision        :       2019/10/08
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       implementation of vslideup, vslidedown, vslide1up, vslide1down insts
*
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, only the first elements of vectors are usefull
*       io.vs2Data      :       input[8VLEN-1:0], data, to be shift data, to be shifted serval elements's width
*       io.vl           :       input[log2Ceil(VLEN):0], data, showing expected number of elements
*       io.vsew         :       input[VSEW_SZ-1:0], data, to form shift amount
*       io.vlmul        :       input[VLMUL_SZ-1:0], data, to form edge
*       io.majFun       :       input[MajFun_SZ-1:0], control, showing function is vslide or vslide1
*       io.slideFun     :       input[SlideFun_SZ-1:0], control, showing function: slidedown or slideup
*       io.vSlideOut    :       output, bundle of vectors, data, SEW relative, results
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VSlide(params: VPUParams) extends VModule(params) {
  val vlWidth = log2Ceil(VLEN)+1
  val shiftWidth = log2Ceil(VLEN*8)

  val io = IO(new Bundle {
    val vsrc1e     = Input(new SEWVec)
    val vs2Data    = Input(UInt((VLEN*8).W))
    val vl         = Input(UInt(vlWidth.W))
    val vsew       = Input(UInt(VSEW_SZ.W))
    val vlmul      = Input(UInt(VLMUL_SZ.W))
    val majFun     = Input(UInt(MajFun_SZ.W))
    val slideFun   = Input(UInt(SlideFun_SZ.W))
    val vSlideOut  = Output(new FullSEWVec)
  })

  val isSlideUp = io.slideFun === SlideFun_Up
  val isSlideDn = io.slideFun === SlideFun_Dn
  val isSlide1  = io.majFun === IsSlide1

  val slideENum = if(XLEN == 64) Mux(isSlide1, 1.U(64.W), io.vsrc1e.e64(0))
                  else           Mux(isSlide1, 1.U(32.W), io.vsrc1e.e32(0))
  val offset    = (slideENum << (io.vsew +& 3.U(3.W)))(shiftWidth,0)
  val vlBits    = (io.vl << (io.vsew +& 3.U(3.W)))(shiftWidth,0)
  val vlm1Bits  = ((io.vl-1.U) << (io.vsew +& 3.U(3.W)))(shiftWidth,0)
  val vlmaxBits = 1.U << (io.vlmul +& log2Ceil(VLEN).U)
  val edge      = Mux(io.slideFun === SlideFun_Dn && isSlide1, vlBits, vlmaxBits)
  val mask      = (~(0.U((VLEN*8).W))) >> ((VLEN*8).U - edge)
  val toShiftData = Mux(isSlideDn, io.vs2Data & mask, io.vs2Data)
  val shiftedData = Mux(isSlideDn, toShiftData >> offset, toShiftData << offset)

  val toHeadData = MuxCase(io.vsrc1e.e8(0),
                    Array((io.vsew === HWordWidth) -> io.vsrc1e.e16(0),
                          (io.vsew === WordWidth)  -> io.vsrc1e.e32(0))
                   ++ (if(ELEN >= 64)   Array((io.vsew === DWordWidth) -> io.vsrc1e.e64(0))   else Nil)
                   ++ (if(ELEN >= 128)  Array((io.vsew === QWordWidth) -> io.vsrc1e.e128(0))  else Nil)
                   ++ (if(ELEN >= 256)  Array((io.vsew === OWordWidth) -> io.vsrc1e.e256(0))  else Nil)
                   ++ (if(ELEN >= 512)  Array((io.vsew === SWordWidth) -> io.vsrc1e.e512(0))  else Nil)
                   ++ (if(ELEN == 1024) Array((io.vsew === TWordWidth) -> io.vsrc1e.e1024(0)) else Nil))

  val toTailData = toHeadData << vlm1Bits

  val slide1Data = Mux(isSlideUp && isSlide1, shiftedData | toHeadData, shiftedData | toTailData)
  val slideData  = Mux(isSlide1, slide1Data, shiftedData)

  for(i <- 0 until E8Depth)    io.vSlideOut.e8(i)    := slideData((i+1)*8-1,    i*8)
  for(i <- 0 until E16Depth)   io.vSlideOut.e16(i)   := slideData((i+1)*16-1,   i*16)
  for(i <- 0 until E32Depth)   io.vSlideOut.e32(i)   := slideData((i+1)*32-1,   i*32)
  for(i <- 0 until E64Depth)   io.vSlideOut.e64(i)   := slideData((i+1)*64-1,   i*64)
  for(i <- 0 until E128Depth)  io.vSlideOut.e128(i)  := slideData((i+1)*128-1,  i*128)
  for(i <- 0 until E256Depth)  io.vSlideOut.e256(i)  := slideData((i+1)*256-1,  i*256)
  for(i <- 0 until E512Depth)  io.vSlideOut.e512(i)  := slideData((i+1)*512-1,  i*512)
  for(i <- 0 until E1024Depth) io.vSlideOut.e1024(i) := slideData((i+1)*1024-1, i*1024)

}


