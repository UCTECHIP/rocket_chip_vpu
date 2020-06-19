// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFunSelBack.scala
*       Author          :       liangzh
*       Revision        :       2020/04/05
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       select function modules output
*
*       io.majFun       :       input[MajFun_SZ-1:0], control, select module result
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, select data in converting SEW relative bundle to MLEN relative bundle
*       io.vSEWData     :       input[8*VLEN-1:0], data, output of buffer storing rolling calculate results
*       io.vIotaOut     :       input, bundle of vectors, data, SEW relative, results from VIota module
*       io.vIndexOut    :       input, bundle of vectors, data, SEW relative, results from VIndex module
*       io.vSlideOut    :       input, bundle of vectors, data, SEW relative, results from VSlide module
*       io.vCompressOut :       input, bundle of vectors, data, SEW relative, results from VCompress module
*       io.vFromXOut    :       input, bundle of vectors, data, SEW relative, results from VScalarMove module
*       io.vFromFOut    :       input, bundle of vectors, data, SEW relative, results from VScalarMove module
*       io.vMSetOut     :       input, bundle of vectors, data, MLEN relative, results from VMinIndex module
*       io.vMBitwise    :       input, bundle of vectors, data, MLEN relative, results from VBitwise module
*       io.vPopcOut     :       input[XLEN-1:0], data, result from VPopc module
*       io.vMinIndex    :       input[XLEN-1:0], data, result from VMinIndex module
*       io.vXtoXData    :       input[XLEN-1:0], data, result from VScalarMove module
*       io.vFtoXData    :       input[XLEN-1:0], data, result from VScalarMove module
*       io.vFtoFData    :       input[FLEN:0], data, result from VScalarMove module
*       io.vSEWOut      :       output, bundle of vectors, data, SEW relative, selected output
*       io.vMLENOut     :       output, bundle of vectors, data, MLEN relative, selected output
*       io.scalarXOut   :       output[XLEN-1:0], data, selected output
*       io.scalarFOut   :       output[FLEN:0], data, selected output
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VFunSelBack(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val majFun       = Input(UInt(MajFun_SZ.W))
    val vlmul        = Input(UInt(VLMUL_SZ.W))

    val vSEWData     = Input(UInt((8*VLEN).W))

    val vIotaOut     = Input(new FullSEWVec)
    val vIndexOut    = Input(new FullSEWVec)
    val vSlideOut    = Input(new FullSEWVec)
    val vCompressOut = Input(new FullSEWVec)
    val vFromXOut    = Input(new FullSEWVec)

    val vFromFOut    = Input(new FullFSEWVec)

    val vMSetOut     = Input(new MLENVec)
    val vMBitwise    = Input(new MLENVec)

    val vPopcOut     = Input(UInt(XLEN.W))
    val vMinIndex    = Input(UInt(XLEN.W))
    val vXToXData    = Input(UInt(XLEN.W))
    val vFToXData    = Input(UInt(XLEN.W))

    val vFToFData    = Input(UInt((FLEN+1).W))

    val vSEWOut      = Output(new FullSEWVec)
    val vMLENOut     = Output(new MLENVec)
    val scalarXOut   = Output(UInt(XLEN.W))
    val scalarFOut   = Output(UInt((FLEN+1).W))
  })

//////////////////////////separate elements///////////////////////////
  val vFullSEWOut = Wire(new FullSEWVec)
  for(i <- 0 until E8Depth)    vFullSEWOut.e8(i)    := io.vSEWData(8*(i+1)-1, 8*i)
  for(i <- 0 until E16Depth)   vFullSEWOut.e16(i)   := io.vSEWData(16*(i+1)-1, 16*i)
  for(i <- 0 until E32Depth)   vFullSEWOut.e32(i)   := io.vSEWData(32*(i+1)-1, 32*i)
  for(i <- 0 until E64Depth)   vFullSEWOut.e64(i)   := io.vSEWData(64*(i+1)-1, 64*i)
  for(i <- 0 until E128Depth)  vFullSEWOut.e128(i)  := io.vSEWData(128*(i+1)-1, 128*i)
  for(i <- 0 until E256Depth)  vFullSEWOut.e256(i)  := io.vSEWData(256*(i+1)-1, 256*i)
  for(i <- 0 until E512Depth)  vFullSEWOut.e512(i)  := io.vSEWData(512*(i+1)-1, 512*i)
  for(i <- 0 until E1024Depth) vFullSEWOut.e1024(i) := io.vSEWData(1024*(i+1)-1, 1024*i)

  val vSEWtoMLEN = Wire(new MLENVec)
  vSEWtoMLEN.m1 := vFullSEWOut.e8
  for(i <- 0 until m2Depth)
    vSEWtoMLEN.m2(i) := MuxCase(0.U(1.W),
                         Array((io.vlmul === QuadReg) -> vFullSEWOut.e8(i),
                               (io.vlmul === OctuReg) -> vFullSEWOut.e16(i)))
  for(i <- 0 until m4Depth)
    vSEWtoMLEN.m4(i) := MuxCase(0.U(1.W),
                         Array((io.vlmul === DoubReg) -> vFullSEWOut.e8(i),
                               (io.vlmul === QuadReg) -> vFullSEWOut.e16(i),
                               (io.vlmul === OctuReg) -> vFullSEWOut.e32(i)))
  for(i <- 0 until m8Depth)
    vSEWtoMLEN.m8(i) := MuxCase(0.U(1.W),
                         Array((io.vlmul === SingReg) -> vFullSEWOut.e8(i),
                               (io.vlmul === DoubReg) -> vFullSEWOut.e16(i),
                               (io.vlmul === QuadReg) -> vFullSEWOut.e32(i))
                        ++ (if(ELEN >= 64) Array((io.vlmul === OctuReg) -> vFullSEWOut.e64(i)) else Nil))
  for(i <- 0 until m16Depth)
    vSEWtoMLEN.m16(i) := MuxCase(0.U(1.W),
                          Array((io.vlmul === SingReg) -> vFullSEWOut.e16(i),
                                (io.vlmul === DoubReg) -> vFullSEWOut.e32(i))
                         ++ (if(ELEN >= 64)  Array((io.vlmul === QuadReg) -> vFullSEWOut.e64(i))  else Nil)
                         ++ (if(ELEN >= 128) Array((io.vlmul === OctuReg) -> vFullSEWOut.e128(i)) else Nil))
  for(i <- 0 until m32Depth)
    vSEWtoMLEN.m32(i) := MuxCase(0.U(1.W),
                          Array((io.vlmul === SingReg) -> vFullSEWOut.e32(i))
                          ++ (if(ELEN >= 64)  Array((io.vlmul === DoubReg) -> vFullSEWOut.e64(i))  else Nil)
                          ++ (if(ELEN >= 128) Array((io.vlmul === QuadReg) -> vFullSEWOut.e128(i)) else Nil)
                          ++ (if(ELEN >= 256) Array((io.vlmul === OctuReg) -> vFullSEWOut.e256(i)) else Nil))
  for(i <- 0 until m64Depth)
    vSEWtoMLEN.m64(i) := MuxCase(0.U(1.W), 
                          Array((io.vlmul === SingReg) -> vFullSEWOut.e64(i))
                         ++ (if(ELEN >= 128) Array((io.vlmul === DoubReg) -> vFullSEWOut.e128(i)) else Nil)
                         ++ (if(ELEN >= 256) Array((io.vlmul === QuadReg) -> vFullSEWOut.e256(i)) else Nil)
                         ++ (if(ELEN >= 512) Array((io.vlmul === OctuReg) -> vFullSEWOut.e512(i)) else Nil))
  for(i <- 0 until m128Depth)
    vSEWtoMLEN.m128(i) := MuxCase(0.U(1.W), 
                           Array((io.vlmul === SingReg) -> vFullSEWOut.e128(i))
                          ++ (if(ELEN >= 256)  Array((io.vlmul === DoubReg) -> vFullSEWOut.e256(i))  else Nil)
                          ++ (if(ELEN >= 512)  Array((io.vlmul === QuadReg) -> vFullSEWOut.e512(i))  else Nil)
                          ++ (if(ELEN == 1024) Array((io.vlmul === OctuReg) -> vFullSEWOut.e1024(i)) else Nil))
  for(i <- 0 until m256Depth)
    vSEWtoMLEN.m256(i) := MuxCase(0.U(1.W), 
                           Array((io.vlmul === SingReg) -> vFullSEWOut.e256(i))
                          ++ (if(ELEN >= 512)  Array((io.vlmul === DoubReg) -> vFullSEWOut.e512(i))  else Nil)
                          ++ (if(ELEN == 1024) Array((io.vlmul === QuadReg) -> vFullSEWOut.e1024(i)) else Nil))
  for(i <- 0 until m512Depth)
    vSEWtoMLEN.m512(i) := MuxCase(0.U(1.W), 
                           Array((io.vlmul === SingReg) -> vFullSEWOut.e512(i))
                          ++ (if(ELEN == 1024) Array((io.vlmul === DoubReg) -> vFullSEWOut.e1024(i)) else Nil))
  for(i <- 0 until m1024Depth)
    vSEWtoMLEN.m1024(i) := vFullSEWOut.e1024(i)
//////////////////////////separate elements///////////////////////////

//////////////////////////function selection//////////////////////////
  val vCompressOut = Wire(new FullSEWVec)
  val vFromFOut    = Wire(new FullSEWVec)
  val needSlideOut = io.majFun === IsSlide || io.majFun === IsSlide1

                   vCompressOut.e8    := io.vCompressOut.e8
                   vCompressOut.e16   := vFullSEWOut.e16
                   vCompressOut.e32   := vFullSEWOut.e32
  if(ELEN >= 64)   vCompressOut.e64   := vFullSEWOut.e64
  if(ELEN >= 128)  vCompressOut.e128  := vFullSEWOut.e128
  if(ELEN >= 256)  vCompressOut.e256  := vFullSEWOut.e256
  if(ELEN >= 512)  vCompressOut.e512  := vFullSEWOut.e512
  if(ELEN >= 1024) vCompressOut.e1024 := vFullSEWOut.e1024

                   vFromFOut.e8    :=                                            vFullSEWOut.e8
                   vFromFOut.e16   := (if(FSEW16)         io.vFromFOut.f16  else vFullSEWOut.e16)
                   vFromFOut.e32   :=                     io.vFromFOut.f32
  if(ELEN >= 64)   vFromFOut.e64   := (if(FSEWMAX >= 64)  io.vFromFOut.f64  else vFullSEWOut.e64)
  if(ELEN >= 128)  vFromFOut.e128  := (if(FSEWMAX == 128) io.vFromFOut.f128 else vFullSEWOut.e128)
  if(ELEN >= 256)  vFromFOut.e256  :=                                            vFullSEWOut.e256
  if(ELEN >= 512)  vFromFOut.e512  :=                                            vFullSEWOut.e512
  if(ELEN >= 1024) vFromFOut.e1024 :=                                            vFullSEWOut.e1024

  io.vSEWOut := MuxCase(vFullSEWOut,
                  Array((io.majFun === IsIota)  -> io.vIotaOut,
                        (io.majFun === IsIdx)   -> io.vIndexOut)
                ++ (if(SLIDE)    Array(needSlideOut            -> io.vSlideOut) else Nil)
                ++ (if(MV)       Array((io.majFun === IsMv)    -> io.vFromXOut) else Nil)
                ++ (if(COMPRESS) Array((io.majFun === IsZip)   -> vCompressOut) else Nil)
                ++ (if(FMV)      Array((io.majFun === IsFMv)   -> vFromFOut)    else Nil))

  io.vMLENOut := MuxCase(vSEWtoMLEN,
                   Array((io.majFun === IsMIdx) -> io.vMSetOut,
                         (io.majFun === IsMBit) -> io.vMBitwise))

  io.scalarXOut := MuxCase(0.U(XLEN.W),
                    Array((io.majFun === IsPopc)  -> io.vPopcOut,
                          (io.majFun === IsFirst) -> io.vMinIndex)
                   ++ (if(MV)           Array((io.majFun === IsMv)  -> io.vXToXData) else Nil)
                   ++ (if(FMV && ZFINX) Array((io.majFun === IsFMv) -> io.vFToXData) else Nil))

  io.scalarFOut := io.vFToFData
//////////////////////////function selection//////////////////////////
}
