// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VSEWtoMLEN.scala
*       Author          :       liangzh
*       Revision        :       2020/03/30
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       convert SEW 1 bit relative bundle of vectors
                                to MLEN 1 bit relative bundle of vectors
*
*       io.majFun       :       input[MajFun_SZ-1:0], control, to select which input to be converted
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, to select SEW 1 bit vectors
*       io.vAddCarry    :       input, bundle of vectors, data, SEW 1 bit relative, to be converted
*       io.vCmpOut      :       ipnut, bundle of vectors, data, SEW 1 bit relative, to be converted
*       io.vFCmpOut     :       input, bundle of vectors, data, SEW 1 bit relative, to be converted
*       io.vSEWtoMLEN   :       output, bundle of vectors, data, MLEN 1 bit relative, converted result
*       io.vFSEWtoFMLEN :       output, bundle of vectors, data, MLEN 1 bit relative, converted result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VSEWtoMLEN(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val majFun       = Input(UInt(MajFun_SZ.W))
    val vlmul        = Input(UInt(VLMUL_SZ.W))
    val vAddCarry    = Input(new FullSEW1wVec)
    val vCmpOut      = Input(new FullSEW1wVec)
    val vFCmpOut     = Input(new FullFSEW1wVec)
    val vSEWtoMLEN   = Output(new MLENVec)
    val vFSEWtoFMLEN = Output(new FMLENVec)
  })

  val vSEW1wVec = Mux(io.majFun === IsAdd, io.vAddCarry, io.vCmpOut)

  io.vSEWtoMLEN.m1 := vSEW1wVec.e8

  for(i <- 0 until m2Depth)
    io.vSEWtoMLEN.m2(i) := MuxCase(0.U(1.W),
                            Array((io.vlmul === QuadReg) -> vSEW1wVec.e8(i),
                                  (io.vlmul === OctuReg) -> vSEW1wVec.e16(i)))
  for(i <- 0 until m4Depth)
    io.vSEWtoMLEN.m4(i) := MuxCase(0.U(1.W),
                            Array((io.vlmul === DoubReg) -> vSEW1wVec.e8(i),
                                  (io.vlmul === QuadReg) -> vSEW1wVec.e16(i),
                                  (io.vlmul === OctuReg) -> vSEW1wVec.e32(i)))
  for(i <- 0 until m8Depth)
    io.vSEWtoMLEN.m8(i) := MuxCase(0.U(1.W),
                            Array((io.vlmul === SingReg) -> vSEW1wVec.e8(i),
                                  (io.vlmul === DoubReg) -> vSEW1wVec.e16(i),
                                  (io.vlmul === QuadReg) -> vSEW1wVec.e32(i))
                           ++ (if(ELEN >= 64) Array((io.vlmul === OctuReg) -> vSEW1wVec.e64(i)) else Nil))
  for(i <- 0 until m16Depth)
    io.vSEWtoMLEN.m16(i) := MuxCase(0.U(1.W),
                             Array((io.vlmul === SingReg) -> vSEW1wVec.e16(i),
                                   (io.vlmul === DoubReg) -> vSEW1wVec.e32(i))
                            ++ (if(ELEN >= 64)  Array((io.vlmul === QuadReg) -> vSEW1wVec.e64(i))  else Nil)
                            ++ (if(ELEN >= 128) Array((io.vlmul === OctuReg) -> vSEW1wVec.e128(i)) else Nil))
  for(i <- 0 until m32Depth)
    io.vSEWtoMLEN.m32(i) := MuxCase(0.U(1.W),
                             Array((io.vlmul === SingReg) -> vSEW1wVec.e32(i))
                            ++ (if(ELEN >= 64)  Array((io.vlmul === DoubReg) -> vSEW1wVec.e64(i))  else Nil)
                            ++ (if(ELEN >= 128) Array((io.vlmul === QuadReg) -> vSEW1wVec.e128(i)) else Nil)
                            ++ (if(ELEN >= 256) Array((io.vlmul === OctuReg) -> vSEW1wVec.e256(i)) else Nil))
  for(i <- 0 until m64Depth)
    io.vSEWtoMLEN.m64(i) := MuxCase(0.U(1.W), 
                             Array((io.vlmul === SingReg) -> vSEW1wVec.e64(i))
                            ++ (if(ELEN >= 128) Array((io.vlmul === DoubReg) -> vSEW1wVec.e128(i)) else Nil)
                            ++ (if(ELEN >= 256) Array((io.vlmul === QuadReg) -> vSEW1wVec.e256(i)) else Nil)
                            ++ (if(ELEN >= 512) Array((io.vlmul === OctuReg) -> vSEW1wVec.e512(i)) else Nil))
  for(i <- 0 until m128Depth)
    io.vSEWtoMLEN.m128(i) := MuxCase(0.U(1.W), 
                              Array((io.vlmul === SingReg) -> vSEW1wVec.e128(i))
                             ++ (if(ELEN >= 256)  Array((io.vlmul === DoubReg) -> vSEW1wVec.e256(i))  else Nil)
                             ++ (if(ELEN >= 512)  Array((io.vlmul === QuadReg) -> vSEW1wVec.e512(i))  else Nil)
                             ++ (if(ELEN == 1024) Array((io.vlmul === OctuReg) -> vSEW1wVec.e1024(i)) else Nil))
  for(i <- 0 until m256Depth)
    io.vSEWtoMLEN.m256(i) := MuxCase(0.U(1.W), 
                              Array((io.vlmul === SingReg) -> vSEW1wVec.e256(i))
                             ++ (if(ELEN >= 512)  Array((io.vlmul === DoubReg) -> vSEW1wVec.e512(i))  else Nil)
                             ++ (if(ELEN == 1024) Array((io.vlmul === QuadReg) -> vSEW1wVec.e1024(i)) else Nil))
  for(i <- 0 until m512Depth)
    io.vSEWtoMLEN.m512(i) := MuxCase(0.U(1.W), 
                              Array((io.vlmul === SingReg) -> vSEW1wVec.e512(i))
                             ++ (if(ELEN == 1024) Array((io.vlmul === DoubReg) -> vSEW1wVec.e1024(i)) else Nil))
  for(i <- 0 until m1024Depth)
     io.vSEWtoMLEN.m1024(i) := vSEW1wVec.e1024(i)



  for(i <- 0 until m2Depth)
    io.vFSEWtoFMLEN.m2(i) := MuxCase(0.U(1.W), Array()
                             ++ (if(FSEW16) Array((io.vlmul === OctuReg) -> io.vFCmpOut.f16(i)) else Nil))
  for(i <- 0 until m4Depth)
    io.vFSEWtoFMLEN.m4(i) := MuxCase(0.U(1.W),
                              Array((io.vlmul === OctuReg) -> io.vFCmpOut.f32(i))
                             ++ (if(FSEW16) Array((io.vlmul === QuadReg) -> io.vFCmpOut.f16(i)) else Nil))
  for(i <- 0 until m8Depth)
    io.vFSEWtoFMLEN.m8(i) := MuxCase(0.U(1.W),
                              Array((io.vlmul === QuadReg) -> io.vFCmpOut.f32(i))
                             ++ (if(FSEW16)        Array((io.vlmul === DoubReg) -> io.vFCmpOut.f16(i)) else Nil)
                             ++ (if(FSEWMAX >= 64) Array((io.vlmul === OctuReg) -> io.vFCmpOut.f64(i)) else Nil))
  for(i <- 0 until m16Depth)
    io.vFSEWtoFMLEN.m16(i) := MuxCase(0.U(1.W),
                               Array((io.vlmul === DoubReg) -> io.vFCmpOut.f32(i))
                              ++ (if(FSEW16)         Array((io.vlmul === SingReg) -> io.vFCmpOut.f16(i))  else Nil)
                              ++ (if(FSEWMAX >= 64)  Array((io.vlmul === QuadReg) -> io.vFCmpOut.f64(i))  else Nil)
                              ++ (if(FSEWMAX == 128) Array((io.vlmul === OctuReg) -> io.vFCmpOut.f128(i)) else Nil))
  for(i <- 0 until m32Depth)
    io.vFSEWtoFMLEN.m32(i) := MuxCase(0.U(1.W),
                               Array((io.vlmul === SingReg) -> io.vFCmpOut.f32(i))
                              ++ (if(FSEWMAX >= 64)  Array((io.vlmul === DoubReg) -> io.vFCmpOut.f64(i))  else Nil)
                              ++ (if(FSEWMAX == 128) Array((io.vlmul === QuadReg) -> io.vFCmpOut.f128(i)) else Nil))
  for(i <- 0 until m64Depth)
    io.vFSEWtoFMLEN.m64(i) := MuxCase(0.U(1.W), Array()
                              ++ (if(FSEWMAX >= 64)  Array((io.vlmul === SingReg) -> io.vFCmpOut.f64(i))  else Nil)
                              ++ (if(FSEWMAX == 128) Array((io.vlmul === DoubReg) -> io.vFCmpOut.f128(i)) else Nil))
  for(i <- 0 until m128Depth)
    io.vFSEWtoFMLEN.m128(i) := MuxCase(0.U(1.W), Array()
                               ++ (if(FSEWMAX == 128) Array((io.vlmul === SingReg) -> io.vFCmpOut.f128(i)) else Nil))
}
