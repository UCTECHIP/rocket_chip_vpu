// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VPopc.scala
*       Author          :       yexc
*       Revision        :       2019/04/25
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       for processing the inst -- vpopc.m
*
*       io.vs2m         :       input, bundle of vectors, data, elements all 1 bit, operand
*       io.v0m          :       input, bundle of vectors, data, elements all 1 bit, to mask operand
*       io.vsew         :       input[VSEW_SZ-1:0], control, to form select signal
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, to form select signal
*       io.vm           :       input, control, to enable mask
*       io.vPopcOut     :       Output[XLEN-1:0], data, result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VPopc(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    // Inputs
    val vs2m        = Input(new MLENVec)
    val v0m         = Input(new MLENVec)
    val vsew        = Input(UInt(VSEW_SZ.W))
    val vlmul       = Input(UInt(VLMUL_SZ.W))
    val vm          = Input(UInt(1.W))
    // Outputs
    val vPopcOut   = Output(UInt(XLEN.W))
  })

  val out1Width    = log2Ceil(m1Depth) + 1
  val out2Width    = log2Ceil(m2Depth) + 1
  val out4Width    = log2Ceil(m4Depth) + 1
  val out8Width    = log2Ceil(m8Depth) + 1
  val out16Width   = log2Ceil(m16Depth) + 1
  val out32Width   = log2Ceil(m32Depth) + 1
  val out64Width   = if(ELEN >= 64)   log2Ceil(m64Depth) + 1   else 0
  val out128Width  = if(ELEN >= 128)  log2Ceil(m128Depth) + 1  else 0
  val out256Width  = if(ELEN >= 256)  log2Ceil(m256Depth) + 1  else 0
  val out512Width  = if(ELEN >= 512)  log2Ceil(m512Depth) + 1  else 0
  val out1024Width = if(ELEN >= 1024) log2Ceil(m1024Depth) + 1 else 0

  val PopcOut1        = Wire(Vec(m1Depth,    UInt(out1Width.W)))
  val PopcOut2        = Wire(Vec(m2Depth,    UInt(out2Width.W)))
  val PopcOut4        = Wire(Vec(m4Depth,    UInt(out4Width.W)))
  val PopcOut8        = Wire(Vec(m8Depth,    UInt(out8Width.W)))
  val PopcOut16       = Wire(Vec(m16Depth,   UInt(out16Width.W)))
  val PopcOut32       = Wire(Vec(m32Depth,   UInt(out32Width.W)))
  val PopcOut64       = Wire(Vec(m64Depth,   UInt(out64Width.W)))
  val PopcOut128      = Wire(Vec(m128Depth,  UInt(out128Width.W)))
  val PopcOut256      = Wire(Vec(m256Depth,  UInt(out256Width.W)))
  val PopcOut512      = Wire(Vec(m512Depth,  UInt(out512Width.W)))
  val PopcOut1024     = Wire(Vec(m1024Depth, UInt(out1024Width.W)))

  val MLEN    = Wire(UInt(11.W))

  val Out1    = Wire(UInt(out1Width.W))
  val Out2    = Wire(UInt(out2Width.W))
  val Out4    = Wire(UInt(out4Width.W))
  val Out8    = Wire(UInt(out8Width.W))
  val Out16   = Wire(UInt(out16Width.W))
  val Out32   = Wire(UInt(out32Width.W))
  val Out64   = Wire(UInt(out64Width.W))
  val Out128  = Wire(UInt(out128Width.W))
  val Out256  = Wire(UInt(out256Width.W))
  val Out512  = Wire(UInt(out512Width.W))
  val Out1024 = Wire(UInt(out1024Width.W))


  MLEN := 1.U << (io.vsew + 3.U - io.vlmul)


  for(i <- 0 until m1Depth)
    PopcOut1(i)    :=  (io.v0m.m1(i) | io.vm) & io.vs2m.m1(i)

  for(i <- 0 until m2Depth)
    PopcOut2(i)    :=  (io.v0m.m2(i) | io.vm) & io.vs2m.m2(i)

  for(i <- 0 until m4Depth)
    PopcOut4(i)    :=  (io.v0m.m4(i) | io.vm) & io.vs2m.m4(i)

  for(i <- 0 until m8Depth)
    PopcOut8(i)    :=  (io.v0m.m8(i) | io.vm) & io.vs2m.m8(i)

  for(i <- 0 until m16Depth)
    PopcOut16(i)   :=  (io.v0m.m16(i) | io.vm) & io.vs2m.m16(i)

  for(i <- 0 until m32Depth)
    PopcOut32(i)   :=  (io.v0m.m32(i) | io.vm) & io.vs2m.m32(i)

  for(i <- 0 until m64Depth)
    PopcOut64(i)   :=  (io.v0m.m64(i) | io.vm) & io.vs2m.m64(i)

  for(i <- 0 until m128Depth)
    PopcOut128(i)  :=  (io.v0m.m128(i) | io.vm) & io.vs2m.m128(i)

  for(i <- 0 until m256Depth)
    PopcOut256(i)  :=  (io.v0m.m256(i) | io.vm) & io.vs2m.m256(i)

  for(i <- 0 until m512Depth)
    PopcOut512(i)  :=  (io.v0m.m512(i) | io.vm) & io.vs2m.m512(i)

  for(i <- 0 until m1024Depth)
    PopcOut1024(i) :=  (io.v0m.m1024(i) | io.vm) & io.vs2m.m1024(i)


  Out1    :=  PopcOut1.foldRight(0.U)(_ + _)
  Out2    :=  PopcOut2.foldRight(0.U)(_ + _)
  Out4    :=  PopcOut4.foldRight(0.U)(_ + _) 
  Out8    :=  PopcOut8.foldRight(0.U)(_ + _)
  Out16   :=  PopcOut16.foldRight(0.U)(_ + _)
  Out32   :=  PopcOut32.foldRight(0.U)(_ + _)
  Out64   :=  PopcOut64.foldRight(0.U)(_ + _)
  Out128  :=  PopcOut128.foldRight(0.U)(_ + _)
  Out256  :=  PopcOut256.foldRight(0.U)(_ + _)
  Out512  :=  PopcOut512.foldRight(0.U)(_ + _)
  Out1024 :=  PopcOut1024.foldRight(0.U)(_ + _)


  io.vPopcOut := MuxCase(0.U, 
                  Array((MLEN === 1.U)    -> Out1,
                        (MLEN === 2.U)    -> Out2,
                        (MLEN === 4.U)    -> Out4,
                        (MLEN === 8.U)    -> Out8,
                        (MLEN === 16.U)   -> Out16,
                        (MLEN === 32.U)   -> Out32,
                        (MLEN === 64.U)   -> Out64,
                        (MLEN === 128.U)  -> Out128,
                        (MLEN === 256.U)  -> Out256,
                        (MLEN === 512.U)  -> Out512,
                        (MLEN === 1024.U) -> Out1024)

                         )

}


