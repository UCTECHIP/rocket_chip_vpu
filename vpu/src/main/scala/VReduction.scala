// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VReduction.scala
*       Author          :       liangzh
*       Revision        :       2019/10/08
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       vector reduction operation module
*
*       io.vsew         :       input[VSEW_SZ-1:0], control, vsew field of vtype CSR
*       io.redFun       :       input[2:0], control, select reduction output
*       io.enable       :       input, control, enable process
*       io.firstELEN    :       input, control, showing where the first elements of vsrc1e are
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, need the first element
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, need active elements
*       io.vdvs3e       :       input, bundle of vectors, data, SEW relative, to fill other position
*       io.v0e          :       input, bundle of vectors, control, to mask vsrc2e elements
*       io.vRedOut      :       output, bundle of vectors, data, SEW relative, reduction result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import vpu.DataExtend._

class VReduction(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vsew      = Input(UInt(VSEW_SZ.W))
    val redFun    = Input(UInt(RedFun_SZ.W))
    val enable    = Input(Bool())
    val firstELEN = Input(Bool())
    val vsrc1e    = Input(new SEWVec)
    val vsrc2e    = Input(new SEWVec)
    val vdvs3e    = Input(new SEWVec)
    val v0e       = Input(new SEW1wVec)

    val vRedOut   = Valid(new SEWVec)
  })

/////////////////exchange inactive elements to value that not affecting the result///////////////
  val vs2ZeroMasked = Wire(new SEWVec)  //for reduction add, unsigned max, bitwise or,bitwise xor
  val vs2OneMasked  = Wire(new SEWVec)  //for reduction unsigned min, bitwise and
  val vs2SMaxMasked = Wire(new SEWVec)  //for reduction signed min
  val vs2SMinMasked = Wire(new SEWVec)  //for reduction signed max

  def Zero(w: Int) = 0.U(w.W)
  def UMax(w: Int) = ~(0.U(w.W))
  def SMax(w: Int) = Cat(0.U(1.W), ~(0.U((w-1).W)))
  def SMin(w: Int) = Cat(1.U(1.W), 0.U((w-1).W))

  for(i <- 0 until e8Depth)    vs2ZeroMasked.e8(i)    := Mux(io.v0e.e8(i).asBool, io.vsrc2e.e8(i), Zero(8))
  for(i <- 0 until e16Depth)   vs2ZeroMasked.e16(i)   := Mux(io.v0e.e16(i).asBool, io.vsrc2e.e16(i), Zero(16))
  for(i <- 0 until e32Depth)   vs2ZeroMasked.e32(i)   := Mux(io.v0e.e32(i).asBool, io.vsrc2e.e32(i), Zero(32))
  for(i <- 0 until e64Depth)   vs2ZeroMasked.e64(i)   := Mux(io.v0e.e64(i).asBool, io.vsrc2e.e64(i), Zero(64))
  for(i <- 0 until e128Depth)  vs2ZeroMasked.e128(i)  := Mux(io.v0e.e128(i).asBool, io.vsrc2e.e128(i), Zero(128))
  for(i <- 0 until e256Depth)  vs2ZeroMasked.e256(i)  := Mux(io.v0e.e256(i).asBool, io.vsrc2e.e256(i), Zero(256))
  for(i <- 0 until e512Depth)  vs2ZeroMasked.e512(i)  := Mux(io.v0e.e512(i).asBool, io.vsrc2e.e512(i), Zero(512))
  for(i <- 0 until e1024Depth) vs2ZeroMasked.e1024(i) := Mux(io.v0e.e1024(i).asBool, io.vsrc2e.e1024(i), Zero(1024))

  for(i <- 0 until e8Depth)    vs2OneMasked.e8(i)     := Mux(io.v0e.e8(i).asBool, io.vsrc2e.e8(i), UMax(8))
  for(i <- 0 until e16Depth)   vs2OneMasked.e16(i)    := Mux(io.v0e.e16(i).asBool, io.vsrc2e.e16(i), UMax(16))
  for(i <- 0 until e32Depth)   vs2OneMasked.e32(i)    := Mux(io.v0e.e32(i).asBool, io.vsrc2e.e32(i), UMax(32))
  for(i <- 0 until e64Depth)   vs2OneMasked.e64(i)    := Mux(io.v0e.e64(i).asBool, io.vsrc2e.e64(i), UMax(64))
  for(i <- 0 until e128Depth)  vs2OneMasked.e128(i)   := Mux(io.v0e.e128(i).asBool, io.vsrc2e.e128(i), UMax(128))
  for(i <- 0 until e256Depth)  vs2OneMasked.e256(i)   := Mux(io.v0e.e256(i).asBool, io.vsrc2e.e256(i), UMax(256))
  for(i <- 0 until e512Depth)  vs2OneMasked.e512(i)   := Mux(io.v0e.e512(i).asBool, io.vsrc2e.e512(i), UMax(512))
  for(i <- 0 until e1024Depth) vs2OneMasked.e1024(i)  := Mux(io.v0e.e1024(i).asBool, io.vsrc2e.e1024(i), UMax(1024))

  for(i <- 0 until e8Depth)    vs2SMaxMasked.e8(i)    := Mux(io.v0e.e8(i).asBool, io.vsrc2e.e8(i), SMax(8))
  for(i <- 0 until e16Depth)   vs2SMaxMasked.e16(i)   := Mux(io.v0e.e16(i).asBool, io.vsrc2e.e16(i), SMax(16))
  for(i <- 0 until e32Depth)   vs2SMaxMasked.e32(i)   := Mux(io.v0e.e32(i).asBool, io.vsrc2e.e32(i), SMax(32))
  for(i <- 0 until e64Depth)   vs2SMaxMasked.e64(i)   := Mux(io.v0e.e64(i).asBool, io.vsrc2e.e64(i), SMax(64))
  for(i <- 0 until e128Depth)  vs2SMaxMasked.e128(i)  := Mux(io.v0e.e128(i).asBool, io.vsrc2e.e128(i), SMax(128))
  for(i <- 0 until e256Depth)  vs2SMaxMasked.e256(i)  := Mux(io.v0e.e256(i).asBool, io.vsrc2e.e256(i), SMax(256))
  for(i <- 0 until e512Depth)  vs2SMaxMasked.e512(i)  := Mux(io.v0e.e512(i).asBool, io.vsrc2e.e512(i), SMax(512))
  for(i <- 0 until e1024Depth) vs2SMaxMasked.e1024(i) := Mux(io.v0e.e1024(i).asBool, io.vsrc2e.e1024(i), SMax(1024))

  for(i <- 0 until e8Depth)    vs2SMinMasked.e8(i)    := Mux(io.v0e.e8(i).asBool, io.vsrc2e.e8(i), SMin(8))
  for(i <- 0 until e16Depth)   vs2SMinMasked.e16(i)   := Mux(io.v0e.e16(i).asBool, io.vsrc2e.e16(i), SMin(16))
  for(i <- 0 until e32Depth)   vs2SMinMasked.e32(i)   := Mux(io.v0e.e32(i).asBool, io.vsrc2e.e32(i), SMin(32))
  for(i <- 0 until e64Depth)   vs2SMinMasked.e64(i)   := Mux(io.v0e.e64(i).asBool, io.vsrc2e.e64(i), SMin(64))
  for(i <- 0 until e128Depth)  vs2SMinMasked.e128(i)  := Mux(io.v0e.e128(i).asBool, io.vsrc2e.e128(i), SMin(128))
  for(i <- 0 until e256Depth)  vs2SMinMasked.e256(i)  := Mux(io.v0e.e256(i).asBool, io.vsrc2e.e256(i), SMin(256))
  for(i <- 0 until e512Depth)  vs2SMinMasked.e512(i)  := Mux(io.v0e.e512(i).asBool, io.vsrc2e.e512(i), SMin(512))
  for(i <- 0 until e1024Depth) vs2SMinMasked.e1024(i) := Mux(io.v0e.e1024(i).asBool, io.vsrc2e.e1024(i), SMin(1024))

//////////////////////////////////////reduction operation////////////////////////////////////////
  def RedSum(redReg: UInt, vs2: UInt):  UInt = vs2 + redReg
  def RedMaxu(redReg: UInt, vs2: UInt): UInt = Mux(vs2 > redReg, vs2, redReg)
  def RedMax(redReg: UInt, vs2: UInt):  UInt = Mux(vs2.asSInt > redReg.asSInt, vs2, redReg)
  def RedMinu(redReg: UInt, vs2: UInt): UInt = Mux(vs2 > redReg, redReg, vs2)
  def RedMin(redReg: UInt, vs2: UInt):  UInt = Mux(vs2.asSInt > redReg.asSInt, redReg, vs2)
  def RedAnd(redReg: UInt, vs2: UInt):  UInt = vs2 & redReg
  def RedOr(redReg: UInt, vs2: UInt):   UInt = vs2 | redReg
  def RedXor(redReg: UInt, vs2: UInt):  UInt = vs2 ^ redReg

/////////////////////////////////////////////////////////////////////////////////////////////////
  val redReg8  = RegInit(0.U(8.W))
  val redReg16 = RegInit(0.U(16.W))
  val redReg32 = RegInit(0.U(32.W))
  val redReg64 = RegInit(0.U(64.W))
  val ready :: cal0 :: cal1 :: cal2 :: cal3 :: cal4 :: cal5 :: cal6 :: cal7 :: finish :: Nil = Enum(10)
  val redState = RegInit(ready)

  val e8step  = Array(redState === cal0, redState === cal1, 
                      redState === cal2, redState === cal3, 
                      redState === cal4, redState === cal5, 
                      redState === cal6, redState === cal7)
  val e16step = Array(redState === cal0, redState === cal1, 
                      redState === cal2, redState === cal3)
  val e32step = Array(redState === cal0, redState === cal1)
  val e64step = Array(redState === cal0)

  val vs2ZeroMaskede8  = MuxCase(Zero(8),  (e8step zip vs2ZeroMasked.e8))
  val vs2ZeroMaskede16 = MuxCase(Zero(16), (e16step zip vs2ZeroMasked.e16))
  val vs2ZeroMaskede32 = MuxCase(Zero(32), (e32step zip vs2ZeroMasked.e32))
  val vs2ZeroMaskede64 = MuxCase(Zero(64), (e64step zip vs2ZeroMasked.e64))

  val vs2OneMaskede8  = MuxCase(UMax(8),  (e8step zip vs2OneMasked.e8))
  val vs2OneMaskede16 = MuxCase(UMax(16), (e16step zip vs2OneMasked.e16))
  val vs2OneMaskede32 = MuxCase(UMax(32), (e32step zip vs2OneMasked.e32))
  val vs2OneMaskede64 = MuxCase(UMax(64), (e64step zip vs2OneMasked.e64))

  val vs2SMaxMaskede8  = MuxCase(SMax(8),  (e8step zip vs2SMaxMasked.e8))
  val vs2SMaxMaskede16 = MuxCase(SMax(16), (e16step zip vs2SMaxMasked.e16))
  val vs2SMaxMaskede32 = MuxCase(SMax(32), (e32step zip vs2SMaxMasked.e32))
  val vs2SMaxMaskede64 = MuxCase(SMax(64), (e64step zip vs2SMaxMasked.e64))

  val vs2SMinMaskede8  = MuxCase(SMin(8),  (e8step zip vs2SMinMasked.e8))
  val vs2SMinMaskede16 = MuxCase(SMin(16), (e16step zip vs2SMinMasked.e16))
  val vs2SMinMaskede32 = MuxCase(SMin(32), (e32step zip vs2SMinMasked.e32))
  val vs2SMinMaskede64 = MuxCase(SMin(64), (e64step zip vs2SMinMasked.e64))

/////////////////////////////////apply for different width element///////////////////////////////
  def RedOp8: UInt = {
    val result = MuxCase(RedSum(redReg8, vs2ZeroMaskede8),
                  Array((io.redFun === RedFun_Maxu) -> RedMaxu(redReg8, vs2ZeroMaskede8),
                        (io.redFun === RedFun_Max)  -> RedMax( redReg8, vs2SMinMaskede8),
                        (io.redFun === RedFun_Minu) -> RedMinu(redReg8, vs2OneMaskede8),
                        (io.redFun === RedFun_Min)  -> RedMin( redReg8, vs2SMaxMaskede8),
                        (io.redFun === RedFun_And)  -> RedAnd( redReg8, vs2OneMaskede8),
                        (io.redFun === RedFun_Or)   -> RedOr(  redReg8, vs2ZeroMaskede8),
                        (io.redFun === RedFun_Xor)  -> RedXor( redReg8, vs2ZeroMaskede8)))
    result
  }
  def RedOp16: UInt = {
    val result = MuxCase(RedSum(redReg16, vs2ZeroMaskede16),
                  Array((io.redFun === RedFun_Maxu) -> RedMaxu(redReg16, vs2ZeroMaskede16),
                        (io.redFun === RedFun_Max)  -> RedMax( redReg16, vs2SMinMaskede16),
                        (io.redFun === RedFun_Minu) -> RedMinu(redReg16, vs2OneMaskede16),
                        (io.redFun === RedFun_Min)  -> RedMin( redReg16, vs2SMaxMaskede16),
                        (io.redFun === RedFun_And)  -> RedAnd( redReg16, vs2OneMaskede16),
                        (io.redFun === RedFun_Or)   -> RedOr(  redReg16, vs2ZeroMaskede16),
                        (io.redFun === RedFun_Xor)  -> RedXor( redReg16, vs2ZeroMaskede16)))
    result
  }
  def RedOp32: UInt = {
    val result = MuxCase(RedSum(redReg32, vs2ZeroMaskede32),
                  Array((io.redFun === RedFun_Maxu) -> RedMaxu(redReg32, vs2ZeroMaskede32),
                        (io.redFun === RedFun_Max)  -> RedMax( redReg32, vs2SMinMaskede32),
                        (io.redFun === RedFun_Minu) -> RedMinu(redReg32, vs2OneMaskede32),
                        (io.redFun === RedFun_Min)  -> RedMin( redReg32, vs2SMaxMaskede32),
                        (io.redFun === RedFun_And)  -> RedAnd( redReg32, vs2OneMaskede32),
                        (io.redFun === RedFun_Or)   -> RedOr(  redReg32, vs2ZeroMaskede32),
                        (io.redFun === RedFun_Xor)  -> RedXor( redReg32, vs2ZeroMaskede32)))
    result
  }
  def RedOp64: UInt = {
    val result = MuxCase(RedSum(redReg64, vs2ZeroMaskede64),
                  Array((io.redFun === RedFun_Maxu) -> RedMaxu(redReg64, vs2ZeroMaskede64),
                        (io.redFun === RedFun_Max)  -> RedMax( redReg64, vs2SMinMaskede64),
                        (io.redFun === RedFun_Minu) -> RedMinu(redReg64, vs2OneMaskede64),
                        (io.redFun === RedFun_Min)  -> RedMin( redReg64, vs2SMaxMaskede64),
                        (io.redFun === RedFun_And)  -> RedAnd( redReg64, vs2OneMaskede64),
                        (io.redFun === RedFun_Or)   -> RedOr(  redReg64, vs2ZeroMaskede64),
                        (io.redFun === RedFun_Xor)  -> RedXor( redReg64, vs2ZeroMaskede64)))
    result
  }

  when(io.firstELEN) {
    redReg8  := io.vsrc1e.e8(0)
    redReg16 := io.vsrc1e.e16(0)
    redReg32 := io.vsrc1e.e32(0)
    redReg64 := io.vsrc1e.e64(0)
  }.elsewhen(redState =/= ready && redState =/= finish) {
    redReg8  := RedOp8
    redReg16 := RedOp16
    redReg32 := RedOp32
    redReg64 := RedOp64
  }

  switch(redState) {
    is(ready) {
      when(io.enable) {
        redState := cal0
      }
    }
    is(cal0) {
      when(io.vsew === DWordWidth) {
        redState := finish
      }.otherwise {
        redState := cal1
      }
    }
    is(cal1) {
      when(io.vsew === WordWidth) {
        redState := finish
      }.otherwise {
        redState := cal2
      }
    }
    is(cal2) {
      redState := cal3
    }
    is(cal3) {
      when(io.vsew === HWordWidth) {
        redState := finish
      }.otherwise {
        redState := cal4
      }
    }
    is(cal4) {
      redState := cal5
    }
    is(cal5) {
      redState := cal6
    }
    is(cal6) {
      redState := cal7
    }
    is(cal7) {
      redState := finish
    }
    is(finish) {
      redState := ready
    }
  }

  io.vRedOut.valid := redState === finish
  io.vRedOut.bits.e8  := VecInit(redReg8 +: io.vdvs3e.e8.drop(1))
  io.vRedOut.bits.e16 := VecInit(redReg16 +: io.vdvs3e.e16.drop(1))
  io.vRedOut.bits.e32 := VecInit(redReg32 +: io.vdvs3e.e32.drop(1))
  if(ELEN >= 64)   io.vRedOut.bits.e64   := VecInit(redReg64 +: io.vdvs3e.e64.drop(1))
  if(ELEN >= 128)  io.vRedOut.bits.e128  := io.vdvs3e.e128  //TODO
  if(ELEN >= 256)  io.vRedOut.bits.e256  := io.vdvs3e.e256  //TODO
  if(ELEN >= 512)  io.vRedOut.bits.e512  := io.vdvs3e.e512  //TODO
  if(ELEN == 1024) io.vRedOut.bits.e1024 := io.vdvs3e.e1024 //TODO
}
      



