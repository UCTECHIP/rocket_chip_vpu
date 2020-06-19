// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFullAdder.scala
*       Author          :       liangzh
*       Revision        :       2019/04/30
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       full adder module for integer add, sub, reverser sub insts
*
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, addend 2
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, addend 1
*       io.carryIn      :       input, bundle of vectors, data, SEW relative, carry in
*       io.addFun       :       input[AddFun_SZ-1:0], control, showing function: add or subtract or reverse subtract
*       io.vsew         :       input[VSEW_SZ-1:0], control, select saturate flag
*       io.vxrm         :       input[XRM_SZ-1:0], control, showing fixed-point rounding mode
*       io.sign         :       input, control, determine how to expand addend1 and addend2 1 bit
*       io.isRND        :       input, control, showing whether the results need to be rounding or not
*       io.isSAT        :       input, control, showing whether the results need to be saturate or not
*       io.vAddSum      :       output, bundle of vectors, data, SEW relative, sum out
*       io.vAddCarry    :       output, bundle of vectors, data, SEW 1 bit relative, carry out
*       io.vAddSat      :       output, control, showing wether the results overflow or not
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VFullAdder(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vsrc1e    = Input(new SEWVec)
    val vsrc2e    = Input(new SEWVec)
    val carryIn   = Input(new SEW1wVec)
    val addFun    = Input(UInt(AddFun_SZ.W))
    val vsew      = Input(UInt(VSEW_SZ.W))
    val vxrm      = Input(UInt(VPUConstants.XRM_SZ.W))
    val sign      = Input(Bool())
    val isRND     = Input(Bool())
    val isSAT     = Input(Bool())
    val vAddSum   = Output(new SEWVec)
    val vAddCarry = Output(new SEW1wVec)
    val vAddSat   = Output(UInt(VPUConstants.XSAT_SZ.W))
  })

  val addend1  = Wire(new SEWp1Vec)
  val addend2  = Wire(new SEWp1Vec)
  val rawSum   = Wire(new SEWp1Vec)
  val carryOut = Wire(new SEW1wVec)
  val cutSum   = Wire(new SEWVec)
  val rndSum   = Wire(new SEWVec)
  val satSum   = Wire(new SEWVec)
  val addend1Sign    = Wire(new SEW1wVec)
  val addend2Sign    = Wire(new SEW1wVec)
  val sumRealSign    = Wire(new SEW1wVec)
  val overflow       = Wire(new SEW1wVec)
  val signOverflow   = Wire(new SEW1wVec)
  val unsignOverflow = Wire(new SEW1wVec)

  val isSSAdd = io.isSAT && io.sign     //for saturate
  val isUSAdd = io.isSAT && !io.sign    //for saturate

  val isAddend1Plus = io.addFun =/= AddFun_Rev //for vsrc2e
  val isAddend2Plus = io.addFun === AddFun_Add || io.addFun === AddFun_Rev || io.addFun === AddFun_Adc //for vsrc1e

//prepare addend1
  for(i <- 0 until e8Depth) {
    val e9i        = Cat(Mux(io.sign, io.vsrc2e.e8(i)(7), 0.U(1.W)), io.vsrc2e.e8(i))
    addend1.e9(i) := Mux(isAddend1Plus, e9i, ~e9i)
  }
  for(i <- 0 until e16Depth) {
    val e17i        = Cat(Mux(io.sign, io.vsrc2e.e16(i)(15), 0.U(1.W)), io.vsrc2e.e16(i))
    addend1.e17(i) := Mux(isAddend1Plus, e17i, ~e17i)
  }
  for(i <- 0 until e32Depth) {
    val e33i        = Cat(Mux(io.sign, io.vsrc2e.e32(i)(31), 0.U(1.W)), io.vsrc2e.e32(i))
    addend1.e33(i) := Mux(isAddend1Plus, e33i, ~e33i)
  }
  for(i <- 0 until e64Depth) {
    val e65i        = Cat(Mux(io.sign, io.vsrc2e.e64(i)(63), 0.U(1.W)), io.vsrc2e.e64(i))
    addend1.e65(i) := Mux(isAddend1Plus, e65i, ~e65i)
  }
  for(i <- 0 until e128Depth) {
    val e129i        = Cat(Mux(io.sign, io.vsrc2e.e128(i)(127), 0.U(1.W)), io.vsrc2e.e128(i))
    addend1.e129(i) := Mux(isAddend1Plus, e129i, ~e129i)
  }
  for(i <- 0 until e256Depth) {
    val e257i        = Cat(Mux(io.sign, io.vsrc2e.e256(i)(255), 0.U(1.W)), io.vsrc2e.e256(i))
    addend1.e257(i) := Mux(isAddend1Plus, e257i, ~e257i)
  }
  for(i <- 0 until e512Depth) {
    val e513i        = Cat(Mux(io.sign, io.vsrc2e.e512(i)(511), 0.U(1.W)), io.vsrc2e.e512(i))
    addend1.e513(i) := Mux(isAddend1Plus, e513i, ~e513i)
  }
  for(i <- 0 until e1024Depth) {
    val e1025i        = Cat(Mux(io.sign, io.vsrc2e.e1024(i)(1023), 0.U(1.W)), io.vsrc2e.e1024(i))
    addend1.e1025(i) := Mux(isAddend1Plus, e1025i, ~e1025i)
  }
//prpare addend2
  for(i <- 0 until e8Depth) {
    val e9i        = Cat(Mux(io.sign, io.vsrc1e.e8(i)(7), 0.U(1.W)), io.vsrc1e.e8(i))
    addend2.e9(i) := Mux(isAddend2Plus, e9i, ~e9i)
  }
  for(i <- 0 until e16Depth) {
    val e17i        = Cat(Mux(io.sign, io.vsrc1e.e16(i)(15), 0.U(1.W)), io.vsrc1e.e16(i))
    addend2.e17(i) := Mux(isAddend2Plus, e17i, ~e17i)
  }
  for(i <- 0 until e32Depth) {
    val e33i        = Cat(Mux(io.sign, io.vsrc1e.e32(i)(31), 0.U(1.W)), io.vsrc1e.e32(i))
    addend2.e33(i) := Mux(isAddend2Plus, e33i, ~e33i)
  }
  for(i <- 0 until e64Depth) {
    val e65i        = Cat(Mux(io.sign, io.vsrc1e.e64(i)(63), 0.U(1.W)), io.vsrc1e.e64(i))
    addend2.e65(i) := Mux(isAddend2Plus, e65i, ~e65i)
  }
  for(i <- 0 until e128Depth) {
    val e129i        = Cat(Mux(io.sign, io.vsrc1e.e128(i)(127), 0.U(1.W)), io.vsrc1e.e128(i))
    addend2.e129(i) := Mux(isAddend2Plus, e129i, ~e129i)
  }
  for(i <- 0 until e256Depth) {
    val e257i        = Cat(Mux(io.sign, io.vsrc1e.e256(i)(255), 0.U(1.W)), io.vsrc1e.e256(i))
    addend2.e257(i) := Mux(isAddend2Plus, e257i, ~e257i)
  }
  for(i <- 0 until e512Depth) {
    val e513i        = Cat(Mux(io.sign, io.vsrc1e.e512(i)(511), 0.U(1.W)), io.vsrc1e.e512(i))
    addend2.e513(i) := Mux(isAddend2Plus, e513i, ~e513i)
  }
  for(i <- 0 until e1024Depth) {
    val e1025i        = Cat(Mux(io.sign, io.vsrc1e.e1024(i)(1023), 0.U(1.W)), io.vsrc1e.e1024(i))
    addend2.e1025(i) := Mux(isAddend2Plus, e1025i, ~e1025i)
  }


//sum addend1, addend2 and carryIn, MSBs of results are carryOut, other bits are sum
  rawSum.e9          := (addend1.e9,    addend2.e9,    io.carryIn.e8).zipped.map(_ + _ + _)
  rawSum.e17         := (addend1.e17,   addend2.e17,   io.carryIn.e16).zipped.map(_ + _ + _)
  rawSum.e33         := (addend1.e33,   addend2.e33,   io.carryIn.e32).zipped.map(_ + _ + _)
  rawSum.e65         := (addend1.e65,   addend2.e65,   io.carryIn.e64).zipped.map(_ + _ + _)
  rawSum.e129        := (addend1.e129,  addend2.e129,  io.carryIn.e128).zipped.map(_ + _ + _)
  rawSum.e257        := (addend1.e257,  addend2.e257,  io.carryIn.e256).zipped.map(_ + _ + _)
  rawSum.e513        := (addend1.e513,  addend2.e513,  io.carryIn.e512).zipped.map(_ + _ + _)
  rawSum.e1025       := (addend1.e1025, addend2.e1025, io.carryIn.e1024).zipped.map(_ + _ + _)
//extract carry out
  carryOut.e8        := rawSum.e9.map(_(8))
  carryOut.e16       := rawSum.e17.map(_(16))
  carryOut.e32       := rawSum.e33.map(_(32))
  carryOut.e64       := rawSum.e65.map(_(64))
  carryOut.e128      := rawSum.e129.map(_(128))
  carryOut.e256      := rawSum.e257.map(_(256))
  carryOut.e512      := rawSum.e513.map(_(512))
  carryOut.e1024     := rawSum.e1025.map(_(1024))
//extract addend1 sign
  addend1Sign.e8     := addend1.e9.map(_(7))
  addend1Sign.e16    := addend1.e17.map(_(15))
  addend1Sign.e32    := addend1.e33.map(_(31))
  addend1Sign.e64    := addend1.e65.map(_(63))
  addend1Sign.e128   := addend1.e129.map(_(127))
  addend1Sign.e256   := addend1.e257.map(_(255))
  addend1Sign.e512   := addend1.e513.map(_(511))
  addend1Sign.e1024  := addend1.e1025.map(_(1023))
//extract addend2 sign
  addend2Sign.e8     := addend2.e9.map(_(7))
  addend2Sign.e16    := addend2.e17.map(_(15))
  addend2Sign.e32    := addend2.e33.map(_(31))
  addend2Sign.e64    := addend2.e65.map(_(63))
  addend2Sign.e128   := addend2.e129.map(_(127))
  addend2Sign.e256   := addend2.e257.map(_(255))
  addend2Sign.e512   := addend2.e513.map(_(511))
  addend2Sign.e1024  := addend2.e1025.map(_(1023))
//extract sum sign
  sumRealSign.e8     := rawSum.e9.map(_(7))
  sumRealSign.e16    := rawSum.e17.map(_(15))
  sumRealSign.e32    := rawSum.e33.map(_(31))
  sumRealSign.e64    := rawSum.e65.map(_(63))
  sumRealSign.e128   := rawSum.e129.map(_(127))
  sumRealSign.e256   := rawSum.e257.map(_(255))
  sumRealSign.e512   := rawSum.e513.map(_(511))
  sumRealSign.e1024  := rawSum.e1025.map(_(1023))

//see if signed add/sub overflow
  signOverflow.e8    := (addend1Sign.e8,    addend2Sign.e8,    sumRealSign.e8).zipped.map((s1, s2, ss)    => s1 === s2 && s1 =/= ss)
  signOverflow.e16   := (addend1Sign.e16,   addend2Sign.e16,   sumRealSign.e16).zipped.map((s1, s2, ss)   => s1 === s2 && s1 =/= ss)
  signOverflow.e32   := (addend1Sign.e32,   addend2Sign.e32,   sumRealSign.e32).zipped.map((s1, s2, ss)   => s1 === s2 && s1 =/= ss)
  signOverflow.e64   := (addend1Sign.e64,   addend2Sign.e64,   sumRealSign.e64).zipped.map((s1, s2, ss)   => s1 === s2 && s1 =/= ss)
  signOverflow.e128  := (addend1Sign.e128,  addend2Sign.e128,  sumRealSign.e128).zipped.map((s1, s2, ss)  => s1 === s2 && s1 =/= ss)
  signOverflow.e256  := (addend1Sign.e256,  addend2Sign.e256,  sumRealSign.e256).zipped.map((s1, s2, ss)  => s1 === s2 && s1 =/= ss)
  signOverflow.e512  := (addend1Sign.e512,  addend2Sign.e512,  sumRealSign.e512).zipped.map((s1, s2, ss)  => s1 === s2 && s1 =/= ss)
  signOverflow.e1024 := (addend1Sign.e1024, addend2Sign.e1024, sumRealSign.e1024).zipped.map((s1, s2, ss) => s1 === s2 && s1 =/= ss)
//see if unsigned add/sub overflow
  unsignOverflow := carryOut
//merge signed overflow and unsigned overflow
  overflow       := Mux(io.sign, signOverflow, unsignOverflow)

//output overflow flag
  if(!SATADD) io.vAddSat := 0.U(1.W)
  else 
    io.vAddSat := MuxCase(0.U(1.W),
                   Array((io.isSAT && io.vsew === ByteWidth)  -> (overflow.e8.asUInt =/= 0.U),
                         (io.isSAT && io.vsew === HWordWidth) -> (overflow.e16.asUInt =/= 0.U),
                         (io.isSAT && io.vsew === WordWidth)  -> (overflow.e32.asUInt =/= 0.U))
                  ++ (if(ELEN >= 64) Array((io.isSAT && io.vsew === DWordWidth) -> (overflow.e64.asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 128) Array((io.isSAT && io.vsew === QWordWidth) -> (overflow.e128.asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 256) Array((io.isSAT && io.vsew === OWordWidth) -> (overflow.e256.asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 512) Array((io.isSAT && io.vsew === SWordWidth) -> (overflow.e512.asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 1024) Array((io.isSAT && io.vsew === TWordWidth) -> (overflow.e1024.asUInt =/= 0.U)) else Nil))

//saturate sum
  for(i <- 0 until e8Depth)
    satSum.e8(i) := Mux(isSSAdd && addend1Sign.e8(i).asBool  && signOverflow.e8(i).asBool,   Cat(1.U(1.W), 0.U(7.W)), 
                      Mux(isSSAdd && !addend1Sign.e8(i).asBool && signOverflow.e8(i).asBool,   Cat(0.U(1.W), ~(0.U(7.W))), 
                        Mux(isUSAdd && io.addFun === AddFun_Add  && unsignOverflow.e8(i).asBool, ~(0.U(8.W)),
                          Mux(isUSAdd && io.addFun === AddFun_Sub  && unsignOverflow.e8(i).asBool, 0.U(8.W), rawSum.e9(i)(7,0)))))
  for(i <- 0 until e16Depth)
    satSum.e16(i) := Mux(isSSAdd && addend1Sign.e16(i).asBool  && signOverflow.e16(i).asBool,   Cat(1.U(1.W), 0.U(15.W)), 
                       Mux(isSSAdd && !addend1Sign.e16(i).asBool && signOverflow.e16(i).asBool,   Cat(0.U(1.W), ~(0.U(15.W))), 
                         Mux(isUSAdd && io.addFun === AddFun_Add   && unsignOverflow.e16(i).asBool, ~(0.U(16.W)),
                           Mux(isUSAdd && io.addFun === AddFun_Sub   && unsignOverflow.e16(i).asBool, 0.U(16.W), rawSum.e17(i)(15,0)))))
  for(i <- 0 until e32Depth)
    satSum.e32(i) := Mux(isSSAdd && addend1Sign.e32(i).asBool  && signOverflow.e32(i).asBool,   Cat(1.U(1.W), 0.U(31.W)), 
                       Mux(isSSAdd && !addend1Sign.e32(i).asBool && signOverflow.e32(i).asBool,   Cat(0.U(1.W), ~(0.U(31.W))), 
                         Mux(isUSAdd && io.addFun === AddFun_Add   && unsignOverflow.e32(i).asBool, ~(0.U(32.W)),
                           Mux(isUSAdd && io.addFun === AddFun_Sub   && unsignOverflow.e32(i).asBool, 0.U(32.W), rawSum.e33(i)(31,0)))))
  for(i <- 0 until e64Depth)
    satSum.e64(i) := Mux(isSSAdd && addend1Sign.e64(i).asBool  && signOverflow.e64(i).asBool,   Cat(1.U(1.W), 0.U(63.W)), 
                       Mux(isSSAdd && !addend1Sign.e64(i).asBool && signOverflow.e64(i).asBool,   Cat(0.U(1.W), ~(0.U(63.W))), 
                         Mux(isUSAdd && io.addFun === AddFun_Add   && unsignOverflow.e64(i).asBool, ~(0.U(64.W)),
                           Mux(isUSAdd && io.addFun === AddFun_Sub   && unsignOverflow.e64(i).asBool, 0.U(64.W), rawSum.e65(i)(63,0)))))
  for(i <- 0 until e128Depth)
    satSum.e128(i) := Mux(isSSAdd && addend1Sign.e128(i).asBool  && signOverflow.e128(i).asBool,   Cat(1.U(1.W), 0.U(127.W)), 
                        Mux(isSSAdd && !addend1Sign.e128(i).asBool && signOverflow.e128(i).asBool,   Cat(0.U(1.W), ~(0.U(127.W))), 
                          Mux(isUSAdd && io.addFun === AddFun_Add    && unsignOverflow.e128(i).asBool, ~(0.U(128.W)),
                            Mux(isUSAdd && io.addFun === AddFun_Sub    && unsignOverflow.e128(i).asBool, 0.U(128.W), rawSum.e129(i)(127,0)))))
  for(i <- 0 until e256Depth)
    satSum.e256(i) := Mux(isSSAdd && addend1Sign.e256(i).asBool  && signOverflow.e256(i).asBool,   Cat(1.U(1.W), 0.U(255.W)), 
                        Mux(isSSAdd && !addend1Sign.e256(i).asBool && signOverflow.e256(i).asBool,   Cat(0.U(1.W), ~(0.U(255.W))), 
                          Mux(isUSAdd && io.addFun === AddFun_Add    && unsignOverflow.e256(i).asBool, ~(0.U(256.W)),
                            Mux(isUSAdd && io.addFun === AddFun_Sub    && unsignOverflow.e256(i).asBool, 0.U(256.W), rawSum.e257(i)(255,0)))))
  for(i <- 0 until e512Depth)
    satSum.e512(i) := Mux(isSSAdd && addend1Sign.e512(i).asBool  && signOverflow.e512(i).asBool,   Cat(1.U(1.W), 0.U(511.W)), 
                        Mux(isSSAdd && !addend1Sign.e512(i).asBool && signOverflow.e512(i).asBool,   Cat(0.U(1.W), ~(0.U(511.W))), 
                          Mux(isUSAdd && io.addFun === AddFun_Add    && unsignOverflow.e512(i).asBool, ~(0.U(512.W)),
                            Mux(isUSAdd && io.addFun === AddFun_Sub    && unsignOverflow.e512(i).asBool, 0.U(512.W), rawSum.e513(i)(511,0)))))
  for(i <- 0 until e1024Depth)
    satSum.e1024(i) := Mux(isSSAdd && addend1Sign.e1024(i).asBool  && signOverflow.e1024(i).asBool,   Cat(1.U(1.W), 0.U(1023.W)), 
                         Mux(isSSAdd && !addend1Sign.e1024(i).asBool && signOverflow.e1024(i).asBool,   Cat(0.U(1.W), ~(0.U(1023.W))), 
                           Mux(isUSAdd && io.addFun === AddFun_Add     && unsignOverflow.e1024(i).asBool, ~(0.U(1024.W)),
                             Mux(isUSAdd && io.addFun === AddFun_Sub     && unsignOverflow.e1024(i).asBool, 0.U(1024.W), rawSum.e1025(i)(1023,0)))))

//round sum
  def AverRounding(vxrm: UInt, v: UInt): UInt = {
    val rndData = MuxCase((v >> 1) + v(0),
                    Array((vxrm === 1.U) -> ((v >> 1) + (v(0) & v(1))),
                          (vxrm === 2.U) -> (v >> 1),
                          (vxrm === 3.U) -> ((v >> 1) + (v(0) & ~v(1)))))
    rndData
  }

  for(i <- 0 until e8Depth)    rndSum.e8(i)    := AverRounding(io.vxrm, rawSum.e9(i))
  for(i <- 0 until e16Depth)   rndSum.e16(i)   := AverRounding(io.vxrm, rawSum.e17(i))
  for(i <- 0 until e32Depth)   rndSum.e32(i)   := AverRounding(io.vxrm, rawSum.e33(i))
  for(i <- 0 until e64Depth)   rndSum.e64(i)   := AverRounding(io.vxrm, rawSum.e65(i))
  for(i <- 0 until e128Depth)  rndSum.e128(i)  := AverRounding(io.vxrm, rawSum.e129(i))
  for(i <- 0 until e256Depth)  rndSum.e256(i)  := AverRounding(io.vxrm, rawSum.e257(i))
  for(i <- 0 until e512Depth)  rndSum.e512(i)  := AverRounding(io.vxrm, rawSum.e513(i))
  for(i <- 0 until e1024Depth) rndSum.e1024(i) := AverRounding(io.vxrm, rawSum.e1025(i))


//output sum
  (cutSum.getElements, rawSum.getElements).zipped.map(_ := _)

  if(SATADD && AVERADD) io.vAddSum := Mux(io.isRND, rndSum, satSum)
  else if(AVERADD)      io.vAddSum := Mux(io.isRND, rndSum, cutSum)
  else if(SATADD)       io.vAddSum := satSum
  else                  io.vAddSum := cutSum

  io.vAddCarry := carryOut

}
