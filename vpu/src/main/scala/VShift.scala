// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VShift.scala
*       Author          :       yexc
*       Revision        :       2019/04/23
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       elements shift module
*
*       io.vsrc1t       :       input, bundle of vectors, data, log2(SEW) relative, shift amount
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, to be shifted operand
*       io.shiftFun     :       input[ShiftFun_SZ-1:0], control, showing function: left shift logical, right shift logical, right shift arithmetic
*       io.isSrc22SEW   :       input, control, showing whether source 2 is double SEW width or not
*       io.vxrm         :       input[XRM_SZ-1:0], control, fixed-point rounding mode
*       io.isRND        :       input, control, showing whether the results need to be rounding or not
*       io.isSAT        :       input, control, showing whether the results need to be saturate or not
*       io.sign         :       input, control, showing whether operands are sign value or not
*       io.vsew         :       input[VSEW_SZ-1:0], control, to select saturate flag by width
*       io.vShiftOut    :       output, bundle of vectors, data, SEW relative, results
*       io.vShiftSat    :       output, data, showing wether the results overflow or not
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VShift(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    // Inputs
    val vsrc1t     = Input(new Log2SEWVec)
    val vsrc2e     = Input(new SEWVec)
    val shiftFun   = Input(UInt(ShiftFun_SZ.W))
    val isSrc22SEW = Input(Bool())
    val vxrm       = Input(UInt(VPUConstants.XRM_SZ.W))
    val isRND      = Input(Bool())
    val isSAT      = Input(Bool())
    val sign       = Input(Bool())
    val vsew       = Input(UInt(VSEW_SZ.W))
    // Outputs
    val vShiftOut  = Output(new SEWVec)
    val vShiftSat  = Output(UInt(VPUConstants.XSAT_SZ.W))
  })

  val isSLL = io.shiftFun === ShiftFun_LL
  val isSRL = io.shiftFun === ShiftFun_RL
  val isSRA = io.shiftFun === ShiftFun_RA

  def shift_right_logical(src: UInt, shift: UInt): UInt = src >> shift
  def shift_left_logical(src: UInt, shift: UInt): UInt = src << shift
  def shift_right_arth(src: SInt, shift: UInt): SInt = src >> shift

  val In       = Wire(new SEWVec)
  val Out      = Wire(new SEWVec)
  val Round    = Wire(new SEW1wVec)
  val uNSat    = Wire(new SEW1wVec)
  val sNSat    = Wire(new SEW1wVec)
  val noSat    = Mux(io.sign, sNSat, uNSat)  

  val SatuOut8 = Wire(Vec(e16Depth, UInt(8.W)))    
  val SatuOut16 = Wire(Vec(e32Depth, UInt(16.W))) 
  val SatuOut32 = Wire(Vec(e64Depth, UInt(32.W))) 
  val SatuOut64 = Wire(Vec(e128Depth, UInt(64.W))) 
  val SatuOut128 = Wire(Vec(e256Depth, UInt(128.W)))  
  val SatuOut256 = Wire(Vec(e512Depth, UInt(256.W)))  
  val SatuOut512 = Wire(Vec(e1024Depth, UInt(512.W)))  

  val RoundOut8 = Wire(Vec(e16Depth, UInt(8.W)))    
  val RoundOut16 = Wire(Vec(e32Depth, UInt(16.W))) 
  val RoundOut32 = Wire(Vec(e64Depth, UInt(32.W))) 
  val RoundOut64 = Wire(Vec(e128Depth, UInt(64.W))) 
  val RoundOut128 = Wire(Vec(e256Depth, UInt(128.W)))  
  val RoundOut256 = Wire(Vec(e512Depth, UInt(256.W)))  
  val RoundOut512 = Wire(Vec(e1024Depth, UInt(512.W)))  

  uNSat.e8 := VecInit(Seq.fill(e8Depth){ 0.U(1.W) })
  sNSat.e8 := VecInit(Seq.fill(e8Depth){ 0.U(1.W) })

  if(!NCLIP) io.vShiftSat := 0.U(1.W)
  else 
    io.vShiftSat := MuxCase(0.U(1.W),
                     Array((io.isSAT && io.vsew === ByteWidth)  -> (noSat.e16.asUInt =/= ~0.U(e16Depth.W)),
                           (io.isSAT && io.vsew === HWordWidth) -> (noSat.e32.asUInt =/= ~0.U(e32Depth.W)))
                    ++ (if(ELEN >= 64) Array((io.isSAT && io.vsew === WordWidth)  -> (noSat.e64.asUInt =/= 0.U(e64Depth.W))) else Nil)
                    ++ (if(ELEN >= 128) Array((io.isSAT && io.vsew === DWordWidth) -> (noSat.e128.asUInt =/= 0.U(e128Depth.W))) else Nil)
                    ++ (if(ELEN >= 256) Array((io.isSAT && io.vsew === QWordWidth) -> (noSat.e256.asUInt =/= 0.U(e256Depth.W))) else Nil)
                    ++ (if(ELEN >= 512) Array((io.isSAT && io.vsew === OWordWidth) -> (noSat.e512.asUInt =/= 0.U(e512Depth.W))) else Nil)
                    ++ (if(ELEN >= 1024) Array((io.isSAT && io.vsew === SWordWidth) -> (noSat.e1024.asUInt =/= 0.U(e1024Depth.W))) else Nil))

  //*****************************************
  //      1024-bit shift
  //*****************************************
  for(i <- 0 until e1024Depth)
  {
    val src = io.vsrc2e.e1024(i)
    val len = io.vsrc1t.to10(i)

    In.e1024(i) := MuxCase(0.U, 
                     Array(isSLL -> shift_left_logical(io.vsrc2e.e1024(i), io.vsrc1t.to10(i)),
                           isSRL -> shift_right_logical(io.vsrc2e.e1024(i), io.vsrc1t.to10(i)),
                           isSRA -> shift_right_arth(io.vsrc2e.e1024(i).asSInt, io.vsrc1t.to10(i)).asUInt
                          ))

    val lsb         = (src << (1023.U - len))(1023)                            // v[d]
    val lsbHalf     = (src << (1024.U - len))(1023)                            // v[d-1]
    val lsbRest     = Mux((src << (1024.U - len))(1023, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (1025.U - len))(1023, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e1024(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                         (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                         (io.vxrm === RDN) -> 0.U,
                                         (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                        ))

    if(SCALESR || NCLIP)
      Out.e1024(i) := Mux(io.isRND, In.e1024(i) + Round.e1024(i), In.e1024(i))
    else
      Out.e1024(i) := In.e1024(i)

    sNSat.e1024(i)    := Out.e1024(i)(1023,511) === 0.U(513.W) || Out.e1024(i)(1023,511) === ~(0.U(513.W))
    uNSat.e1024(i)    := Out.e1024(i)(1023,512) === 0.U
    val SatuOutSign    = Mux(sNSat.e1024(i).asBool, Out.e1024(i)(511,0), 
                            Cat(Out.e1024(i)(1023), Cat(Seq.fill(511){~Out.e1024(i)(1023)})))
    val SatuOutUnSign  = Mux(uNSat.e1024(i).asBool, Out.e1024(i)(511,0), ~(0.U(512.W)))

    SatuOut512(i)     := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut512(i)  := Mux(io.isSAT, SatuOut512(i), Out.e1024(i)(511,0))
    else
      RoundOut512(i)  := In.e1024(i)(511,0)

    io.vShiftOut.e1024(i) := Mux(io.isSrc22SEW, 0.U, Out.e1024(i))
  }


  //*****************************************
  //      512-bit shift
  //*****************************************
  for(i <- 0 until e512Depth)
  {
    val src = io.vsrc2e.e512(i)
    val len = io.vsrc1t.to9(i)

    In.e512(i) := MuxCase(0.U, 
                    Array(isSLL -> shift_left_logical(io.vsrc2e.e512(i), io.vsrc1t.to9(i)),
                          isSRL -> shift_right_logical(io.vsrc2e.e512(i), io.vsrc1t.to9(i)),
                          isSRA -> shift_right_arth(io.vsrc2e.e512(i).asSInt, io.vsrc1t.to9(i)).asUInt
                         ))

    val lsb         = (src << (511.U - len))(511)                            // v[d]
    val lsbHalf     = (src << (512.U - len))(511)                            // v[d-1]
    val lsbRest     = Mux((src << (512.U - len))(511, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (513.U - len))(511, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e512(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                        (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                        (io.vxrm === RDN) -> 0.U,
                                        (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                       ))

    if(SCALESR || NCLIP)
      Out.e512(i) := Mux(io.isRND, In.e512(i) + Round.e512(i), In.e512(i))
    else
      Out.e512(i) := In.e512(i)

    sNSat.e512(i)    := Out.e512(i)(511,255) === 0.U(257.W) || Out.e512(i)(511,255) === ~(0.U(257.W))
    uNSat.e512(i)    := Out.e512(i)(511,256) === 0.U
    val SatuOutSign   = Mux(sNSat.e512(i).asBool, Out.e512(i)(255,0), 
                            Cat(Out.e512(i)(511), Cat(Seq.fill(255){~Out.e512(i)(511)})))
    val SatuOutUnSign = Mux(uNSat.e512(i).asBool, Out.e512(i)(255,0), ~(0.U(256.W)))

    SatuOut256(i)    := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut256(i) := Mux(io.isSAT, SatuOut256(i), Out.e512(i)(255,0))
    else
      RoundOut256(i) := In.e512(i)(255,0)
  }

  for(i <- 0 until e512Depth/2)
    if(ELEN > 512)                      io.vShiftOut.e512(i) := Mux(io.isSrc22SEW, RoundOut512(i), Out.e512(i))
    else                                io.vShiftOut.e512(i) := Mux(io.isSrc22SEW, 0.U,            Out.e512(i))
  for(i <- e512Depth/2 until e512Depth) io.vShiftOut.e512(i) := Mux(io.isSrc22SEW, 0.U,            Out.e512(i))


  //*****************************************
  //      256-bit shift
  //*****************************************
  for(i <- 0 until e256Depth)
  {
    val src = io.vsrc2e.e256(i)
    val len = io.vsrc1t.to8(i)

    In.e256(i) := MuxCase(0.U, 
                    Array(isSLL -> shift_left_logical(io.vsrc2e.e256(i), io.vsrc1t.to8(i)),
                          isSRL -> shift_right_logical(io.vsrc2e.e256(i), io.vsrc1t.to8(i)),
                          isSRA -> shift_right_arth(io.vsrc2e.e256(i).asSInt, io.vsrc1t.to8(i)).asUInt
                         ))

    val lsb         = (src << (255.U - len))(255)                            // v[d]
    val lsbHalf     = (src << (256.U - len))(255)                            // v[d-1]
    val lsbRest     = Mux((src << (256.U - len))(255, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (257.U - len))(255, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e256(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                        (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                        (io.vxrm === RDN) -> 0.U,
                                        (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                       ))

    if(SCALESR || NCLIP)
      Out.e256(i) := Mux(io.isRND, In.e256(i) + Round.e256(i), In.e256(i))
    else
      Out.e256(i) := In.e256(i)

    sNSat.e256(i)    := Out.e256(i)(255,127) === 0.U(129.W) || Out.e256(i)(255,127) === ~(0.U(129.W))
    uNSat.e256(i)    := Out.e256(i)(255,128) === 0.U
    val SatuOutSign   = Mux(sNSat.e256(i).asBool, Out.e256(i)(127,0), 
                            Cat(Out.e256(i)(255), Cat(Seq.fill(127){~Out.e256(i)(255)})))
    val SatuOutUnSign = Mux(uNSat.e256(i).asBool, Out.e256(i)(127,0), ~(0.U(128.W)))

    SatuOut128(i)    := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut128(i) := Mux(io.isSAT, SatuOut128(i), Out.e256(i)(127,0))
    else
      RoundOut128(i) := In.e256(i)(127,0)
  }

  for(i <- 0 until e256Depth/2)
    if(ELEN > 256)                      io.vShiftOut.e256(i) := Mux(io.isSrc22SEW, RoundOut256(i), Out.e256(i))
    else                                io.vShiftOut.e256(i) := Mux(io.isSrc22SEW, 0.U,            Out.e256(i))
  for(i <- e256Depth/2 until e256Depth) io.vShiftOut.e256(i) := Mux(io.isSrc22SEW, 0.U,            Out.e256(i))


  //*****************************************
  //      128-bit shift
  //*****************************************
  for(i <- 0 until e128Depth)
  {
    val src = io.vsrc2e.e128(i)
    val len = io.vsrc1t.to7(i)

    In.e128(i) := MuxCase(0.U, 
                    Array(isSLL -> shift_left_logical(io.vsrc2e.e128(i), io.vsrc1t.to7(i)),
                          isSRL -> shift_right_logical(io.vsrc2e.e128(i), io.vsrc1t.to7(i)),
                          isSRA -> shift_right_arth(io.vsrc2e.e128(i).asSInt, io.vsrc1t.to7(i)).asUInt
                         ))

    val lsb         = (src << (127.U - len))(127)                            // v[d]
    val lsbHalf     = (src << (128.U - len))(127)                            // v[d-1]
    val lsbRest     = Mux((src << (128.U - len))(127, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (129.U - len))(127, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e128(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                        (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                        (io.vxrm === RDN) -> 0.U,
                                        (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                       ))

    if(SCALESR || NCLIP)
      Out.e128(i) := Mux(io.isRND, In.e128(i) + Round.e128(i), In.e128(i))
    else
      Out.e128(i) := In.e128(i)

    sNSat.e128(i)    := Out.e128(i)(127,63) === 0.U(65.W) || Out.e128(i)(127,63) === ~(0.U(65.W))
    uNSat.e128(i)    := Out.e128(i)(127,64) === 0.U
    val SatuOutSign   = Mux(sNSat.e128(i).asBool, Out.e128(i)(63,0), 
                            Cat(Out.e128(i)(127), Cat(Seq.fill(63){~Out.e128(i)(127)})))
    val SatuOutUnSign = Mux(uNSat.e128(i).asBool, Out.e128(i)(63,0), ~(0.U(64.W)))

    SatuOut64(i)     := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut64(i)  := Mux(io.isSAT, SatuOut64(i), Out.e128(i)(63,0))
    else
      RoundOut64(i)  := In.e128(i)(63,0)
  }

  for(i <- 0 until e128Depth/2) 
    if(ELEN > 128)                      io.vShiftOut.e128(i) := Mux(io.isSrc22SEW, RoundOut128(i), Out.e128(i))
    else                                io.vShiftOut.e128(i) := Mux(io.isSrc22SEW, 0.U,            Out.e128(i))
  for(i <- e128Depth/2 until e128Depth) io.vShiftOut.e128(i) := Mux(io.isSrc22SEW, 0.U,            Out.e128(i))


  //*****************************************
  //      64-bit shift
  //*****************************************
  for(i <- 0 until e64Depth)
  {
    val src = io.vsrc2e.e64(i)
    val len = io.vsrc1t.to6(i)

    In.e64(i) := MuxCase(0.U, 
                   Array(isSLL -> shift_left_logical(io.vsrc2e.e64(i), io.vsrc1t.to6(i)),
                         isSRL -> shift_right_logical(io.vsrc2e.e64(i), io.vsrc1t.to6(i)),
                         isSRA -> shift_right_arth(io.vsrc2e.e64(i).asSInt, io.vsrc1t.to6(i)).asUInt
                        ))

    val lsb         = (src << (63.U - len))(63)                            // v[d]
    val lsbHalf     = (src << (64.U - len))(63)                            // v[d-1]
    val lsbRest     = Mux((src << (64.U - len))(63, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (65.U - len))(63, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e64(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                       (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                       (io.vxrm === RDN) -> 0.U,
                                       (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                      ))

    if(SCALESR || NCLIP)
      Out.e64(i) := Mux(io.isRND, In.e64(i) + Round.e64(i), In.e64(i))
    else
      Out.e64(i) := In.e64(i)

    sNSat.e64(i)     := Out.e64(i)(63,31) === 0.U(33.W) || Out.e64(i)(63,31) === ~(0.U(33.W))
    uNSat.e64(i)     := Out.e64(i)(63,32) === 0.U
    val SatuOutSign   = Mux(sNSat.e64(i).asBool, Out.e64(i)(31,0), 
                            Cat(Out.e64(i)(63), Cat(Seq.fill(31){~Out.e64(i)(63)})))
    val SatuOutUnSign = Mux(uNSat.e64(i).asBool, Out.e64(i)(31,0), ~(0.U(32.W)))

    SatuOut32(i)     := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut32(i)  := Mux(io.isSAT, SatuOut32(i), Out.e64(i)(31,0))
    else
      RoundOut32(i)  := In.e64(i)(31,0)
  }

  for(i <- 0 until e64Depth/2)
    if(ELEN > 64)                     io.vShiftOut.e64(i) := Mux(io.isSrc22SEW, RoundOut64(i), Out.e64(i))
    else                              io.vShiftOut.e64(i) := Mux(io.isSrc22SEW, 0.U,           Out.e64(i))
  for(i <- e64Depth/2 until e64Depth) io.vShiftOut.e64(i) := Mux(io.isSrc22SEW, 0.U,           Out.e64(i))


  //*****************************************
  //      32-bit shift
  //*****************************************
  for(i <- 0 until e32Depth)
  {
    val src = io.vsrc2e.e32(i)
    val len = io.vsrc1t.to5(i)

    In.e32(i) := MuxCase(0.U, 
                   Array(isSLL -> shift_left_logical(io.vsrc2e.e32(i), io.vsrc1t.to5(i)),
                         isSRL -> shift_right_logical(io.vsrc2e.e32(i), io.vsrc1t.to5(i)),
                         isSRA -> shift_right_arth(io.vsrc2e.e32(i).asSInt, io.vsrc1t.to5(i)).asUInt
                        ))

    val lsb         = (src << (31.U - len))(31)                            // v[d]
    val lsbHalf     = (src << (32.U - len))(31)                            // v[d-1]
    val lsbRest     = Mux((src << (32.U - len))(31, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (33.U - len))(31, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e32(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                       (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                       (io.vxrm === RDN) -> 0.U,
                                       (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                      ))

    if(SCALESR || NCLIP)
      Out.e32(i) := Mux(io.isRND, In.e32(i) + Round.e32(i), In.e32(i))
    else
      Out.e32(i) := In.e32(i)

    sNSat.e32(i)     := Out.e32(i)(31,15) === 0.U(17.W) || Out.e32(i)(31,15) === ~(0.U(17.W))
    uNSat.e32(i)     := Out.e32(i)(31,16) === 0.U
    val SatuOutSign   = Mux(sNSat.e32(i).asBool, Out.e32(i)(15,0), 
                            Cat(Out.e32(i)(31), Cat(Seq.fill(15){~Out.e32(i)(31)})))
    val SatuOutUnSign = Mux(uNSat.e32(i).asBool, Out.e32(i)(15,0), ~(0.U(16.W)))

    SatuOut16(i)     := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut16(i)  := Mux(io.isSAT, SatuOut16(i), Out.e32(i)(15,0))
    else
      RoundOut16(i)  := In.e32(i)(15,0)
  }

  for(i <- 0 until e32Depth/2)             
    if(ELEN > 32)                     io.vShiftOut.e32(i) := Mux(io.isSrc22SEW, RoundOut32(i), Out.e32(i)) 
    else                              io.vShiftOut.e32(i) := Mux(io.isSrc22SEW, 0.U,           Out.e32(i))
  for(i <- e32Depth/2 until e32Depth) io.vShiftOut.e32(i) := Mux(io.isSrc22SEW, 0.U,           Out.e32(i))



  //*****************************************
  //      16-bit shift
  //*****************************************
  for(i <- 0 until e16Depth)
  {
    val src = io.vsrc2e.e16(i)
    val len = io.vsrc1t.to4(i)

    In.e16(i) := MuxCase(0.U, 
                   Array(isSLL -> shift_left_logical(io.vsrc2e.e16(i), io.vsrc1t.to4(i)),
                         isSRL -> shift_right_logical(io.vsrc2e.e16(i), io.vsrc1t.to4(i)),
                         isSRA -> shift_right_arth(io.vsrc2e.e16(i).asSInt, io.vsrc1t.to4(i)).asUInt
                        ))

    val lsb         = (src << (15.U - len))(15)                            // v[d]
    val lsbHalf     = (src << (16.U - len))(15)                            // v[d-1]
    val lsbRest     = Mux((src << (16.U - len))(15, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (17.U - len))(15, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e16(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                       (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                       (io.vxrm === RDN) -> 0.U,
                                       (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                      ))

    if(SCALESR || NCLIP)
      Out.e16(i) := Mux(io.isRND, In.e16(i) + Round.e16(i), In.e16(i))
    else
      Out.e16(i) := In.e16(i)

    sNSat.e16(i)     := Out.e16(i)(15,7) === 0.U(9.W) || Out.e16(i)(15,7) === ~(0.U(9.W))
    uNSat.e16(i)     := Out.e16(i)(15,8) === 0.U
    val SatuOutSign   = Mux(sNSat.e16(i).asBool, Out.e16(i)(7,0), 
                            Cat(Out.e16(i)(15), Cat(Seq.fill(7){~Out.e16(i)(15)})))
    val SatuOutUnSign = Mux(uNSat.e16(i).asBool, Out.e16(i)(7,0), ~(0.U(8.W)))

    SatuOut8(i)      := Mux(io.sign, SatuOutSign, SatuOutUnSign)
    if(NCLIP)
      RoundOut8(i)   := Mux(io.isSAT, SatuOut8(i), Out.e16(i)(7,0))
    else
      RoundOut8(i)   := In.e16(i)(7,0)
  }

  for(i <- 0 until e16Depth/2)        io.vShiftOut.e16(i) := Mux(io.isSrc22SEW, RoundOut16(i), Out.e16(i))
  for(i <- e16Depth/2 until e16Depth) io.vShiftOut.e16(i) := Mux(io.isSrc22SEW, 0.U,           Out.e16(i))


  //*****************************************
  //      8-bit shift
  //*****************************************
  for(i <- 0 until e8Depth)
  {
    val src = io.vsrc2e.e8(i)
    val len = io.vsrc1t.to3(i)

    In.e8(i) := MuxCase(0.U, 
                  Array(isSLL -> shift_left_logical(io.vsrc2e.e8(i), io.vsrc1t.to3(i)),
                        isSRL -> shift_right_logical(io.vsrc2e.e8(i), io.vsrc1t.to3(i)),
                        isSRA -> shift_right_arth(io.vsrc2e.e8(i).asSInt, io.vsrc1t.to3(i)).asUInt
                       ))

    val lsb         = (src << (7.U - len))(7)                            // v[d]
    val lsbHalf     = (src << (8.U - len))(7)                            // v[d-1]
    val lsbRest     = Mux((src << (8.U - len))(7, 0) =/= 0.U, 1.U, 0.U)  // v[d-1:0]
    val lsbHalfRest = Mux((src << (9.U - len))(7, 0) =/= 0.U, 1.U, 0.U)  // v[d-2:0]

    Round.e8(i) := MuxCase(0.U, Array((io.vxrm === RNU) -> lsbHalf,
                                      (io.vxrm === RNE) -> (lsbHalf & (lsbHalfRest | lsb)),
                                      (io.vxrm === RDN) -> 0.U,
                                      (io.vxrm === ROD) -> (~(lsb) & lsbRest)
                                     ))

    if(SCALESR || NCLIP)
      Out.e8(i) := Mux(io.isRND, In.e8(i) + Round.e8(i), In.e8(i))
    else
      Out.e8(i) := In.e8(i)
  }

  for(i <- 0 until e8Depth/2)       io.vShiftOut.e8(i) := Mux(io.isSrc22SEW, RoundOut8(i), Out.e8(i))
  for(i <- e8Depth/2 until e8Depth) io.vShiftOut.e8(i) := Mux(io.isSrc22SEW, 0.U,          Out.e8(i))
}

