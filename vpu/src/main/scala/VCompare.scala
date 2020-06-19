// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VCompare.scala
*       Author          :       yexc
*       Revision        :       2019/04/12
*       Company         :       UC TECH IP
*       Department      :       STG
*       Description     :       compare data
*
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, operand 2
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, operand 1
*       io.cmpFun       :       input[CmpFun_SZ-1:0], control, function select
*       io.sign         :       input, control, show whether operands are signed values or not
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, vlmul field from vtype CSR
*       io.vCmpOut      :       output, bundle of vectors, data, elements 1 bit width, compare result
*       io.vMinMaxOut   :       output, bundle of vectors, data, SEW relative, Min/Max ones between two values 
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VCompare(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    // Input
    val vsrc1e     = Input(new SEWVec)
    val vsrc2e     = Input(new SEWVec)
    val cmpFun     = Input(UInt(CmpFun_SZ.W))
    val sign       = Input(Bool())
    val vlmul      = Input(UInt(VLMUL_SZ.W))
    // Output
    val vCmpOut    = Output(new SEW1wVec)
    val vMinMaxOut = Output(new SEWVec)
  })  

  def equal(src1: UInt, src2: UInt): Bool = src1 === src2
  def less(src1: UInt, src2: UInt, sign: Bool): Bool = Mux(sign, src1.asSInt < src2.asSInt, src1 < src2)
  def cmp(src1: UInt, src2: UInt, fun: UInt, sign: Bool): Bool = {
    val equal_wire = Wire(Bool())
    val less_wire = Wire(Bool())    
    val greater_wire = Wire(Bool())

    equal_wire := equal(src1, src2)
    less_wire := less(src1, src2, sign)
    greater_wire := Mux(equal_wire, false.B, ~less_wire)

    MuxCase(false.B, Array((fun === "b000".U) -> equal_wire,
                           (fun === "b001".U) -> ~equal_wire,
                           (fun === "b010".U) -> less_wire,
                           (fun === "b011".U) -> (less_wire | equal_wire), 
                           (fun === "b100".U) -> greater_wire,
                           (fun === "b101".U) -> less_wire,
                           (fun === "b110".U) -> greater_wire
           ))    
    }


  // compare bit output
  for(i <- 0 until e8Depth)
      io.vCmpOut.e8(i) := Mux(cmp(io.vsrc2e.e8(i), io.vsrc1e.e8(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e16Depth)
      io.vCmpOut.e16(i) := Mux(cmp(io.vsrc2e.e16(i), io.vsrc1e.e16(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e32Depth)
      io.vCmpOut.e32(i) := Mux(cmp(io.vsrc2e.e32(i), io.vsrc1e.e32(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e64Depth)
      io.vCmpOut.e64(i) := Mux(cmp(io.vsrc2e.e64(i), io.vsrc1e.e64(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e128Depth)
      io.vCmpOut.e128(i) := Mux(cmp(io.vsrc2e.e128(i), io.vsrc1e.e128(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e256Depth)
      io.vCmpOut.e256(i) := Mux(cmp(io.vsrc2e.e256(i), io.vsrc1e.e256(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e512Depth)
      io.vCmpOut.e512(i) := Mux(cmp(io.vsrc2e.e512(i), io.vsrc1e.e512(i), io.cmpFun, io.sign), 1.U, 0.U)

  for(i <- 0 until e1024Depth)
      io.vCmpOut.e1024(i) := Mux(cmp(io.vsrc2e.e1024(i), io.vsrc1e.e1024(i), io.cmpFun, io.sign), 1.U, 0.U)



  // select min/max value
  for(i <- 0 until e8Depth)
      io.vMinMaxOut.e8(i) := Mux(io.vCmpOut.e8(i) === 1.U, io.vsrc2e.e8(i), io.vsrc1e.e8(i))

  for(i <- 0 until e16Depth)
      io.vMinMaxOut.e16(i) := Mux(io.vCmpOut.e16(i) === 1.U, io.vsrc2e.e16(i), io.vsrc1e.e16(i))

  for(i <- 0 until e32Depth)
      io.vMinMaxOut.e32(i) := Mux(io.vCmpOut.e32(i) === 1.U, io.vsrc2e.e32(i), io.vsrc1e.e32(i))

  for(i <- 0 until e64Depth)
      io.vMinMaxOut.e64(i) := Mux(io.vCmpOut.e64(i) === 1.U, io.vsrc2e.e64(i), io.vsrc1e.e64(i))

  for(i <- 0 until e128Depth)
      io.vMinMaxOut.e128(i) := Mux(io.vCmpOut.e128(i) === 1.U, io.vsrc2e.e128(i), io.vsrc1e.e128(i))

  for(i <- 0 until e256Depth)
      io.vMinMaxOut.e256(i) := Mux(io.vCmpOut.e256(i) === 1.U, io.vsrc2e.e256(i), io.vsrc1e.e256(i))

  for(i <- 0 until e512Depth)
      io.vMinMaxOut.e512(i) := Mux(io.vCmpOut.e512(i) === 1.U, io.vsrc2e.e512(i), io.vsrc1e.e512(i))

  for(i <- 0 until e1024Depth)
      io.vMinMaxOut.e1024(i) := Mux(io.vCmpOut.e1024(i) === 1.U, io.vsrc2e.e1024(i), io.vsrc1e.e1024(i))
}



