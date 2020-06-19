// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename           :       VAllocate.scala
*       Author             :       liangzh
*       Revision           :       2020/04/12
*       Company            :       UC TECH IP
*       Department         :       MAG
*       Description        :       allocate data
*
*       io.vm2e_io_carryIn :       input, bundle of vectors, data,    elements 1 bit width, from VMLENtoSEW module, to be divided
*       io.vm2e_io_v0merge :       input, bundle of vectors, control, elements 1 bit width, from VMLENtoSEW module, to be divided
*       io.vm2e_io_v0mul   :       input, bundle of vectors, control, elements 1 bit width, from VMLENtoSEW module, to be divided
*       io.vm2e_io_v0maske :       input, bundle of vectors, control, elements 1 bit width, from VMLENtoSEW module, to be divided
*       io.vm2e_io.v0fen   :       input, bundle of vectors, control, elements 1 bit width, from VMLENtoSEW module, to be divided
*       io.isFullMul       :       input, control, showing whether multiply results are widening or not
*       io.isSrc22SEW      :       input, control, show whether source 2 requires double SEW width or not
*       io.isVd2SEW        :       input, control, show whether destination requires double SEW width or not
*       io.inStep          :       input, vectors, control, show rolling calculation is in which step
*       io.carryIn         :       output, bundle of vectors, data,    elements 1 bit width, carry in
*       io.v0merge         :       output, bundle of vectors, control, elements 1 bit width, condition in merge
*       io.v0mul           :       output, bundle of vectors, control, elements 1 bit width, a part of request valid in VMulDiv module
*       io.v0en            :       output, bundle of vectors, control, elements 1 bit width, to mask fix-point reduction operand
*       io.v0fen           :       output, bundle of vectors, control, elements 1 bit width, a part of request valid in floating-point modules
                                                                                            and to mask floating-point reduction operand
*       io.v0fexc          :       output, bundle of vectors, control, to mask fflags
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VAllocate(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vm2e_io_carryIn   = Input(new FullSEW1wVec)
    val vm2e_io_v0merge   = Input(new FullSEW1wVec)
    val vm2e_io_v0mul     = Input(new FullSEW1wVec)
    val vm2e_io_v0maske   = Input(new FullSEW1wVec)
    val vm2e_io_v0fen     = Input(new FullFSEW1wVec)

    val isFullMul         = Input(Bool())
    val isSrc22SEW        = Input(Bool())
    val isVd2SEW          = Input(Bool())
    val inStep            = Input(Vec(7, Bool()))

    val carryIn           = Output(new SEW1wVec)
    val v0merge           = Output(new SEW1wVec)
    val v0mul             = Output(new SEW1wVec)
    val v0en              = Output(new SEW1wVec)
    val v0fen             = Output(new FSEW1wVec)
    val v0fexc            = Output(new FSEW1wVec)
  })

  def ToSEW1wVec(sel: Vec[Bool], in: FullSEW1wVec) = {
    val out = MuxCase(TakeSEW1wVec(in, 0),
                Array(sel(0) -> TakeSEW1wVec(in, 4),
                      sel(1) -> TakeSEW1wVec(in, 8),
                      sel(2) -> TakeSEW1wVec(in, 12),
                      sel(3) -> TakeSEW1wVec(in, 16),
                      sel(4) -> TakeSEW1wVec(in, 20),
                      sel(5) -> TakeSEW1wVec(in, 24),
                      sel(6) -> TakeSEW1wVec(in, 28)))
    out
  }
  def ForDWMul1w(sel: Vec[Bool], in: FullSEW1wVec) = {
    val out = MuxCase(TakeSEW1wVec(in, 0),
                Array(sel(0) -> TakeSEW1wVec(in, 2),
                      sel(1) -> TakeSEW1wVec(in, 4),
                      sel(2) -> TakeSEW1wVec(in, 6),
                      sel(3) -> TakeSEW1wVec(in, 8),
                      sel(4) -> TakeSEW1wVec(in, 10),
                      sel(5) -> TakeSEW1wVec(in, 12),
                      sel(6) -> TakeSEW1wVec(in, 14)))
    out
  }
  def ForQWMul1w(sel: Vec[Bool], in: FullSEW1wVec) = {
    val out = MuxCase(TakeSEW1wVec(in, 0),
                Array(sel(0) -> TakeSEW1wVec(in, 1),
                      sel(1) -> TakeSEW1wVec(in, 2),
                      sel(2) -> TakeSEW1wVec(in, 3),
                      sel(3) -> TakeSEW1wVec(in, 4),
                      sel(4) -> TakeSEW1wVec(in, 5),
                      sel(5) -> TakeSEW1wVec(in, 6),
                      sel(6) -> TakeSEW1wVec(in, 7)))
    out
  }
//////////////////////////////////////////////////
  def ToFSEW1wVec(sel: Vec[Bool], in: FullFSEW1wVec) = {
    val out = MuxCase(TakeFSEW1wVec(in, 0),
                Array(sel(0) -> TakeFSEW1wVec(in, 2),
                      sel(1) -> TakeFSEW1wVec(in, 4),
                      sel(2) -> TakeFSEW1wVec(in, 6),
                      sel(3) -> TakeFSEW1wVec(in, 8),
                      sel(4) -> TakeFSEW1wVec(in, 10),
                      sel(5) -> TakeFSEW1wVec(in, 12),
                      sel(6) -> TakeFSEW1wVec(in, 14)))
    out
  }
  def ForFNFlags1w(sel: Vec[Bool], in: FullFSEW1wVec) = {
    val out = MuxCase(TakeFSEW1wVec(in, 0),
                Array(sel(0) -> TakeFSEW1wVec(in, 1),
                      sel(1) -> TakeFSEW1wVec(in, 2),
                      sel(2) -> TakeFSEW1wVec(in, 3),
                      sel(3) -> TakeFSEW1wVec(in, 4),
                      sel(4) -> TakeFSEW1wVec(in, 5),
                      sel(5) -> TakeFSEW1wVec(in, 6),
                      sel(6) -> TakeFSEW1wVec(in, 7)))
    out
  }


  io.carryIn := ToSEW1wVec(io.inStep,   io.vm2e_io_carryIn)
  io.v0merge := ToSEW1wVec(io.inStep,   io.vm2e_io_v0merge)
  io.v0en    := ToSEW1wVec(io.inStep,   io.vm2e_io_v0maske)
  io.v0fen   := ToFSEW1wVec(io.inStep,  io.vm2e_io_v0fen)
  io.v0fexc  := Mux(io.isSrc22SEW && !io.isVd2SEW, ForFNFlags1w(io.inStep, io.vm2e_io_v0fen), io.v0fen)

  val vmuldiv_dw_v0mul  = ForDWMul1w(io.inStep, io.vm2e_io_v0mul)
  val vmuldiv_qw_v0mul  = ForQWMul1w(io.inStep, io.vm2e_io_v0mul)

  io.v0mul := Mux(io.isFullMul && io.isVd2SEW, vmuldiv_qw_v0mul, 
                Mux(io.isFullMul && !io.isVd2SEW, vmuldiv_dw_v0mul, ToSEW1wVec(io.inStep,   io.vm2e_io_v0mul)))

}
