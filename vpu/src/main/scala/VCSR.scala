// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VCSR.scala
*       Author          :       liangzh
*       Revision        :       2019/06/29
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       CSR module for VPU
*
*       io.vstartEn     :       input, control, enable vstart value to write in
*       io.vstartIn     :       input[log2Ceil(VLEN)-1:0], data, vstart value to write in
*       io.reducevlEn   :       input, control, enable reduced vl value to write in
*       io.reducevl     :       input[XLEN-1:0], data, reduced vl value to write in
*       io.enable       :       input, control, enable to write vl and vtype CSR
*       io.killm        :       input, control, kill process, valid in mem state
*       io.src1Field    :       input[4:0], data, src1 field in an inst
*       io.dstField     :       input[4:0], data, dstination field in an inst
*       io.fromXData1   :       input[XLEN-1:0], data, AVL to write in vl CSR
*       io.fromXData2   :       input[XLEN-1:0], data, vtype value to write in vtype CSR
*       io.vtypei       :       input[10:0], data, immediate vtype value to write in vtype CSR
*       io.csrType      :       input, control, show which type of vtype value to write in
*       io.villNext     :       output, control, show CSR values are illegal, for seeing the next inst is illegal or not
*       io.vedivNext    :       output[VEDIV_SZ-1:0], data, vediv field in vtype CSR, for seeing the next inst is illegal or not
*       io.vsewNext     :       output[VSEW_SZ-1:0], data, vsew field in vtype CSR, for seeing the next inst is illegal or not
*       io.vlmulNext    :       output[VLMUL_SZ-1:0], data, vlmul field in vtype CSR, for seeing the next inst is illegal or not
*       io.vlNext       :       output[XLEN-1:0], data, vl value, for seeing the next inst is illegal or not
*       io.vill         :       output, control, showing illegal CSR values
*       io.vediv        :       output[VEDIV_SZ-1:0], data, vediv field in vtype CSR
*       io.vsew         :       output[VSEW-1:0], data, vsew field in vtype CSR
*       io.vlmul        :       output[VMUL_SZ-1:0], data, vlmul field in vtype CSR
*       io.vl           :       output[XLEN-1:0], data, vl CSR value
*       io.vstart       :       output[log2Ceil(VLEN)-1:0], data, vstart CSR value
*       io.mem_reg_vtype:       output[XLEN-1:0], data, to write in vtype CSR value, for mirroring in Rocket CSR module
*       io.mem_reg_vl   :       output[XLEN-1:0], data, to write in vl CSR value, for mirroring in Rocket CSR module
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VCSR(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vstartEn   = Input(Bool())
    val vstartIn   = Input(UInt(log2Ceil(VLEN).W))
    val reducevlEn = Input(Bool())
    val reducedvl  = Input(UInt(XLEN.W))
    val enable     = Input(Bool())
    val killm      = Input(Bool())
    val src1Field  = Input(UInt(5.W))
    val dstField   = Input(UInt(5.W))
    val fromXData1 = Input(UInt(XLEN.W))
    val fromXData2 = Input(UInt(XLEN.W))
    val vtypei     = Input(UInt(11.W))
    val csrType    = Input(UInt(1.W))

    val villNext   = Output(UInt(1.W))
    val vedivNext  = Output(UInt(VEDIV_SZ.W))
    val vsewNext   = Output(UInt(VSEW_SZ.W))
    val vlmulNext  = Output(UInt(VLMUL_SZ.W))

    val vlNext     = Output(UInt(XLEN.W))

    val vill       = Output(UInt(1.W))
    val vediv      = Output(UInt(VEDIV_SZ.W))
    val vsew       = Output(UInt(VSEW_SZ.W))
    val vlmul      = Output(UInt(VLMUL_SZ.W))

    val vl         = Output(UInt(XLEN.W))
    val vstart     = Output(UInt(log2Ceil(VLEN).W))

    val mem_reg_vtype = Output(UInt(XLEN.W))
    val mem_reg_vl    = Output(UInt(XLEN.W))
  })

  val mem_reg_enable = RegInit(false.B)
  val mem_reg_vtype  = RegInit(Cat(1.U(1.W), 0.U((XLEN-1).W)))
  val mem_reg_vl     = RegInit(0.U(XLEN.W))
  val wb_reg_vtype   = RegInit(Cat(1.U(1.W), 0.U((XLEN-1).W)))
  val wb_reg_vl      = RegInit(0.U(XLEN.W))
  val vstart         = RegInit(0.U(log2Ceil(VLEN).W))

  val isImmVer   = io.csrType === 0.U
  val vtypeNext  = Mux(isImmVer, Cat(0.U((XLEN-11).W), io.vtypei), io.fromXData2)
  val vedivNext  = vtypeNext(6, 5)
  val vsewNext   = vtypeNext(4, 2)
  val vlmulNext  = vtypeNext(1, 0)
  val vlmax      = 1.U << (vlmulNext +& log2Ceil(VLEN).U - 3.U - vsewNext)
  val vlmax2     = vlmax << 1.U
  val isRs1x0    = io.src1Field === 0.U(5.W)
  val isRdx0     = io.dstField === 0.U(5.W)
  val avl        = Mux(isRs1x0 && isRdx0, wb_reg_vl, io.fromXData1)
  val isLEvlmax  = avl <= vlmax
  val isLT2vlmax = avl < vlmax2
  val isGE2vlmax = avl >= vlmax2
  val vlNext     = Mux(isRs1x0 && !isRdx0, vlmax, MuxCase(0.U(XLEN.W),
                                                    Array(isLEvlmax  -> avl,
                                                          isLT2vlmax -> ((avl >> 1.U) + 1.U(XLEN.W)),
                                                          isGE2vlmax -> vlmax)))

  val sewMax     = log2Ceil(ELEN).U - 3.U
  val wrongEdiv  = (vsewNext - vedivNext < (log2Ceil(SELEN)-3).U) || (vedivNext > 0.U && !EDIV.B)
  val villNext   = vsewNext > sewMax || wrongEdiv
  val wen        = io.enable && !villNext
  val wrongvtype = io.enable && villNext

  mem_reg_enable := io.enable

  when(wen) {
    mem_reg_vtype := vtypeNext
    mem_reg_vl    := vlNext
  }.elsewhen(wrongvtype) {
    mem_reg_vtype := Cat(1.U(1.W), 0.U((XLEN-1).W))
    mem_reg_vl    := 0.U(XLEN.W)
  }

  when(mem_reg_enable && !io.killm) {
    wb_reg_vtype := mem_reg_vtype
    wb_reg_vl    := mem_reg_vl
  }.elsewhen(io.reducevlEn) { //???
    wb_reg_vl    := io.reducedvl
  }

  when(io.vstartEn) { vstart := io.vstartIn }

  io.villNext  := villNext
  io.vedivNext := vedivNext
  io.vsewNext  := vsewNext
  io.vlmulNext := vlmulNext
  io.vlNext    := vlNext

  io.vstart    := vstart
  io.vill      := wb_reg_vtype(XLEN-1)
  io.vediv     := wb_reg_vtype(6,5)
  io.vsew      := wb_reg_vtype(4,2)
  io.vlmul     := wb_reg_vtype(1,0)
  io.vl        := wb_reg_vl

  io.mem_reg_vl    := mem_reg_vl
  io.mem_reg_vtype := mem_reg_vtype
}

