// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VPUIO.scala
*       Author          :       liangzh
*       Revision        :       2019/04/26
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       io port for top module VPU
*
*       io.core.resp.toXData         :       output[XLEN-1:0], data, result from VPU to write in Rocket GPR
*       io.core.resp.rd              :       output[4:0], data, address of writing result to GPR
*
*       io.core.req.valid            :       input, control, showing inst or data valid
*       io.core.req.ready            :       output, control, showing vpu is ready to receive an inst or data
*       io.core.req.bits.inst        :       input[31:0], control, vector instruction
*       io.core.fromXData1           :       input[XLEN-1:0], data, from GPR, corresponds to rs1
*       io.core.fromXData2           :       input[XLEN-1:0], data, from GPR, corresponds to rs2
*
*       io.core.fflags_ready         :       output, control, showing whether CSR fflags is ready to be read
*       io.core.vcsr_fflags.valid    :       output, control, floating-point fflags output valid
*       io.core.vcsr_fflags.bits     :       output[FFLAGS_SZ-1:0], data, fflags
*       io.core.vcsr_frm             :       input[FRM_SZ-1:0], data, floating-point rounding mode
*       io.core.vxsat_ready          :       output, control, showing whether CSR vxsat is ready to be read
*       io.core.vcsr_vxsat.valid     :       output, control, fixed-point overflow flag output valid
*       io.core.vcsr_vxsat.bits      :       output[XSAT_SZ-1:0], data, fixed-point overflow flag
*       io.core.vcsr_vxrm            :       input[XRM_SZ-1:0], data, fixed-point rounding mode
*       io.core.vcsr_vstart          :       output[log2Ceil(VLEN)-1:0], data, value to write CSR vstart
*       io.core.vcsr_vl              :       output[XLEN-1:0], data, value to write CSR vl
*       io.core.vcsr_vtype           :       output[XLEN-1:0], data, value to write CSR vtype
*
*	io.core.ctrl_killx           :       input, control, kill the vector instruction executed at EXE stage
*	io.core.ctrl_killm           :       input, control, kill the vector instruction executed at MEM stage
*	io.core.ctrl_killw           :       input, control, kill the vector instruction executed at WB stage
*	io.core.eret                 :       input, control, take the trap finish
*	io.core.vxcpt_precise        :       output, control, precise exception signal generated at VPU, it connect to WB stage
*       io.core.vxcpt_imprecise      :       output, control, imprecise exception signal generated at VPU, it connect to ID stage
*       io.core.vxcpt_imprecise_resp :       input, control, inform VPU that exception has been received
*	io.core.vcause               :       output[XLEN-1:0], data, exception cause
*       io.core.vmtval               :       output[XLEN-1:0], data, 
*       io.core.vnack                :       output, control, imform Rocket that a vector inst has not been executed
*       io.core.dcache_blocked       :       output, control, showing VPU is occupying DCache
*
*       io.fpu.req.fromFData         :       input[FLEN-1:0], data, from floating-point registers, corresponds to rs1
*       io.fpu.req.rs1               :       output[4:0], data, address for reading rs1 from floating-point registers
*
*       io.fpu.resp.valid            :       output, control, showing address and data valid
*       io.fpu.resp.ready            :       input, control, showing ready to receive address and data
*       io.fpu.resp.bits.toFData     :       output[FLEN-1:0], data, result from VPU to FPU
*       io.fpu.resp.bits.rd          :       output[4:0], data, address of writing result to floating-point registers
*
*       io.respValid                :       input, control, showing DCache response valid
*       io.respTag                  :       input[6:0], control, return tag of request for in order receive
*       io.respSize                 :       input[2:0], control, data width
*       io.respHasData              :       input, control, showing response has data
*       io.respData                 :       input[XLEN-1:0], data, response data
*	io.respS2Xcpt               :       input HellaCacheExceptions, data, response exception information
*
*       io.reqReady                 :       input, control, showing DCache is ready to receive a request
*       io.s2Nack                   :       input, control, not-acknowledge request
*       io.reqValid                 :       output, control, showing request valid
*       io.reqAddr                  :       output[XLEN-1:0], control, address of memory data
*       io.reqTag                   :       output[6:0], control, tag of request for in order receive
*       io.reqCmd                   :       output[4:0], control, showing load or store action
*       io.reqSize                  :       output[2:0], control, data width
*       io.s1Data                   :       output[XLEN-1:0], data, data to store in memory
*       io.s1Kill                   :       output, control, kill s1 stage of DCache
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

////////////////////////////////////VPU-Rocket//////////////////////////////////
//response signals from VPU to Rocket
class VPUCoreResponse(val XLEN: Int) extends Bundle {
  val toXData = UInt(XLEN.W)
  val rd      = UInt(5.W)

  override def cloneType = new VPUCoreResponse(XLEN).asInstanceOf[this.type]
}
//request signals from Rocket to VPU
class VPUCoreRequest extends Bundle {
  val inst       = UInt(32.W)
}


//VPU-Rocket communication IO
class VPUCoreIO(val VLEN: Int, val XLEN: Int) extends Bundle {
  val resp                 = Output(new VPUCoreResponse(XLEN))
  val req                  = Flipped(Decoupled(new VPUCoreRequest))
  val fromXData1           = Input(UInt(XLEN.W))
  val fromXData2           = Input(UInt(XLEN.W))

  val fflags_ready         = Output(Bool())
  val vcsr_fflags          = Valid(UInt(VPUConstants.FFLAGS_SZ.W))
  val vcsr_frm             = Input(UInt(VPUConstants.FRM_SZ.W))
  val vxsat_ready          = Output(Bool())
  val vcsr_vxsat           = Valid(UInt(VPUConstants.XSAT_SZ.W))
  val vcsr_vxrm            = Input(UInt(VPUConstants.XRM_SZ.W))
  val vcsr_vstart          = Output(UInt(log2Ceil(VLEN).W))
  val vcsr_vl              = Output(UInt(XLEN.W))
  val vcsr_vtype           = Output(UInt(XLEN.W))

  val ctrl_killx           = Input(Bool())
  val ctrl_killm           = Input(Bool())
  val ctrl_killw           = Input(Bool())

  val eret                 = Input(Bool())
  val vxcpt_precise        = Output(Bool())
  val vxcpt_imprecise      = Output(Bool())
  val vxcpt_imprecise_resp = Input(Bool())
  val vcause               = Output(UInt(XLEN.W))
  val vmtval               = Output(UInt(XLEN.W))
  val vnack                = Output(Bool())
  val dcache_blocked       = Output(Bool())

  override def cloneType = new VPUCoreIO(VLEN, XLEN).asInstanceOf[this.type]
}



////////////////////////////////////VPU-FPU/////////////////////////////////////
//request signals from VPU to FPU
class VPUFPUResponse(val FLEN: Int) extends Bundle {
  val toFData = UInt((FLEN+1).W)
  val rd      = UInt(5.W)

  override def cloneType = new VPUFPUResponse(FLEN).asInstanceOf[this.type]
}
//response signals from FPU to VPU
class VPUFPURequest(val FLEN: Int) extends Bundle {
  val fromFData = Input(UInt((FLEN+1).W))
  val rs1       = Output(UInt(5.W))

  override def cloneType = new VPUFPURequest(FLEN).asInstanceOf[this.type]
}


//VPU-FPU communication IO
class VPUFPUIO(val FLEN: Int) extends Bundle {
  val req  = new VPUFPURequest(FLEN)
  val resp = Decoupled(new VPUFPUResponse(FLEN))

  override def cloneType = new VPUFPUIO(FLEN).asInstanceOf[this.type]
}



/////////////////////////////////Rocket HellaCache IO///////////////////////////
class AlignmentExceptions extends Bundle {
	val ld = Bool()
	val st = Bool()
}

class HellaCacheExceptions extends Bundle {
	val ma = new AlignmentExceptions
	val pf = new AlignmentExceptions
	val ae = new AlignmentExceptions
}



////////////////////////////////////VPU IO//////////////////////////////////////
class VPUIO(VLEN: Int, XLEN: Int, FLEN: Int, tagBits: Int, addrBits: Int) extends Bundle {
  val dataBits  = XLEN max FLEN
  val dataBytes = dataBits/8
  val sizeBits  = log2Ceil(log2Ceil(dataBytes)+1)

  val core        = new VPUCoreIO(VLEN, XLEN)
  val fpu         = new VPUFPUIO(FLEN)
  //temporary input
  val reqReady    = Input(Bool())
  val s2Nack      = Input(UInt(1.W))
  val respValid   = Input(Bool())
  val respTag     = Input(UInt(tagBits.W))
  val respSize    = Input(UInt(sizeBits.W))
  val respHasData = Input(Bool())
  val respData    = Input(UInt(dataBits.W))
  val respS2Xcpt  = Input(new HellaCacheExceptions)
  //temporary output
  val reqValid    = Output(Bool())
  val reqAddr     = Output(UInt(addrBits.W))
  val reqTag      = Output(UInt(tagBits.W))
  val reqCmd      = Output(UInt(5.W))
  val reqSize     = Output(UInt(sizeBits.W))
  val s1Kill      = Output(Bool())
  val s1Data      = Output(UInt(dataBits.W))

  override def cloneType = new VPUIO(VLEN, XLEN, FLEN, tagBits, addrBits).asInstanceOf[this.type]
}

