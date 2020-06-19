// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VIota.scala
*       Author          :       yexc
*       Revision        :       2019/04/26
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       
*
*       io.req.valid          :       input, control, request valid
*       io.req.ready          :       output, control, request ready
*       io.req.bits.vs2       :       input[VLEN-1:0], data, operand 1
*       io.req.bits.v0        :       input[VLEN-1:0], data, to mask operand 1 elements 
*       io.resp.valid         :       output, control, response valid
*       io.resp.ready         :       input, control, responsr ready
*       io.resp.bits.vIotaOut :       output, bundle of vectors, data, SEW relative, results
*       io.vsew               :       input[VSEW_SZ-1:0], data, to form select signal mlen
*       io.vlmul              :       input[VLMUL_SZ-1:0], data, to form slect signal mlen
*       io.vm                 :       input, control, enable mask v0
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VIota(params: VPUParams) extends VModule(params) {
    val vlenLog2  = log2Ceil(VLEN)

    class VMIotaOps extends Bundle {
       val vs2 = UInt(VLEN.W)
       val v0  = UInt(VLEN.W)
       override def cloneType = new VMIotaOps().asInstanceOf[this.type]
     }

    class VMIotaRes extends Bundle {
       val vIotaOut = new FullSEWVec
       override def cloneType = new VMIotaRes().asInstanceOf[this.type]
     }

    val io = IO(new Bundle{
        val req = Flipped(Decoupled(new VMIotaOps))
        val resp = Decoupled(new VMIotaRes)
        val vsew = Input(UInt(VSEW_SZ.W))
        val vlmul = Input(UInt(VLMUL_SZ.W))
        val vm = Input(UInt(1.W))
    })

  val dpram = Mem(VLEN, UInt(vlenLog2.W))
  val s_ready :: s_calc :: s_done :: Nil = Enum(3)
  val state = RegInit(s_ready)
  val countOn = state === s_calc
  val (counterValue, counterWrap) = Counter(countOn, VLEN)

  val vs2ShiftReg = RegEnable(io.req.bits.vs2, 0.U, io.req.fire)
  val v0ShiftReg = RegEnable(io.req.bits.v0, 0.U, io.req.fire)
  val vOut = RegInit(0.U(vlenLog2.W))

  val mlen  = 1.U << (io.vsew + 3.U - io.vlmul)
  io.req.ready := Mux(state === s_calc, false.B, true.B)
  io.resp.valid := Mux(state === s_done, true.B, false.B)

  val vl = 1.U << (3.U - io.vsew + io.vlmul)

  switch(state)
  {
    is(s_ready)
    {
       when(io.req.fire)
       {
          state := s_calc
       }
       .otherwise
       {
          state := s_ready
       }
    }

    is(s_calc)
    {
       when(counterValue === (vl-1.U))
       {
         state := s_done
       }
    }
    
    is(s_done)
    {
       state := s_ready
       vOut := 0.U
       counterValue := 0.U
    }
  }


  when(state === s_calc)
  {
    v0ShiftReg := v0ShiftReg >> mlen
    vs2ShiftReg := vs2ShiftReg >> mlen
    vOut := vOut + (vs2ShiftReg(0) & (v0ShiftReg(0) | io.vm))
    dpram(counterValue) := vOut
  }

  for(i <- 0 until E8Depth)    io.resp.bits.vIotaOut.e8(i) := dpram(i)
  for(i <- 0 until E16Depth)   io.resp.bits.vIotaOut.e16(i) := dpram(i)
  for(i <- 0 until E32Depth)   io.resp.bits.vIotaOut.e32(i) := dpram(i)
  for(i <- 0 until E64Depth)   io.resp.bits.vIotaOut.e64(i) := dpram(i)
  for(i <- 0 until E128Depth)   io.resp.bits.vIotaOut.e128(i) := dpram(i)
  for(i <- 0 until E256Depth)   io.resp.bits.vIotaOut.e256(i) := dpram(i)
  for(i <- 0 until E512Depth)   io.resp.bits.vIotaOut.e512(i) := dpram(i)
  for(i <- 0 until E1024Depth)   io.resp.bits.vIotaOut.e1024(i) := dpram(i)
}




