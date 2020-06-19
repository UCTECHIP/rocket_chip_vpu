// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFSignInject.scala
*       Author          :       liangzh
*       Revision        :       2020/01/04
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       floating-point sign-inject module
*
*       io.vsrc1f       :       input, bundle of vectors, data, UCB relative, sign-inject operand 2
*       io.vsrc2f       :       input, bundle of vertors, data, UCB relative, sign-inject operand 1
*       io.fsgnjFun     :       input[FSgnJFun_SZ-1:0], control, function select
*       io.vFSgnJOut    :       output, bundle of vectors, data, UCB relative, sign-inject result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VFSignInject(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vsrc1f    = Input(new FUCBVec)
    val vsrc2f    = Input(new FUCBVec)
    val fsgnjFun  = Input(UInt(FSgnJFun_SZ.W))
    val vFSgnJOut = Output(new FUCBVec)
  })

  for(i <- 0 until f16Depth) {
    io.vFSgnJOut.f16(i) := MuxCase(Cat(io.vsrc1f.f16(i)(16), io.vsrc2f.f16(i)(15,0)),
                            Array((io.fsgnjFun === FSgnJFun_Not) -> Cat(~io.vsrc1f.f16(i)(16), io.vsrc2f.f16(i)(15,0)),
                                  (io.fsgnjFun === FSgnJFun_Xor) -> Cat(io.vsrc1f.f16(i)(16) ^ io.vsrc2f.f16(i)(16), io.vsrc2f.f16(i)(15,0))))
  }
  for(i <- 0 until f32Depth) {
    io.vFSgnJOut.f32(i) := MuxCase(Cat(io.vsrc1f.f32(i)(32), io.vsrc2f.f32(i)(31,0)),
                            Array((io.fsgnjFun === FSgnJFun_Not) -> Cat(~io.vsrc1f.f32(i)(32), io.vsrc2f.f32(i)(31,0)),
                                  (io.fsgnjFun === FSgnJFun_Xor) -> Cat(io.vsrc1f.f32(i)(32) ^ io.vsrc2f.f32(i)(32), io.vsrc2f.f32(i)(31,0))))
  }
  for(i <- 0 until f64Depth) {
    io.vFSgnJOut.f64(i) := MuxCase(Cat(io.vsrc1f.f64(i)(64), io.vsrc2f.f64(i)(63,0)),
                            Array((io.fsgnjFun === FSgnJFun_Not) -> Cat(~io.vsrc1f.f64(i)(64), io.vsrc2f.f64(i)(63,0)),
                                  (io.fsgnjFun === FSgnJFun_Xor) -> Cat(io.vsrc1f.f64(i)(64) ^ io.vsrc2f.f64(i)(64), io.vsrc2f.f64(i)(63,0))))
  }
  for(i <- 0 until f128Depth) {
    io.vFSgnJOut.f128(i) := MuxCase(Cat(io.vsrc1f.f128(i)(128), io.vsrc2f.f128(i)(127,0)),
                             Array((io.fsgnjFun === FSgnJFun_Not) -> Cat(~io.vsrc1f.f128(i)(128), io.vsrc2f.f128(i)(127,0)),
                                   (io.fsgnjFun === FSgnJFun_Xor) -> Cat(io.vsrc1f.f128(i)(128) ^ io.vsrc2f.f128(i)(128), io.vsrc2f.f128(i)(127,0))))
  }
}
