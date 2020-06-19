// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VFClass.scala
*       Author          :       liangzh
*       Revision        :       2020/01/06
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       
*
*       io.vsrc2f       :       input, bundle of vectors, data, UCB relative, to be classified values
*       io.vFClassOut   :       output, bundle of vectors, data, SEW relative, classified flags
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VFClass(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle {
    val vsrc2f     = Input(new FUCBVec)
    val vFClassOut = Output(new SEWVec)
  })
//classify floating-point value
  if(FSEW16)
    (io.vFClassOut.e16, io.vsrc2f.f16).zipped.foreach{ _ := FType.H.classify(_) }
  else
    io.vFClassOut.e16 := VecInit(Seq.fill(e16Depth){ 0.U(16.W) })

  (io.vFClassOut.e32, io.vsrc2f.f32).zipped.foreach{ _ := FType.S.classify(_) }

  if(FSEWMAX >= 64)
    (io.vFClassOut.e64, io.vsrc2f.f64).zipped.foreach{ _ := FType.D.classify(_) }
  else if(ELEN >= 64 && FSEWMAX < 64)
    io.vFClassOut.e64 := VecInit(Seq.fill(e64Depth){ 0.U(64.W) })

  if(FSEWMAX == 128)
    (io.vFClassOut.e128, io.vsrc2f.f128).zipped.foreach{ _ := FType.Q.classify(_) }
  else if(ELEN >= 128 && FSEWMAX < 128)
    io.vFClassOut.e128 := VecInit(Seq.fill(e128Depth){ 0.U(128.W) })

//useless output
  io.vFClassOut.e8 := VecInit(Seq.fill(e8Depth){ 0.U(8.W) })
  if(!io.vFClassOut.e256.isEmpty) io.vFClassOut.e256 := VecInit(Seq.fill(e256Depth){ 0.U(256.W) })
  if(!io.vFClassOut.e512.isEmpty) io.vFClassOut.e512 := VecInit(Seq.fill(e512Depth){ 0.U(512.W) })
  if(!io.vFClassOut.e1024.isEmpty) io.vFClassOut.e1024 := VecInit(Seq.fill(e1024Depth){ 0.U(1024.W) })
}
