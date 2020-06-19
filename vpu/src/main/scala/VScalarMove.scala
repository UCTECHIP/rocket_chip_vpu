// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VScalarMove.scala
*       Author          :       liangzh
*       Revision        :       2019/10/16
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       exchange a scalar between the first element of a vector and GPR
*
*       io.fromXData1   :       input[XLEN-1:0], data, from GPR data
*       io.fromFData    :       input[FLEN:0], data, from floating-point registers data
*       io.vs2E         :       input, bundle of vectors, data, SEW relative, provide first element
*       io.vdvs3E       :       input, bundle of vectors, data, SEW relative, to fill other position
*       io.vsew         :       input[2:0], control, to select scalar output
*       io.vFromXOut    :       output, bundle of vectors, data, SEW relative, GPR data in vectors
*       io.vFromFOut    :       output, bundle of vectors, data, SEW relative, floating-point data in vectors
*       io.vToXData     :       output[XLEN-1:0], data, first fixed-point element output for GPR
*       io.vFtoXData    :       output[XLEN-1:0], data, first floating-point element output for GPR
*       io.vFtoFData    :       output[FLEN:0], data, first floating-point element output for floating-point registers
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import vpu.DataExtend._

class VScalarMove(params: VPUParams) extends VModule(params) with HasFPUParameters {
  val io = IO(new Bundle {
    val fromXData1 = Input(UInt(XLEN.W))
    val fromFData  = Input(UInt((FLEN+1).W))
    val vs2E       = Input(new FullSEWVec)
    val vdvs3E     = Input(new FullSEWVec)
    val vsew       = Input(UInt(VSEW_SZ.W))
    val vFromXOut  = Output(new FullSEWVec)
    val vFromFOut  = Output(new FullFSEWVec)
    val vXToXData  = Output(UInt(XLEN.W))
    val vFToXData  = Output(UInt(XLEN.W))
    val vFToFData  = Output(UInt((FLEN+1).W))
  })

//move a scalar value from an integer register to element 0 of the source vertor register
  io.vFromXOut.e8   := VecInit(io.fromXData1(7,0) +: io.vdvs3E.e8.drop(1))
  io.vFromXOut.e16  := VecInit(io.fromXData1(15,0) +: io.vdvs3E.e16.drop(1))
  io.vFromXOut.e32  := VecInit(io.fromXData1(31,0) +: io.vdvs3E.e32.drop(1))
  if(!io.vFromXOut.e64.isEmpty && XLEN == 64)
    io.vFromXOut.e64 := VecInit(io.fromXData1 +: io.vdvs3E.e64.drop(1))
  else if(!io.vFromXOut.e64.isEmpty && XLEN == 32)
    io.vFromXOut.e64 := VecInit(SignExtend(io.fromXData1, XLEN, 64) +: io.vdvs3E.e64.drop(1))

  if(!io.vFromXOut.e128.isEmpty)
    io.vFromXOut.e128 := VecInit(SignExtend(io.fromXData1, XLEN, 128) +: io.vdvs3E.e128.drop(1))

  if(!io.vFromXOut.e256.isEmpty)
    io.vFromXOut.e256 := VecInit(SignExtend(io.fromXData1, XLEN, 256) +: io.vdvs3E.e256.drop(1))

  if(!io.vFromXOut.e512.isEmpty)
    io.vFromXOut.e512 := VecInit(SignExtend(io.fromXData1, XLEN, 512) +: io.vdvs3E.e512.drop(1))

  if(!io.vFromXOut.e1024.isEmpty)
    io.vFromXOut.e1024 := VecInit(SignExtend(io.fromXData1, XLEN, 1024) +: io.vdvs3E.e1024.drop(1))

//move a scalar value from a floating-point register to element 0 of the source vertor register
  val fDataLength = if(ZFINX) XLEN else FLEN
  val fromFData = (ZFINX, fDataLength) match {
    case (false, 32) => ieee(unbox(io.fromFData, false.B, Some(FType.S)), FType.S)
    case (false, 64) => ieee(unbox(io.fromFData, false.B, Some(FType.D)), FType.D)
//    case (true, 32)  => io.fromXData1
    case _           => io.fromXData1
  }
  val qNaN16   = Cat(~(0.U(7.W)), 0.U(9.W))
  val qNaN32   = Cat(~(0.U(10.W)), 0.U(22.W))
  val qNaN64   = Cat(~(0.U(13.W)), 0.U(51.W))
  def cutFto16 = if(fDataLength == 64) Mux(fromFData(63,16).andR, fromFData(15,0), qNaN16)
                 else                  Mux(fromFData(31,16).andR, fromFData(15,0), qNaN16)
  val cutFto32 = if(fDataLength == 64) Mux(fromFData(63,32).andR, fromFData(31,0), qNaN32)
                 else                  fromFData

  if(!io.vFromFOut.f16.isEmpty)
    io.vFromFOut.f16  := VecInit(cutFto16 +: io.vdvs3E.e16.drop(1))

  io.vFromFOut.f32  := VecInit(cutFto32 +: io.vdvs3E.e32.drop(1))

  if(!io.vFromFOut.f64.isEmpty && fDataLength == 64)
    io.vFromFOut.f64 := VecInit(fromFData +: io.vdvs3E.e64.drop(1))
  else if(!io.vFromFOut.f64.isEmpty && fDataLength == 32)
    io.vFromFOut.f64 := VecInit(Cat(~(0.U(32.W)), fromFData) +: io.vdvs3E.e64.drop(1))

  if(!io.vFromFOut.f128.isEmpty && fDataLength == 64)
    io.vFromFOut.f128 := VecInit(Cat(~(0.U(64.W)), fromFData) +: io.vdvs3E.e128.drop(1))
  else if(!io.vFromFOut.f128.isEmpty && fDataLength == 32)
    io.vFromFOut.f128 := VecInit(Cat(~(0.U(96.W)), fromFData) +: io.vdvs3E.e128.drop(1))



//move a scalar value from element 0 of the source vertor register to an integer register
  val e32toXLEN = if(XLEN == 32) io.vs2E.e32(0) else SignExtend(io.vs2E.e32(0), 32, XLEN)

  io.vXToXData := MuxCase(SignExtend(io.vs2E.e8(0), 8, XLEN),
                   Array((io.vsew === HWordWidth) -> SignExtend(io.vs2E.e16(0), 16, XLEN),
                         (io.vsew === WordWidth)  -> e32toXLEN)
                  ++ (if(ELEN >= 64)   Array((io.vsew === DWordWidth) -> io.vs2E.e64(0)(XLEN-1,0))   else Nil)
                  ++ (if(ELEN >= 128)  Array((io.vsew === QWordWidth) -> io.vs2E.e128(0)(XLEN-1,0))  else Nil)
                  ++ (if(ELEN >= 256)  Array((io.vsew === OWordWidth) -> io.vs2E.e256(0)(XLEN-1,0))  else Nil)
                  ++ (if(ELEN >= 512)  Array((io.vsew === SWordWidth) -> io.vs2E.e512(0)(XLEN-1,0))  else Nil)
                  ++ (if(ELEN >= 1024) Array((io.vsew === TWordWidth) -> io.vs2E.e1024(0)(XLEN-1,0)) else Nil))

//move a scalar value from element 0 of the source vertor register to a floating-point register
  def nb16t32   = Cat(~(0.U(16.W)), io.vs2E.e16(0))
  def nb16t64   = Cat(~(0.U(48.W)), io.vs2E.e16(0))
  def nb32t64   = Cat(~(0.U(32.W)), io.vs2E.e32(0))
  def cut64t32  = Mux(io.vs2E.e64(0)(63,32).andR, io.vs2E.e64(0)(31,0), qNaN32)
  def cut128t32 = Mux(io.vs2E.e128(0)(127,32).andR, io.vs2E.e128(0)(31,0), qNaN32)
  def cut128t64 = Mux(io.vs2E.e128(0)(127,64).andR, io.vs2E.e128(0)(63,0), qNaN64)

  val vFToXData32 = MuxCase(qNaN32,
                     Array((io.vsew === WordWidth) -> io.vs2E.e32(0))
                    ++ (if(FSEW16)         Array((io.vsew === HWordWidth) -> nb16t32) else Nil)
                    ++ (if(FSEWMAX >= 64)  Array((io.vsew === DWordWidth) -> cut64t32) else Nil)
                    ++ (if(FSEWMAX == 128) Array((io.vsew === QWordWidth) -> cut128t32) else Nil))
  val vFToXData64 = MuxCase(qNaN64,
                     Array((io.vsew === WordWidth) -> nb32t64)
                    ++ (if(FSEW16)         Array((io.vsew === HWordWidth) -> nb16t64) else Nil)
                    ++ (if(FSEWMAX >= 64)  Array((io.vsew === DWordWidth) -> io.vs2E.e64(0)) else Nil)
                    ++ (if(FSEWMAX == 128) Array((io.vsew === QWordWidth) -> cut128t64) else Nil))
  io.vFToXData := (if(XLEN == 32) vFToXData32 else vFToXData64)

  val vFToFData32 = MuxCase(FType.S.qNaN,
                     Array((io.vsew === WordWidth) -> FType.S.recode(io.vs2E.e32(0)))
                    ++ (if(FSEW16)         Array((io.vsew === HWordWidth) -> recode(nb16t32, 0.U)) else Nil)
                    ++ (if(FSEWMAX >= 64)  Array((io.vsew === DWordWidth) -> FType.S.recode(cut64t32)) else Nil)
                    ++ (if(FSEWMAX == 128) Array((io.vsew === QWordWidth) -> FType.S.recode(cut128t32)) else Nil))
  val vFToFData64 = MuxCase(FType.D.qNaN,
                     Array((io.vsew === WordWidth) -> recode(nb32t64, 0.U))
                    ++ (if(FSEW16)         Array((io.vsew === HWordWidth) -> recode(nb16t64, 0.U)) else Nil)
                    ++ (if(FSEWMAX >= 64)  Array((io.vsew === DWordWidth) -> FType.D.recode(io.vs2E.e64(0))) else Nil)
                    ++ (if(FSEWMAX == 128) Array((io.vsew === QWordWidth) -> FType.D.recode(cut128t64)) else Nil))
  io.vFToFData := (if(FLEN == 32) vFToFData32 else vFToFData64)
}
