// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.
// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VMulDiv.scala
*       Author          :       yexc
*       Revision        :       2019/08/13
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       initialize different MulDiv modules for different width elements multiply
*
*       io.kill                 :       input, control, to kill process
*       io.vsew                 :       input[VSEW_SZ-1:0], control, select signal
*       io.isVd2SEW             :       input, control, showing whether multiply results is quadruple SEW width or not
*       io.isFullMul            :       input, control, showing whether multiply results is over double SEW or not
*       io.sign                 :       input, control, showing whether operands are sign value or not
*       io.vxrm                 :       input[XRM_SZ-1:0], control, showing fixed-point rounding mode
*       io.isRND                :       input, control, showing whether the results need to be rounding or not
*       io.isSAT                :       input, control, showing whether the results need to be saturate or not
*       io.vMulSat              :       output[XSAT_SZ-1:0], data, showing whether fixed-point results have overflow
*       io.req.valid            :       input, control, request valid
*       io.req.ready            :       output, control, request ready
*       io.req.bits.fn          :       input, control, showing function
*       io.req.bits.vsrc1e      :       input, bundle of vectors, data, SEW relative, operand 2
*       io.req.bits.vsrc2e      :       input, bundle of vectors, data, SEW relative, operand 1
*       io.req.bits.v0en        :       input, bundle of vectors, control, elements all 1 bit, to enable submodules
*       io.resp.bits.valid      :       output, control, response valid
*       io.resp.bits.ready      :       input, control, response ready
*       io.resp.bits.vMulDivOut :       output, bundle of vectors, data, SEW relative, multiply results
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import chisel3.util.ImplicitConversions._
import vpu.DataExtend._

//TODO remove nXpr parameter
class VMulDiv(params: VPUParams, nXpr: Int = 32) extends VModule(params) {

  class VMultiplierReq extends Bundle {
    val fn     = UInt(MulFun_SZ.W)
    val dw     = UInt(1.W)
    val vsrc1e = new SEWVec
    val vsrc2e = new SEWVec
    val v0en   = new SEW1wVec

    override def cloneType = new VMultiplierReq().asInstanceOf[this.type]
  }

  class VMultiplierResp extends Bundle {
    val vMulDivOut = new SEWVec

    override def cloneType = new VMultiplierResp().asInstanceOf[this.type]
  }

  val io = IO(new Bundle {
    val kill      = Input(Bool())
    val vsew      = Input(UInt(VSEW_SZ.W))
    val isVd2SEW  = Input(Bool())
    val isFullMul = Input(Bool())
    val sign      = Input(Bool())
    val vxrm      = Input(UInt(VPUConstants.XRM_SZ.W))
    val isRND     = Input(Bool())
    val isSAT     = Input(Bool())
    val vMulSat   = Output(UInt(VPUConstants.XSAT_SZ.W))

    val req       = Flipped(Decoupled(new VMultiplierReq))
    val resp      = Decoupled(new VMultiplierResp)
  })

  val MulDiv8    = Array.fill(e8Depth)(Module(new MulDiv(vmulDivParams(0), 8, 8)).io)
  val MulDiv16   = Array.fill(e16Depth)(Module(new MulDiv(vmulDivParams(1), 16, 16)).io)
  val MulDiv32   = Array.fill(e32Depth)(Module(new MulDiv(vmulDivParams(2), 32, 32)).io)
  val MulDiv64   = Array.fill(e64Depth)(Module(new MulDiv(vmulDivParams(3), 64, 64)).io)
  val MulDiv128  = Array.fill(e128Depth)(Module(new MulDiv(vmulDivParams(4), 128, 128)).io)
  val MulDiv256  = Array.fill(e256Depth)(Module(new MulDiv(vmulDivParams(5), 256, 256)).io)
  val MulDiv512  = Array.fill(e512Depth)(Module(new MulDiv(vmulDivParams(6), 512, 512)).io)
  val MulDiv1024 = Array.fill(e1024Depth)(Module(new MulDiv(vmulDivParams(7), 1024, 1024)).io)

  val mulFullData = Wire(new SEWDoubVec)
  val mulRndData  = Wire(new SEWp1Vec)
  val mulSatData  = Wire(new SEWVec)

  def MulRounding(vxrm: UInt, v: UInt, d: Int): UInt = {
    val sraData = (v.asSInt >> d).asUInt
    val rndData = MuxCase(sraData + v(d-1),
                    Array((vxrm === 1.U) -> (sraData + (v(d-1) & (v(d-2,0) =/= 0.U || v(d).asBool))),
                          (vxrm === 2.U) -> sraData,
                          (vxrm === 3.U) -> (sraData + (!v(d).asBool && v(d-1,0) =/= 0.U))))
    rndData
  }

  val isSMul = io.isRND && io.isSAT
  val isSEFull = io.isVd2SEW && io.sign
  val isUEFull = io.isVd2SEW && !io.sign
  val fn = MuxCase(io.req.bits.fn,
            Array((io.req.bits.fn === MulFun_MulF)   -> MulFun_MulH,
                  (io.req.bits.fn === MulFun_MulFSU) -> MulFun_MulHSU,
                  (io.req.bits.fn === MulFun_MulFUS) -> MulFun_MulHSU,
                  (io.req.bits.fn === MulFun_MulFU)  -> MulFun_MulHU))
  val vsrc2e = Mux(io.req.bits.fn === MulFun_MulFUS, io.req.bits.vsrc1e, io.req.bits.vsrc2e)
  val vsrc1e = Mux(io.req.bits.fn === MulFun_MulFUS, io.req.bits.vsrc2e, io.req.bits.vsrc1e)

  val reqReady = Wire(new SEW1wVec)
  val respValidPre = Wire(new SEW1wVec)
  val respValidReg = RegInit(0.U.asTypeOf(new SEW1wVec))

  val allReqReady8    = Wire(Bool())
  val allReqReady16   = Wire(Bool())
  val allReqReady32   = Wire(Bool())
  val allReqReady64   = Wire(Bool())
  val allReqReady128  = Wire(Bool())
  val allReqReady256  = Wire(Bool())
  val allReqReady512  = Wire(Bool())
  val allReqReady1024 = Wire(Bool())


  val allRespValid8    = Wire(Bool())
  val allRespValid16   = Wire(Bool())
  val allRespValid32   = Wire(Bool())
  val allRespValid64   = Wire(Bool())
  val allRespValid128  = Wire(Bool())
  val allRespValid256  = Wire(Bool())
  val allRespValid512  = Wire(Bool())
  val allRespValid1024 = Wire(Bool())

  val enableFlagPre = Wire(Bool())
  val enableFlagReg = RegInit(false.B)

  enableFlagPre := Mux(io.req.fire(), true.B, Mux(io.resp.fire(), false.B, enableFlagReg))
  enableFlagReg := enableFlagPre

  mulFullData.e16   := MulDiv8.map(_.resp.bits.dataFull)
  mulFullData.e32   := MulDiv16.map(_.resp.bits.dataFull)
  mulFullData.e64   := MulDiv32.map(_.resp.bits.dataFull)
  mulFullData.e128  := MulDiv64.map(_.resp.bits.dataFull)
  mulFullData.e256  := MulDiv128.map(_.resp.bits.dataFull)
  mulFullData.e512  := MulDiv256.map(_.resp.bits.dataFull)
  mulFullData.e1024 := MulDiv512.map(_.resp.bits.dataFull)
  mulFullData.e2048 := MulDiv1024.map(_.resp.bits.dataFull)

  for(i <- 0 until e8Depth)    mulRndData.e9(i)    := MulRounding(io.vxrm, mulFullData.e16(i), 7)
  for(i <- 0 until e16Depth)   mulRndData.e17(i)   := MulRounding(io.vxrm, mulFullData.e32(i), 15)
  for(i <- 0 until e32Depth)   mulRndData.e33(i)   := MulRounding(io.vxrm, mulFullData.e64(i), 31)
  for(i <- 0 until e64Depth)   mulRndData.e65(i)   := MulRounding(io.vxrm, mulFullData.e128(i), 63)
  for(i <- 0 until e128Depth)  mulRndData.e129(i)  := MulRounding(io.vxrm, mulFullData.e256(i), 127)
  for(i <- 0 until e256Depth)  mulRndData.e257(i)  := MulRounding(io.vxrm, mulFullData.e512(i), 255)
  for(i <- 0 until e512Depth)  mulRndData.e513(i)  := MulRounding(io.vxrm, mulFullData.e1024(i), 511)
  for(i <- 0 until e1024Depth) mulRndData.e1025(i) := MulRounding(io.vxrm, mulFullData.e2048(i), 1023)

  for(i <- 0 until e8Depth)    mulSatData.e8(i)    := Mux(mulRndData.e9(i)(8,7) === 1.U, Cat(0.U(1.W), ~(0.U(7.W))), mulRndData.e9(i)(7,0))
  for(i <- 0 until e16Depth)   mulSatData.e16(i)   := Mux(mulRndData.e17(i)(16,15) === 1.U, Cat(0.U(1.W), ~(0.U(15.W))), mulRndData.e17(i)(15,0))
  for(i <- 0 until e32Depth)   mulSatData.e32(i)   := Mux(mulRndData.e33(i)(32,31) === 1.U, Cat(0.U(1.W), ~(0.U(31.W))), mulRndData.e33(i)(31,0))
  for(i <- 0 until e64Depth)   mulSatData.e64(i)   := Mux(mulRndData.e65(i)(64,63) === 1.U, Cat(0.U(1.W), ~(0.U(63.W))), mulRndData.e65(i)(63,0))
  for(i <- 0 until e128Depth)  mulSatData.e128(i)  := Mux(mulRndData.e129(i)(128,127) === 1.U, Cat(0.U(1.W), ~(0.U(127.W))), mulRndData.e129(i)(127,0))
  for(i <- 0 until e256Depth)  mulSatData.e256(i)  := Mux(mulRndData.e257(i)(256,255) === 1.U, Cat(0.U(1.W), ~(0.U(255.W))), mulRndData.e257(i)(255,0))
  for(i <- 0 until e512Depth)  mulSatData.e512(i)  := Mux(mulRndData.e513(i)(512,511) === 1.U, Cat(0.U(1.W), ~(0.U(511.W))), mulRndData.e513(i)(511,0))
  for(i <- 0 until e1024Depth) mulSatData.e1024(i) := Mux(mulRndData.e1025(i)(1024,1023) === 1.U, Cat(0.U(1.W), ~(0.U(1023.W))), mulRndData.e1025(i)(1023,0))

  if(!SATMUL) io.vMulSat := 0.U(1.W)
  else 
    io.vMulSat := MuxCase(0.U(1.W),
                   Array((isSMul && io.vsew === ByteWidth)  -> (VecInit(mulRndData.e9.map(_(8,7) === 1.U)).asUInt =/= 0.U),
                         (isSMul && io.vsew === HWordWidth) -> (VecInit(mulRndData.e17.map(_(16,15) === 1.U)).asUInt =/= 0.U),
                         (isSMul && io.vsew === WordWidth)  -> (VecInit(mulRndData.e33.map(_(32,31) === 1.U)).asUInt =/= 0.U))
                  ++ (if(ELEN >= 64) Array((isSMul && io.vsew === DWordWidth) -> (VecInit(mulRndData.e65.map(_(64,63) === 1.U)).asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 128) Array((isSMul && io.vsew === QWordWidth) -> (VecInit(mulRndData.e129.map(_(128,127) === 1.U)).asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 256) Array((isSMul && io.vsew === OWordWidth) -> (VecInit(mulRndData.e257.map(_(256,255) === 1.U)).asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 512) Array((isSMul && io.vsew === SWordWidth) -> (VecInit(mulRndData.e513.map(_(512,511) === 1.U)).asUInt =/= 0.U)) else Nil)
                  ++ (if(ELEN >= 1024) Array((isSMul && io.vsew === TWordWidth) -> (VecInit(mulRndData.e1025.map(_(1024,1023) === 1.U)).asUInt =/= 0.U)) else Nil))

  for(i <- 0 until e8Depth)
  {
     MulDiv8(i).req.bits.dw := io.req.bits.dw
     MulDiv8(i).req.bits.fn := fn
     MulDiv8(i).req.bits.tag := 0.U
     MulDiv8(i).req.valid := io.req.bits.v0en.e8(i).asBool && io.req.valid
     MulDiv8(i).req.bits.in1 := vsrc2e.e8(i)
     MulDiv8(i).req.bits.in2 := vsrc1e.e8(i)
     MulDiv8(i).resp.ready := io.resp.ready
     MulDiv8(i).kill := io.kill

     // leave MulDiv8(i).resp.bits.tag alone
     reqReady.e8(i) := MulDiv8(i).req.ready
     respValidPre.e8(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv8(i).resp.valid || (io.req.bits.v0en.e8(i) === 0.U && enableFlagPre), true.B, respValidReg.e8(i)))
     respValidReg.e8(i) := respValidPre.e8(i)
     io.resp.bits.vMulDivOut.e8(i) := MuxCase(MulDiv8(i).resp.bits.data, Array()
                                      ++ (if(SATMUL) Array(isSMul -> mulSatData.e8(i)) else Nil))
  }

  for(i <- 0 until e16Depth)
  {
     MulDiv16(i).req.bits.dw := io.req.bits.dw
     MulDiv16(i).req.bits.fn := fn
     MulDiv16(i).req.bits.tag := 0.U
     MulDiv16(i).req.valid := io.req.bits.v0en.e16(i).asBool && io.req.valid
     MulDiv16(i).req.bits.in1 := vsrc2e.e16(i)
     MulDiv16(i).req.bits.in2 := vsrc1e.e16(i)
     MulDiv16(i).resp.ready := io.resp.ready
     MulDiv16(i).kill := io.kill

     // leave MulDiv16(i).resp.bits.tag alone
     reqReady.e16(i) := MulDiv16(i).req.ready
     respValidPre.e16(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv16(i).resp.valid || (io.req.bits.v0en.e16(i) === 0.U && enableFlagPre), true.B, respValidReg.e16(i)))
     respValidReg.e16(i) := respValidPre.e16(i)
     io.resp.bits.vMulDivOut.e16(i) := MuxCase(Mux(io.isFullMul, MulDiv8(i).resp.bits.dataFull, MulDiv16(i).resp.bits.data), Array()
                                       ++ (if(SATMUL) Array(isSMul -> mulSatData.e16(i)) else Nil))
  }

  for(i <- 0 until e32Depth)
  {
     MulDiv32(i).req.bits.dw := io.req.bits.dw
     MulDiv32(i).req.bits.fn := fn
     MulDiv32(i).req.bits.tag := 0.U
     MulDiv32(i).req.valid := io.req.bits.v0en.e32(i).asBool && io.req.valid
     MulDiv32(i).req.bits.in1 := vsrc2e.e32(i)
     MulDiv32(i).req.bits.in2 := vsrc1e.e32(i)
     MulDiv32(i).resp.ready := io.resp.ready
     MulDiv32(i).kill := io.kill

     // leave MulDiv32(i).resp.bits.tag alone
     reqReady.e32(i) := MulDiv32(i).req.ready
     respValidPre.e32(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv32(i).resp.valid || (io.req.bits.v0en.e32(i) === 0.U && enableFlagPre), true.B, respValidReg.e32(i)))
     respValidReg.e32(i) := respValidPre.e32(i)
     io.resp.bits.vMulDivOut.e32(i) := MuxCase(Mux(io.isFullMul, MulDiv16(i).resp.bits.dataFull, MulDiv32(i).resp.bits.data), Array()
                                       ++ (if(SATMUL)  Array(isSMul   -> mulSatData.e32(i)) else Nil)
                                       ++ (if(QMULADD) Array(isSEFull -> SignExtend(MulDiv8(i).resp.bits.dataFull, 16, 32)) else Nil)
                                       ++ (if(QMULADD) Array(isUEFull -> UnsignExtend(MulDiv8(i).resp.bits.dataFull, 16, 32)) else Nil))
  }

  for(i <- 0 until e64Depth)
  {
     MulDiv64(i).req.bits.dw := io.req.bits.dw
     MulDiv64(i).req.bits.fn := fn
     MulDiv64(i).req.bits.tag := 0.U
     MulDiv64(i).req.valid := io.req.bits.v0en.e64(i).asBool && io.req.valid
     MulDiv64(i).req.bits.in1 := vsrc2e.e64(i)
     MulDiv64(i).req.bits.in2 := vsrc1e.e64(i)
     MulDiv64(i).resp.ready := io.resp.ready
     MulDiv64(i).kill := io.kill

     // leave MulDiv64(i).resp.bits.tag alone
     reqReady.e64(i) := MulDiv64(i).req.ready
     respValidPre.e64(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv64(i).resp.valid || (io.req.bits.v0en.e64(i) === 0.U && enableFlagPre), true.B, respValidReg.e64(i)))
     respValidReg.e64(i) := respValidPre.e64(i)
     io.resp.bits.vMulDivOut.e64(i) := MuxCase(Mux(io.isFullMul, MulDiv32(i).resp.bits.dataFull, MulDiv64(i).resp.bits.data), Array()
                                       ++ (if(SATMUL)  Array(isSMul   -> mulSatData.e64(i)) else Nil)
                                       ++ (if(QMULADD) Array(isSEFull -> SignExtend(MulDiv16(i).resp.bits.dataFull, 32, 64)) else Nil)
                                       ++ (if(QMULADD) Array(isUEFull -> UnsignExtend(MulDiv16(i).resp.bits.dataFull, 32, 64)) else Nil))
  }

  for(i <- 0 until e128Depth)
  {
     MulDiv128(i).req.bits.dw := io.req.bits.dw
     MulDiv128(i).req.bits.fn := fn
     MulDiv128(i).req.bits.tag := 0.U
     MulDiv128(i).req.valid := io.req.bits.v0en.e128(i).asBool && io.req.valid
     MulDiv128(i).req.bits.in1 := vsrc2e.e128(i)
     MulDiv128(i).req.bits.in2 := vsrc1e.e128(i)
     MulDiv128(i).resp.ready := io.resp.ready
     MulDiv128(i).kill := io.kill

     // leave MulDiv128(i).resp.bits.tag alone
     reqReady.e128(i) := MulDiv128(i).req.ready
     respValidPre.e128(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv128(i).resp.valid || (io.req.bits.v0en.e128(i) === 0.U && enableFlagPre), true.B, respValidReg.e128(i)))
     respValidReg.e128(i) := respValidPre.e128(i)
     io.resp.bits.vMulDivOut.e128(i) := MuxCase(Mux(io.isFullMul, MulDiv64(i).resp.bits.dataFull, MulDiv128(i).resp.bits.data), Array()
                                        ++ (if(SATMUL)  Array(isSMul   -> mulSatData.e128(i)) else Nil)
                                        ++ (if(QMULADD) Array(isSEFull -> SignExtend(MulDiv32(i).resp.bits.dataFull, 64, 128)) else Nil)
                                        ++ (if(QMULADD) Array(isUEFull -> UnsignExtend(MulDiv32(i).resp.bits.dataFull, 64, 128)) else Nil))
  }

  for(i <- 0 until e256Depth)
  {
     MulDiv256(i).req.bits.dw := io.req.bits.dw
     MulDiv256(i).req.bits.fn := fn
     MulDiv256(i).req.bits.tag := 0.U
     MulDiv256(i).req.valid := io.req.bits.v0en.e256(i).asBool && io.req.valid
     MulDiv256(i).req.bits.in1 := vsrc2e.e256(i)
     MulDiv256(i).req.bits.in2 := vsrc1e.e256(i)
     MulDiv256(i).resp.ready := io.resp.ready
     MulDiv256(i).kill := io.kill

     // leave MulDiv256(i).resp.bits.tag alone
     reqReady.e256(i) := MulDiv256(i).req.ready
     respValidPre.e256(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv256(i).resp.valid || (io.req.bits.v0en.e256(i) === 0.U && enableFlagPre), true.B, respValidReg.e256(i)))
     respValidReg.e256(i) := respValidPre.e256(i)
     io.resp.bits.vMulDivOut.e256(i) := MuxCase(Mux(io.isFullMul, MulDiv128(i).resp.bits.dataFull, MulDiv256(i).resp.bits.data), Array()
                                        ++ (if(SATMUL)  Array(isSMul   -> mulSatData.e256(i)) else Nil)
                                        ++ (if(QMULADD) Array(isSEFull -> SignExtend(MulDiv64(i).resp.bits.dataFull, 128, 256)) else Nil)
                                        ++ (if(QMULADD) Array(isUEFull -> UnsignExtend(MulDiv64(i).resp.bits.dataFull, 128, 256)) else Nil))
  }


  for(i <- 0 until e512Depth)
  {
     MulDiv512(i).req.bits.dw := io.req.bits.dw
     MulDiv512(i).req.bits.fn := fn
     MulDiv512(i).req.bits.tag := 0.U
     MulDiv512(i).req.valid := io.req.bits.v0en.e512(i).asBool && io.req.valid
     MulDiv512(i).req.bits.in1 := vsrc2e.e512(i)
     MulDiv512(i).req.bits.in2 := vsrc1e.e512(i)
     MulDiv512(i).resp.ready := io.resp.ready
     MulDiv512(i).kill := io.kill

     // leave MulDiv512(i).resp.bits.tag alone
     reqReady.e512(i) := MulDiv512(i).req.ready
     respValidPre.e512(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv512(i).resp.valid || (io.req.bits.v0en.e512(i) === 0.U && enableFlagPre), true.B, respValidReg.e512(i)))
     respValidReg.e512(i) := respValidPre.e512(i)
     io.resp.bits.vMulDivOut.e512(i) := MuxCase(Mux(io.isFullMul, MulDiv256(i).resp.bits.dataFull, MulDiv512(i).resp.bits.data), Array()
                                        ++ (if(SATMUL)  Array(isSMul   -> mulSatData.e512(i)) else Nil)
                                        ++ (if(QMULADD) Array(isSEFull -> SignExtend(MulDiv128(i).resp.bits.dataFull, 256, 512)) else Nil)
                                        ++ (if(QMULADD) Array(isUEFull -> UnsignExtend(MulDiv128(i).resp.bits.dataFull, 256, 512)) else Nil))
  }

  for(i <- 0 until e1024Depth)
  {
     MulDiv1024(i).req.bits.dw := io.req.bits.dw
     MulDiv1024(i).req.bits.fn := fn
     MulDiv1024(i).req.bits.tag := 0.U
     MulDiv1024(i).req.valid := io.req.bits.v0en.e1024(i).asBool && io.req.valid
     MulDiv1024(i).req.bits.in1 := vsrc2e.e1024(i)
     MulDiv1024(i).req.bits.in2 := vsrc1e.e1024(i)
     MulDiv1024(i).resp.ready := io.resp.ready
     MulDiv1024(i).kill := io.kill

     // leave MulDiv1024(i).resp.bits.tag alone
     reqReady.e1024(i) := MulDiv1024(i).req.ready
     respValidPre.e1024(i) := Mux(io.resp.fire(), false.B, 
                               Mux(MulDiv1024(i).resp.valid || (io.req.bits.v0en.e1024(i) === 0.U && enableFlagPre), true.B, respValidReg.e1024(i)))
     respValidReg.e1024(i) := respValidPre.e1024(i)
     io.resp.bits.vMulDivOut.e1024(i) := MuxCase(Mux(io.isFullMul, MulDiv512(i).resp.bits.dataFull, MulDiv1024(i).resp.bits.data), Array()
                                         ++ (if(SATMUL)  Array(isSMul   -> mulSatData.e1024(i)) else Nil)
                                         ++ (if(QMULADD) Array(isSEFull -> SignExtend(MulDiv256(i).resp.bits.dataFull, 512, 1024)) else Nil)
                                         ++ (if(QMULADD) Array(isUEFull -> UnsignExtend(MulDiv256(i).resp.bits.dataFull, 512, 1024)) else Nil))
  }

//  io.resp.bits.tag := io.req.bits.tag

  allReqReady8 := reqReady.e8.foldRight(1.U)(_ & _)
  allRespValid8 := respValidReg.e8.foldRight(1.U)(_ & _)

  allReqReady16 := reqReady.e16.foldRight(1.U)(_ & _)
  allRespValid16 := respValidReg.e16.foldRight(1.U)(_ & _)

  allReqReady32 := reqReady.e32.foldRight(1.U)(_ & _)
  allRespValid32 := respValidReg.e32.foldRight(1.U)(_ & _)

  allReqReady64 := reqReady.e64.foldRight(1.U)(_ & _)
  allRespValid64 := respValidReg.e64.foldRight(1.U)(_ & _)

  allReqReady128 := reqReady.e128.foldRight(1.U)(_ & _)
  allRespValid128 := respValidReg.e128.foldRight(1.U)(_ & _)

  allReqReady256 := reqReady.e256.foldRight(1.U)(_ & _)
  allRespValid256 := respValidReg.e256.foldRight(1.U)(_ & _)

  allReqReady512 := reqReady.e512.foldRight(1.U)(_ & _)
  allRespValid512 := respValidReg.e512.foldRight(1.U)(_ & _)

  allReqReady1024 := reqReady.e1024.foldRight(1.U)(_ & _)
  allRespValid1024 := respValidReg.e1024.foldRight(1.U)(_ & _)


  io.req.ready := MuxCase(false.B,
                         Array(
                                (io.vsew === ByteWidth)   -> allReqReady8,
                                (io.vsew === HWordWidth)  -> allReqReady16,
                                (io.vsew === WordWidth)   -> allReqReady32,
                                (io.vsew === DWordWidth)  -> allReqReady64,
                                (io.vsew === QWordWidth)  -> allReqReady128,
                                (io.vsew === OWordWidth)  -> allReqReady256,
                                (io.vsew === SWordWidth)  -> allReqReady512,
                                (io.vsew === TWordWidth)  -> allReqReady1024
                              )
                       )

  io.resp.valid := MuxCase(false.B,
                         Array(
                                (io.vsew === ByteWidth)  -> allRespValid8,
                                (io.vsew === HWordWidth) -> allRespValid16,
                                (io.vsew === WordWidth)  -> allRespValid32,
                                (io.vsew === DWordWidth) -> allRespValid64,
                                (io.vsew === QWordWidth) -> allRespValid128,
                                (io.vsew === OWordWidth) -> allRespValid256,
                                (io.vsew === SWordWidth) -> allRespValid512,
                                (io.vsew === TWordWidth) -> allRespValid1024
                              )
                       ) 
}



class MultiplierReq(dataBits: Int, tagBits: Int) extends Bundle {
  val fn  = UInt(4.W)
  val dw  = UInt(1.W)
  val in1 = UInt(dataBits.W)
  val in2 = UInt(dataBits.W)
  val tag = UInt(tagBits.W)
  override def cloneType = new MultiplierReq(dataBits, tagBits).asInstanceOf[this.type]
}

class MultiplierResp(dataBits: Int, tagBits: Int) extends Bundle {
  val data = UInt(dataBits.W)
  val dataFull = UInt((2*dataBits).W)
  val tag  = UInt(tagBits.W)
  override def cloneType = new MultiplierResp(dataBits, tagBits).asInstanceOf[this.type]
}

class MultiplierIO(dataBits: Int, tagBits: Int) extends Bundle {
  val req  = Flipped(Decoupled(new MultiplierReq(dataBits, tagBits)))
  val kill = Input(Bool())
  val resp = Decoupled(new MultiplierResp(dataBits, tagBits))
  override def cloneType = new MultiplierIO(dataBits, tagBits).asInstanceOf[this.type]
}

class MulDiv(cfg: VMulDivParams, width: Int = 8, nXpr: Int = 8) extends Module {
  val io = IO(new MultiplierIO(width, log2Ceil(nXpr)))
  val w = io.req.bits.in1.getWidth
  val mulw = (w + cfg.mulUnroll - 1) / cfg.mulUnroll * cfg.mulUnroll
  val fastMulW = w/2 > cfg.mulUnroll && w % (2*cfg.mulUnroll) == 0
 
  val s_ready :: s_neg_inputs :: s_mul :: s_div :: s_dummy :: s_neg_output :: s_done_mul :: s_done_div :: Nil = Enum(8)
  val state = RegInit(s_ready)

//  val req = RegInit(io.req.bits.fromBits(0.U))
  val req = RegInit(0.U.asTypeOf(io.req.bits))
  val count = RegInit(0.U(log2Ceil((w/cfg.divUnroll + 1) max (w/cfg.mulUnroll)).W))
  val neg_out = RegInit(false.B)
  val isHi = RegInit(false.B)
  val resHi = RegInit(false.B)
  val divisor = RegInit(0.U((w+1).W))
  val remainder = RegInit(0.U((2*mulw+2).W))

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  def FN_DIV  = 4.U
  def FN_DIVU = 5.U
  def FN_REM  = 6.U
  def FN_REMU = 7.U

  def FN_MUL    = 0.U
  def FN_MULH   = 1.U
  def FN_MULHSU = 12.U
  def FN_MULHU  = 14.U

  def DW_32 = false.B

  val cmdMul :: cmdHi :: lhsSigned :: rhsSigned :: Nil =
    DecodeLogic(io.req.bits.fn, List(X, X, X, X), List(
                   FN_DIV    -> List(N, N, Y, Y),
                   FN_REM    -> List(N, Y, Y, Y),
                   FN_DIVU   -> List(N, N, N, N),
                   FN_REMU   -> List(N, Y, N, N),
                   FN_MUL    -> List(Y, N, X, X),
                   FN_MULH   -> List(Y, Y, Y, Y),
                   FN_MULHU  -> List(Y, Y, N, N),
                   FN_MULHSU -> List(Y, Y, Y, N))).map(_ asBool)

  //require(w == 32 || w == 64)
  def halfWidth(req: MultiplierReq) = (w > 32).B && req.dw === DW_32

  def sext(x: Bits, halfW: Bool, signed: Bool) = {
    val sign = signed && Mux(halfW, x(w/2-1), x(w-1))
    val hi = Mux(halfW, Fill(w/2, sign), x(w-1,w/2))
    (Cat(hi, x(w/2-1,0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, halfWidth(io.req.bits), lhsSigned)
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, halfWidth(io.req.bits), rhsSigned)
  
  val subtractor = remainder(2*w,w) - divisor
  val result = Mux(resHi, remainder(2*w, w+1), remainder(w-1, 0))
  val negated_remainder = -result

  when (state === s_neg_inputs) {
    when (remainder(w-1)) {
      remainder := negated_remainder
    }
    when (divisor(w-1)) {
      divisor := subtractor
    }
    state := s_div
  }
  when (state === s_neg_output) {
    remainder := negated_remainder
    state := s_done_div
    resHi := false
  }
  when (state === s_mul) {
    val mulReg = Cat(remainder(2*mulw+1,w+1),remainder(w-1,0))
    val mplierSign = remainder(w)
    val mplier = mulReg(mulw-1,0)
    val accum = mulReg(2*mulw,mulw).asSInt
    val mpcand = divisor.asSInt
    val prod = Cat(mplierSign, mplier(cfg.mulUnroll-1, 0)).asSInt * mpcand + accum
    val nextMulReg = Cat(prod, mplier(mulw-1, cfg.mulUnroll))
    val nextMplierSign = count === mulw/cfg.mulUnroll-2 && neg_out

    val eOutMask = ((BigInt(-1) << mulw).S >> (count * cfg.mulUnroll)(log2Up(mulw)-1,0))(mulw-1,0)
    val eOut = cfg.mulEarlyOut.B && count =/= mulw/cfg.mulUnroll-1 && count =/= 0 &&
      !isHi && (mplier & ~eOutMask) === 0.U
    val eOutRes = (mulReg >> (mulw - count * cfg.mulUnroll)(log2Up(mulw)-1,0))
    val nextMulReg1 = Cat(nextMulReg(2*mulw,mulw), Mux(eOut, eOutRes, nextMulReg)(mulw-1,0))
    remainder := Cat(nextMulReg1 >> w, nextMplierSign, nextMulReg1(w-1,0))

    count := count + 1
    when (eOut || count === mulw/cfg.mulUnroll-1) {
      state := s_done_mul
      resHi := isHi
    }
  }
  when (state === s_div) {
    val unrolls = ((0 until cfg.divUnroll) scanLeft remainder) { case (rem, i) =>
      // the special case for iteration 0 is to save HW, not for correctness
      val difference = if (i == 0) subtractor else rem(2*w,w) - divisor(w-1,0)
      val less = difference(w)
      Cat(Mux(less, rem(2*w-1,w), difference(w-1,0)), rem(w-1,0), !less)
    } tail

    remainder := unrolls.last
    when (count === w/cfg.divUnroll) {
      state := Mux(neg_out, s_neg_output, s_done_div)
      resHi := isHi
      if (w % cfg.divUnroll < cfg.divUnroll - 1)
        remainder := unrolls(w % cfg.divUnroll)
    }
    count := count + 1

    val divby0 = count === 0 && !subtractor(w)
    if (cfg.divEarlyOut) {
      val divisorMSB = Log2(divisor(w-1,0), w)
      val dividendMSB = Log2(remainder(w-1,0), w)
      val eOutPos = (w-1).U + divisorMSB - dividendMSB
      val eOutZero = divisorMSB > dividendMSB
      val eOut = count === 0 && !divby0 && (eOutPos >= cfg.divUnroll || eOutZero)
      when (eOut) {
        val inc = Mux(eOutZero, (w-1).U, eOutPos) >> log2Floor(cfg.divUnroll)
        val shift = inc << log2Floor(cfg.divUnroll)
        remainder := remainder(w-1,0) << shift
        count := inc
      }
    }
    when (divby0 && !isHi) { neg_out := false }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(cmdMul, s_mul, Mux(lhs_sign || rhs_sign, s_neg_inputs, s_div))
    isHi := cmdHi
    resHi := false
    count := Mux[UInt](fastMulW.B && cmdMul && halfWidth(io.req.bits), w/cfg.mulUnroll/2, 0)
    neg_out := Mux(cmdHi, lhs_sign, lhs_sign =/= rhs_sign)
    divisor := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req := io.req.bits
  }

  val outMul = (state & (s_done_mul ^ s_done_div)) === (s_done_mul & ~s_done_div)
  val loOut = Mux(fastMulW.B && halfWidth(req) && outMul, result(w-1,w/2), result(w/2-1,0))
  val hiOut = Mux(halfWidth(req), Fill(w/2, loOut(w/2-1)), result(w-1,w/2))
  io.resp.bits.tag := req.tag
  io.resp.bits.data := Cat(hiOut, loOut)
  io.resp.bits.dataFull := Cat(remainder(2*w, w+1), remainder(w-1, 0))
  io.resp.valid := (state === s_done_mul || state === s_done_div)
  io.req.ready := state === s_ready
}
