// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VCompress.scala
*       Author          :       sujy, liangzh
*       Revision        :       2019/11/05
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       Modules for executing VCOMPRESS instruction
*
*       io.vs1m         :       input, bundle of vectors, data, MLEN relative, operand 2, to mask operand 1
*       io.vs2e         :       input, bundle of vectors, data, SEW relative, operand 1, to be compress
*       io.vdvs3e       :       input, bundle of vectors, data, SEW relative, to fill output
*       io.enable       :       input, control, enable module
*       io.kill         :       input, control, kill process
*       io.vsew         :       input[VSEW_SZ-1:0], control, vsew field of vtype CSR, to select operand 1
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, vlmul field of vtype CSR, to select operand 1
*       io.respValid    :       output, control, show output data valid
*       io.vCompressOut :       output, bundle of vectors, data, vcompress result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VCompress(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle{
    val vs1m         = Input(new MLENVec)
    val vs2e         = Input(new FullSEWVec)
    val vdvs3e       = Input(new FullSEWVec)
    val enable       = Input(Bool())
    val kill         = Input(Bool())
    val vsew         = Input(UInt(VSEW_SZ.W))
    val vlmul        = Input(UInt(VLMUL_SZ.W))

    val respValid    = Output(Bool())
    val vCompressOut = Output(new FullSEWVec)
  })

  val reqReady :: busy :: respValid :: Nil = Enum(3)
  val zipState = RegInit(reqReady)

  val zipReg = Mem(E8Depth, UInt(8.W))
  val mask   = Wire(Vec(E8Depth, UInt(1.W)))
  val vs2Idx = RegInit(0.U(log2Ceil(VLEN).W))
  val zipIdx = RegInit(0.U((log2Ceil(VLEN)+1).W))
  val done   = vs2Idx === (VLEN-1).U

  mask := MuxCase(io.vs1m.m1,
           Array((io.vsew === ByteWidth  && io.vlmul === QuadReg) -> VecInit(io.vs1m.m2 ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                 (io.vsew === HWordWidth && io.vlmul === OctuReg) -> VecInit(io.vs1m.m2.map(i => VecInit(Seq.fill(2){i})).flatten),

                 (io.vsew === ByteWidth  && io.vlmul === DoubReg) -> VecInit(io.vs1m.m4 ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                 (io.vsew === HWordWidth && io.vlmul === QuadReg) -> VecInit(io.vs1m.m4.map(i => VecInit(Seq.fill(2){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                 (io.vsew === WordWidth  && io.vlmul === OctuReg) -> VecInit(io.vs1m.m4.map(i => VecInit(Seq.fill(4){i})).flatten),

                 (io.vsew === ByteWidth  && io.vlmul === SingReg) -> VecInit(io.vs1m.m8 ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }),
                 (io.vsew === HWordWidth && io.vlmul === DoubReg) -> VecInit(io.vs1m.m8.map(i => VecInit(Seq.fill(2){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                 (io.vsew === WordWidth  && io.vlmul === QuadReg) -> VecInit(io.vs1m.m8.map(i => VecInit(Seq.fill(4){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),

                 (io.vsew === HWordWidth && io.vlmul === SingReg) -> VecInit(io.vs1m.m16.map(i => VecInit(Seq.fill(2){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }),
                 (io.vsew === WordWidth  && io.vlmul === DoubReg) -> VecInit(io.vs1m.m16.map(i => VecInit(Seq.fill(4){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),

                 (io.vsew === WordWidth  && io.vlmul === SingReg) -> VecInit(io.vs1m.m32.map(i => VecInit(Seq.fill(4){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }))
          ++ (if(ELEN >= 64) Array((io.vsew === DWordWidth && io.vlmul === OctuReg) -> VecInit(io.vs1m.m8.map(i => VecInit(Seq.fill(8){i})).flatten),
                                   (io.vsew === DWordWidth && io.vlmul === QuadReg) -> VecInit(io.vs1m.m16.map(i => VecInit(Seq.fill(8){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                                   (io.vsew === DWordWidth && io.vlmul === DoubReg) -> VecInit(io.vs1m.m32.map(i => VecInit(Seq.fill(8){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                                   (io.vsew === DWordWidth && io.vlmul === SingReg) -> VecInit(io.vs1m.m64.map(i => VecInit(Seq.fill(8){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }))
              else Nil)
          ++ (if(ELEN >= 128) Array((io.vsew === QWordWidth && io.vlmul === OctuReg) -> VecInit(io.vs1m.m16.map(i => VecInit(Seq.fill(16){i})).flatten),
                                    (io.vsew === QWordWidth && io.vlmul === QuadReg) -> VecInit(io.vs1m.m32.map(i => VecInit(Seq.fill(16){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                                    (io.vsew === QWordWidth && io.vlmul === DoubReg) -> VecInit(io.vs1m.m64.map(i => VecInit(Seq.fill(16){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                                    (io.vsew === QWordWidth && io.vlmul === SingReg) -> VecInit(io.vs1m.m128.map(i => VecInit(Seq.fill(16){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }))
              else Nil)
          ++ (if(ELEN >= 256) Array((io.vsew === OWordWidth && io.vlmul === OctuReg) -> VecInit(io.vs1m.m32.map(i => VecInit(Seq.fill(32){i})).flatten),
                                    (io.vsew === OWordWidth && io.vlmul === QuadReg) -> VecInit(io.vs1m.m64.map(i => VecInit(Seq.fill(32){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                                    (io.vsew === OWordWidth && io.vlmul === DoubReg) -> VecInit(io.vs1m.m128.map(i => VecInit(Seq.fill(32){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                                    (io.vsew === OWordWidth && io.vlmul === SingReg) -> VecInit(io.vs1m.m256.map(i => VecInit(Seq.fill(32){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }))
              else Nil)
          ++ (if(ELEN >= 512) Array((io.vsew === SWordWidth && io.vlmul === OctuReg) -> VecInit(io.vs1m.m64.map(i => VecInit(Seq.fill(64){i})).flatten),
                                    (io.vsew === SWordWidth && io.vlmul === QuadReg) -> VecInit(io.vs1m.m128.map(i => VecInit(Seq.fill(64){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                                    (io.vsew === SWordWidth && io.vlmul === DoubReg) -> VecInit(io.vs1m.m256.map(i => VecInit(Seq.fill(64){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                                    (io.vsew === SWordWidth && io.vlmul === SingReg) -> VecInit(io.vs1m.m512.map(i => VecInit(Seq.fill(64){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }))
              else Nil)
          ++ (if(ELEN >= 1024) Array((io.vsew === TWordWidth && io.vlmul === OctuReg) -> VecInit(io.vs1m.m128.map(i => VecInit(Seq.fill(128){i})).flatten),
                                     (io.vsew === TWordWidth && io.vlmul === QuadReg) -> VecInit(io.vs1m.m256.map(i => VecInit(Seq.fill(128){i})).flatten ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                                     (io.vsew === TWordWidth && io.vlmul === DoubReg) -> VecInit(io.vs1m.m512.map(i => VecInit(Seq.fill(128){i})).flatten ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                                     (io.vsew === TWordWidth && io.vlmul === SingReg) -> VecInit(io.vs1m.m1024.map(i => VecInit(Seq.fill(128){i})).flatten ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }))
              else Nil))


  when(mask(vs2Idx).asBool) { zipReg(zipIdx) := io.vs2e.e8(vs2Idx) }
  vs2Idx := Mux(done || io.kill, 0.U, 
              Mux(zipState === busy, vs2Idx + 1.U, vs2Idx))
  zipIdx := Mux(zipState === respValid || io.kill, 0.U, 
              Mux(zipState === busy && mask(vs2Idx).asBool, zipIdx + 1.U, zipIdx))


  switch(zipState) {
    is(reqReady) {
      when(io.enable) {
        zipState := busy
      }
    }
    is(busy) {
      when(io.kill) {
        zipState := reqReady
      }.elsewhen(done) {
        zipState := respValid
      }
    }
    is(respValid) {
      zipState := reqReady
    }
  }


  for(i <- 0 until E8Depth)    io.vCompressOut.e8(i)    := Mux(i.U < zipIdx, zipReg(i), io.vdvs3e.e8(i))
  for(i <- 0 until E16Depth)   io.vCompressOut.e16(i)   := io.vdvs3e.e16(i)
  for(i <- 0 until E32Depth)   io.vCompressOut.e32(i)   := io.vdvs3e.e32(i)
  for(i <- 0 until E64Depth)   io.vCompressOut.e64(i)   := io.vdvs3e.e64(i)
  for(i <- 0 until E128Depth)  io.vCompressOut.e128(i)  := io.vdvs3e.e128(i)
  for(i <- 0 until E256Depth)  io.vCompressOut.e256(i)  := io.vdvs3e.e256(i)
  for(i <- 0 until E512Depth)  io.vCompressOut.e512(i)  := io.vdvs3e.e512(i)
  for(i <- 0 until E1024Depth) io.vCompressOut.e1024(i) := io.vdvs3e.e1024(i)

  io.respValid := zipState === respValid


}
