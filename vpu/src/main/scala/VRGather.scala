// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VRGather.scala
*       Author          :       sujy, liangzh
*       Revision        :       2019/10/11
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       implementation of vrgather inst 
*
*       io.vsew         :       input[VSEW_SZ-1:0], control, to calculate vlmax value
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, to calculate vlmax value
*       io.vediv        :       input[VEDIV_SZ-1:0], control, to select sub-element rgather result
*       io.src1Typ      :       input[Src1Typ_SZ-1:0], control, showing source 1 type
*       io.fromXData1   :       input[XLEN-1:0], data, for SEW=8 addressing vsrc2e.e8 vector value
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, operand 1, address
*       io.vsrc2e       :       input, bundle of vectors, data, SEW relative, operand 2, to be 'rgather' data
*       io.vRGatherOut  :       output, bundle of vectors, data, SEW relative, results
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VRGather(params: VPUParams) extends VModule(params) {
  val vlWidth = log2Ceil(VLEN)+1

  val io = IO(new Bundle{
    val vsew        = Input(UInt(VSEW_SZ.W))
    val vlmul       = Input(UInt(VLMUL_SZ.W))
    val vediv       = Input(UInt(VEDIV_SZ.W))
    val src1Typ     = Input(UInt(Src1Typ_SZ.W))
    val fromXData1  = Input(UInt(vlWidth.W))
    val vsrc1e      = Input(new SEWVec)
    val vsrc2e      = Input(new FullSEWVec)
    val vRGatherOut = Output(new SEWVec)
  })

  val vlmax  = (1.U << (io.vlmul +& log2Ceil(VLEN).U - 3.U - io.vsew))(vlWidth-1,0)
  val e8Idx  = Wire(Vec(e8Depth, UInt(vlWidth.W)))

  for(i <- 0 until e8Depth) e8Idx(i) := Mux(io.src1Typ === Src1_Xs, io.fromXData1, io.vsrc1e.e8(i))

  if(!EDIV){
    for(i <- 0 until e8Depth)    io.vRGatherOut.e8(i)    := Mux(e8Idx(i)           < vlmax, io.vsrc2e.e8(e8Idx(i))             , 0.U(8.W))
    for(i <- 0 until e16Depth)   io.vRGatherOut.e16(i)   := Mux(io.vsrc1e.e16(i)   < vlmax, io.vsrc2e.e16(io.vsrc1e.e16(i))    , 0.U(16.W))
    for(i <- 0 until e32Depth)   io.vRGatherOut.e32(i)   := Mux(io.vsrc1e.e32(i)   < vlmax, io.vsrc2e.e32(io.vsrc1e.e32(i))    , 0.U(32.W))
    for(i <- 0 until e64Depth)   io.vRGatherOut.e64(i)   := Mux(io.vsrc1e.e64(i)   < vlmax, io.vsrc2e.e64(io.vsrc1e.e64(i))    , 0.U(64.W))
    for(i <- 0 until e128Depth)  io.vRGatherOut.e128(i)  := Mux(io.vsrc1e.e128(i)  < vlmax, io.vsrc2e.e128(io.vsrc1e.e128(i))  , 0.U(128.W))
    for(i <- 0 until e256Depth)  io.vRGatherOut.e256(i)  := Mux(io.vsrc1e.e256(i)  < vlmax, io.vsrc2e.e256(io.vsrc1e.e256(i))  , 0.U(256.W))
    for(i <- 0 until e512Depth)  io.vRGatherOut.e512(i)  := Mux(io.vsrc1e.e512(i)  < vlmax, io.vsrc2e.e512(io.vsrc1e.e512(i))  , 0.U(512.W))
    for(i <- 0 until e1024Depth) io.vRGatherOut.e1024(i) := Mux(io.vsrc1e.e1024(i) < vlmax, io.vsrc2e.e1024(io.vsrc1e.e1024(i)), 0.U(1024.W))
  }
  else{  
    for(i <- 0 until e8Depth) 
      io.vRGatherOut.e8(i) := Mux(e8Idx(i) < vlmax, io.vsrc2e.e8(e8Idx(i)), 0.U(8.W))

    for(i <- 0 until e16Depth)
      io.vRGatherOut.e16(i) := MuxCase(Mux(io.vsrc1e.e16(i) < vlmax, io.vsrc2e.e16(io.vsrc1e.e16(i)), 0.U(16.W)), 
        Array((io.vediv === TwoDiv) -> Cat(Mux(io.vsrc1e.e8(i*2+1) < 2.U, io.vsrc2e.e8(io.vsrc1e.e8(i*2+1)), 0.U(8.W)),
                                           Mux(io.vsrc1e.e8(i*2  ) < 2.U, io.vsrc2e.e8(io.vsrc1e.e8(i*2  )), 0.U(8.W)))))

    for(i <- 0 until e32Depth)
      io.vRGatherOut.e32(i) := MuxCase(Mux(io.vsrc1e.e32(i) < vlmax, io.vsrc2e.e32(io.vsrc1e.e32(i)), 0.U(32.W)), 
        Array((io.vediv === TwoDiv)  -> Cat(Mux(io.vsrc1e.e16(i*2+1) < 2.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*2+1)+(i*2).U), 0.U(16.W)),
                                            Mux(io.vsrc1e.e16(i*2  ) < 2.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*2  )+(i*2).U), 0.U(16.W))),
              (io.vediv === FourDiv) -> Cat(Mux(io.vsrc1e.e8(i*4+3) < 4.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*4+3)+(i*4).U), 0.U(8.W)),
                                            Mux(io.vsrc1e.e8(i*4+2) < 4.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*4+2)+(i*4).U), 0.U(8.W)),
                                            Mux(io.vsrc1e.e8(i*4+1) < 4.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*4+1)+(i*4).U), 0.U(8.W)),
                                            Mux(io.vsrc1e.e8(i*4  ) < 4.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*4  )+(i*4).U), 0.U(8.W)))))


    for(i <- 0 until e64Depth)
      io.vRGatherOut.e64(i) := MuxCase(Mux(io.vsrc1e.e64(i) < vlmax, io.vsrc2e.e64(io.vsrc1e.e64(i)), 0.U(64.W)), 
        Array((io.vediv === TwoDiv)   -> Cat(Mux(io.vsrc1e.e32(i*2+1) < 2.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*2+1)+(i*2).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*2  ) < 2.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*2  )+(i*2).U), 0.U(32.W))),
              (io.vediv === FourDiv)  -> Cat(Mux(io.vsrc1e.e16(i*4+3) < 4.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*4+3)+(i*4).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*4+2) < 4.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*4+2)+(i*4).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*4+1) < 4.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*4+1)+(i*4).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*4  ) < 4.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*4  )+(i*4).U), 0.U(16.W))),
              (io.vediv === EightDiv) -> Cat(Mux(io.vsrc1e.e8(i*8+7) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+7)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8+6) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+6)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8+5) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+5)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8+4) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+4)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8+3) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+3)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8+2) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+2)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8+1) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8+1)+(i*8).U), 0.U(8.W)),
                                             Mux(io.vsrc1e.e8(i*8  ) < 8.U(8.W), io.vsrc2e.e8(io.vsrc1e.e8(i*8  )+(i*8).U), 0.U(8.W)))))

     for(i <- 0 until e128Depth)
      io.vRGatherOut.e128(i) := MuxCase(Mux(io.vsrc1e.e128(i) < vlmax, io.vsrc2e.e128(io.vsrc1e.e128(i)), 0.U(128.W)), 
        Array((io.vediv === TwoDiv)   -> Cat(Mux(io.vsrc1e.e64(i*2+1) < 2.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*2+1)+(i*2).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*2  ) < 2.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*2  )+(i*2).U), 0.U(64.W))),
              (io.vediv === FourDiv)  -> Cat(Mux(io.vsrc1e.e32(i*4+3) < 4.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*4+3)+(i*4).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*4+2) < 4.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*4+2)+(i*4).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*4+1) < 4.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*4+1)+(i*4).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*4  ) < 4.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*4  )+(i*4).U), 0.U(32.W))),
              (io.vediv === EightDiv) -> Cat(Mux(io.vsrc1e.e16(i*8+7) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+7)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8+6) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+6)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8+5) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+5)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8+4) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+4)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8+3) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+3)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8+2) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+2)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8+1) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8+1)+(i*8).U), 0.U(16.W)),
                                             Mux(io.vsrc1e.e16(i*8  ) < 8.U(16.W), io.vsrc2e.e16(io.vsrc1e.e16(i*8  )+(i*8).U), 0.U(16.W)))))

     for(i <- 0 until e256Depth)
      io.vRGatherOut.e256(i) := MuxCase(Mux(io.vsrc1e.e256(i) < vlmax, io.vsrc2e.e256(io.vsrc1e.e256(i)), 0.U(256.W)), 
        Array((io.vediv === TwoDiv)   -> Cat(Mux(io.vsrc1e.e128(i*2+1) < 2.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*2+1)+(i*2).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*2  ) < 2.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*2  )+(i*2).U), 0.U(128.W))),
              (io.vediv === FourDiv)  -> Cat(Mux(io.vsrc1e.e64(i*4+3) < 4.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*4+3)+(i*4).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*4+2) < 4.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*4+2)+(i*4).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*4+1) < 4.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*4+1)+(i*4).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*4  ) < 4.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*4  )+(i*4).U), 0.U(64.W))),
              (io.vediv === EightDiv) -> Cat(Mux(io.vsrc1e.e32(i*8+7) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+7)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8+6) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+6)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8+5) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+5)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8+4) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+4)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8+3) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+3)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8+2) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+2)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8+1) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8+1)+(i*8).U), 0.U(32.W)),
                                             Mux(io.vsrc1e.e32(i*8  ) < 8.U(32.W), io.vsrc2e.e32(io.vsrc1e.e32(i*8  )+(i*8).U), 0.U(32.W)))))

     for(i <- 0 until e512Depth)
      io.vRGatherOut.e512(i) := MuxCase(Mux(io.vsrc1e.e512(i) < vlmax, io.vsrc2e.e512(io.vsrc1e.e512(i)), 0.U(512.W)), 
        Array((io.vediv === TwoDiv)   -> Cat(Mux(io.vsrc1e.e256(i*2+1) < 2.U(256.W), io.vsrc2e.e256(io.vsrc1e.e256(i*2+1)+(i*2).U), 0.U(256.W)),
                                             Mux(io.vsrc1e.e256(i*2  ) < 2.U(256.W), io.vsrc2e.e256(io.vsrc1e.e256(i*2  )+(i*2).U), 0.U(256.W))),
              (io.vediv === FourDiv)  -> Cat(Mux(io.vsrc1e.e128(i*4+3) < 4.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*4+3)+(i*4).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*4+2) < 4.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*4+2)+(i*4).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*4+1) < 4.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*4+1)+(i*4).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*4  ) < 4.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*4  )+(i*4).U), 0.U(128.W))),
              (io.vediv === EightDiv) -> Cat(Mux(io.vsrc1e.e64(i*8+7) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+7)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8+6) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+6)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8+5) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+5)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8+4) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+4)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8+3) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+3)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8+2) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+2)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8+1) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8+1)+(i*8).U), 0.U(64.W)),
                                             Mux(io.vsrc1e.e64(i*8  ) < 8.U(64.W), io.vsrc2e.e64(io.vsrc1e.e64(i*8  )+(i*8).U), 0.U(64.W)))))

    for(i <- 0 until e1024Depth)
      io.vRGatherOut.e1024(i) := MuxCase(Mux(io.vsrc1e.e1024(i) < vlmax, io.vsrc2e.e1024(io.vsrc1e.e1024(i)), 0.U(1024.W)), 
        Array((io.vediv === TwoDiv)   -> Cat(Mux(io.vsrc1e.e512(i*2+1) < 2.U(512.W), io.vsrc2e.e512(io.vsrc1e.e512(i*2+1)+(i*2).U), 0.U(512.W)),
                                             Mux(io.vsrc1e.e512(i*2  ) < 2.U(512.W), io.vsrc2e.e512(io.vsrc1e.e512(i*2  )+(i*2).U), 0.U(512.W))),
              (io.vediv === FourDiv)  -> Cat(Mux(io.vsrc1e.e256(i*4+3) < 4.U(256.W), io.vsrc2e.e256(io.vsrc1e.e256(i*4+3)+(i*4).U), 0.U(256.W)),
                                             Mux(io.vsrc1e.e256(i*4+2) < 4.U(256.W), io.vsrc2e.e256(io.vsrc1e.e256(i*4+2)+(i*4).U), 0.U(256.W)),
                                             Mux(io.vsrc1e.e256(i*4+1) < 4.U(256.W), io.vsrc2e.e256(io.vsrc1e.e256(i*4+1)+(i*4).U), 0.U(256.W)),
                                             Mux(io.vsrc1e.e256(i*4  ) < 4.U(256.W), io.vsrc2e.e256(io.vsrc1e.e256(i*4  )+(i*4).U), 0.U(256.W))),
              (io.vediv === EightDiv) -> Cat(Mux(io.vsrc1e.e128(i*8+7) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+7)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8+6) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+6)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8+5) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+5)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8+4) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+4)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8+3) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+3)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8+2) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+2)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8+1) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8+1)+(i*8).U), 0.U(128.W)),
                                             Mux(io.vsrc1e.e128(i*8  ) < 8.U(128.W), io.vsrc2e.e128(io.vsrc1e.e128(i*8  )+(i*8).U), 0.U(128.W)))))
  }                                          

}
