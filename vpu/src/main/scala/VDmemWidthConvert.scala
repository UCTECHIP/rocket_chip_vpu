// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VDmemWidthConvert.scala
*       Author          :       sujy
*       Revision        :       2019/06/20
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       Width Conversion module for MEM stage,
*                               convert vs2e to vector with VLEN depth
*                               and XLEN width
*
*       io.vsew         :       input[VSEW_SZ-1:0], control, vsew field of vtype CSR
*       io.vs2e         :       input, bundle of vectors, data, SEW relative, to be converted to XLEN
*       io.vs2toXlen    :       output, bundle of vectors, data, elements all XLEN width, width converted for address generating
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VDmemWidthConvert(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle{
    val vsew     = Input(UInt(VSEW_SZ.W))
    val vs2e     = Input(new FullSEWVec)

    val vs2toXlen = Output(Vec(VLEN, UInt(XLEN.W)))
  })

  val vs2toXLEN = Wire(new SEWXLENVec)

  val default1 = Wire(Vec(VLEN, UInt(XLEN.W)))
  val default2 = Wire(Vec(VLEN, UInt(XLEN.W)))
  val default3 = Wire(Vec(VLEN, UInt(XLEN.W)))
  val default4 = Wire(Vec(VLEN, UInt(XLEN.W)))

  for(i <- 0 until E8Depth)  vs2toXLEN.e8(i)  := Cat(0.U((XLEN-8).W), io.vs2e.e8(i))
  for(i <- 0 until E16Depth) vs2toXLEN.e16(i) := Cat(0.U((XLEN-16).W), io.vs2e.e16(i))
  for(i <- 0 until E32Depth) 
    if     (XLEN == 32)  vs2toXLEN.e32(i) := io.vs2e.e32(i)(XLEN-1, 0)
    else if(XLEN == 64)  vs2toXLEN.e32(i) := Cat(0.U((XLEN-32).W), io.vs2e.e32(i))

  if(ELEN >= 64  ) for(i <- 0 until E64Depth  ) vs2toXLEN.e64(i)   := io.vs2e.e64(i)(XLEN-1, 0)
  if(ELEN >= 128 ) for(i <- 0 until E128Depth ) vs2toXLEN.e128(i)  := io.vs2e.e128(i)(XLEN-1, 0)
  if(ELEN >= 256 ) for(i <- 0 until E256Depth ) vs2toXLEN.e256(i)  := io.vs2e.e256(i)(XLEN-1, 0)
  if(ELEN >= 512 ) for(i <- 0 until E512Depth ) vs2toXLEN.e512(i)  := io.vs2e.e512(i)(XLEN-1, 0)
  if(ELEN == 1024) for(i <- 0 until E1024Depth) vs2toXLEN.e1024(i) := io.vs2e.e1024(i)(XLEN-1, 0)

  if(ELEN >= 64)   default1 := Mux(io.vsew === DWordWidth, 
                                   VecInit(vs2toXLEN.e64   ++ Seq.fill((VLEN)-E64Depth){0.U(XLEN.W)}), 
                                   default2)
  else             default1 := VecInit(Seq.fill(VLEN){0.U(XLEN.W)})

  if(ELEN >= 128)  default2 := Mux(io.vsew === QWordWidth, 
                                   VecInit(vs2toXLEN.e128  ++ Seq.fill((VLEN)-E128Depth){0.U(XLEN.W)}), 
                                   default3)
  else             default2 := VecInit(Seq.fill(VLEN){0.U(XLEN.W)})
  
  if(ELEN >= 256)  default3 := Mux(io.vsew === OWordWidth, 
                                   VecInit(vs2toXLEN.e256  ++ Seq.fill((VLEN)-E256Depth){0.U(XLEN.W)}), 
                                   default4)
  else             default3 := VecInit(Seq.fill(VLEN){0.U(XLEN.W)})

  if(ELEN >= 512)  default4 := Mux(io.vsew === SWordWidth, 
                                   VecInit(vs2toXLEN.e512  ++ Seq.fill((VLEN)-E512Depth){0.U(XLEN.W)}), 
                                   VecInit(vs2toXLEN.e1024 ++ Seq.fill((VLEN)-E1024Depth){0.U(XLEN.W)}))
  else             default4 := VecInit(Seq.fill(VLEN){0.U(XLEN.W)})
  
  io.vs2toXlen := MuxCase(default1, 
    Array((io.vsew === ByteWidth)  -> VecInit(vs2toXLEN.e8    ++ Seq.fill((VLEN)-E8Depth){0.U(8.W)}),
          (io.vsew === HWordWidth) -> VecInit(vs2toXLEN.e16   ++ Seq.fill((VLEN)-E16Depth){0.U(16.W)}),
          (io.vsew === WordWidth)  -> VecInit(vs2toXLEN.e32   ++ Seq.fill((VLEN)-E32Depth){0.U(32.W)})
          ))

}

