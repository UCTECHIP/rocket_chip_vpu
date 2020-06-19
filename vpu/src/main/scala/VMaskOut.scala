// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VMaskOut.scala
*       Author          :       liangzh
*       Revision        :       2019/05/14
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       mask result elements, then keep a desired number of elements unchange, others are zeroed
*
*       io.vsrc1e       :       input, bundle of vectors, data, SEW relative, offset that keep less than elements unchanged
*       io.v0maske      :       input, bundle of vectors, control, elements all 1 bit, to mask data
*       io.vSEWOut      :       input, bundle of vectors, data, SEW relative, to be masked input
*       io.vdvs3e       :       input, bundle of vectors, data, SEW relative, to replace masked elements
*       io.v0maskm      :       input, bundle of vectors, control, elements all 1 bit, to mask data
*       io.vMLENOut     :       input, bundle of vectors, data, elements all 1 bit, to be masked input
*       io.vdm          :       input, bundle of vectors, data, elements all 1 bit, to replace masked elements
*       io.vs2Data      :       input[8*VLEN-1:0], data, to be copied
*       io.majFun       :       input[MajFun_SZ-1:0], control, select masked data out
*       io.slideFun     :       input[SlideFun_SZ-1:0], control, select mask for slideup function
*       io.isVd2SEW     :       input, control, to form a select signal
*       io.vediv        :       input[VEDIV_SZ-1:0], control, to form the select signal
*       io.vsew         :       input[VSEW_SZ-1:0], control, to form two select signals
*       io.vlmul        :       input[1:0], control, to form a select signal
*       io.veOut        :       output[8VLEN-1:0], data, masked, then composed SEW relative elements result
*       io.vmOut        :       output[VLEN-1:0], data, masked, then composed MLEN relative elements result
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VMaskOut(params: VPUParams) extends VModule(params) {
  val vlWidth = log2Ceil(VLEN)+1

  val io = IO(new Bundle {
    val vsrc1e     = Input(new SEWVec)
    ////////////////
    val v0maske    = Input(new FullSEW1wVec)
    val vSEWOut    = Input(new FullSEWVec)
    val vdvs3e     = Input(new FullSEWVec)
    ////////////////
    val v0maskm    = Input(new MLENVec)
    val vMLENOut   = Input(new MLENVec)
    val vdm        = Input(new MLENVec)
    ////////////////
    val vs2Data    = Input(UInt((8*VLEN).W))

    val majFun     = Input(UInt(MajFun_SZ.W))
    val isFullMul  = Input(Bool())
    val slideFun   = Input(UInt(SlideFun_SZ.W))
    val isVd2SEW   = Input(Bool())
    val vediv      = Input(UInt(VEDIV_SZ.W))
    val vsew       = Input(UInt(VSEW_SZ.W))
    val vlmul      = Input(UInt(VLMUL_SZ.W))

    val veOut      = Output(UInt((8*VLEN).W))
    val vmOut      = Output(UInt(VLEN.W))
  })

///////////////////////mask and compose MLEN relative elements/////////////////

  val vMLENMasked  = Wire(new MLENWideVec)  //store fixed-point or floating-point masked vector elements

//define signals for selecting output
  val MLEN = 1.U(11.W) << (io.vsew + 3.U(11.W) - io.vlmul)
//fixed-point mask elements
  for(i <- 0 until m1Depth)
    vMLENMasked.m1(i)    := Mux((io.v0maskm.m1(i)).asBool, io.vMLENOut.m1(i), io.vdm.m1(i))
  for(i <- 0 until m2Depth)
    vMLENMasked.m2(i)    := Mux((io.v0maskm.m2(i)).asBool, io.vMLENOut.m2(i), io.vdm.m2(i))
  for(i <- 0 until m4Depth)
    vMLENMasked.m4(i)    := Mux((io.v0maskm.m4(i)).asBool, io.vMLENOut.m4(i), io.vdm.m4(i))
  for(i <- 0 until m8Depth)
    vMLENMasked.m8(i)    := Mux((io.v0maskm.m8(i)).asBool, io.vMLENOut.m8(i), io.vdm.m8(i))
  for(i <- 0 until m16Depth)
    vMLENMasked.m16(i)   := Mux((io.v0maskm.m16(i)).asBool, io.vMLENOut.m16(i), io.vdm.m16(i))
  for(i <- 0 until m32Depth)
    vMLENMasked.m32(i)   := Mux((io.v0maskm.m32(i)).asBool, io.vMLENOut.m32(i), io.vdm.m32(i))
  for(i <- 0 until m64Depth)
    vMLENMasked.m64(i)   := Mux((io.v0maskm.m64(i)).asBool, io.vMLENOut.m64(i), io.vdm.m64(i))
  for(i <- 0 until m128Depth)
    vMLENMasked.m128(i)  := Mux((io.v0maskm.m128(i)).asBool, io.vMLENOut.m128(i), io.vdm.m128(i))
  for(i <- 0 until m256Depth)
    vMLENMasked.m256(i)  := Mux((io.v0maskm.m256(i)).asBool, io.vMLENOut.m256(i), io.vdm.m256(i))
  for(i <- 0 until m512Depth)
    vMLENMasked.m512(i)  := Mux((io.v0maskm.m512(i)).asBool, io.vMLENOut.m512(i), io.vdm.m512(i))
  for(i <- 0 until m1024Depth)
    vMLENMasked.m1024(i) := Mux((io.v0maskm.m1024(i)).asBool, io.vMLENOut.m1024(i), io.vdm.m1024(i))

//select output
  io.vmOut := MuxCase(0.U(VLEN.W),
               Array((MLEN === 1.U)    -> vMLENMasked.m1.asUInt,
                     (MLEN === 2.U)    -> vMLENMasked.m2.asUInt,
                     (MLEN === 4.U)    -> vMLENMasked.m4.asUInt,
                     (MLEN === 8.U)    -> vMLENMasked.m8.asUInt,
                     (MLEN === 16.U)   -> vMLENMasked.m16.asUInt,
                     (MLEN === 32.U)   -> vMLENMasked.m32.asUInt)
              ++ (if(ELEN >= 64)   Array((MLEN === 64.U)   -> vMLENMasked.m64.asUInt)   else Nil)
              ++ (if(ELEN >= 128)  Array((MLEN === 128.U)  -> vMLENMasked.m128.asUInt)  else Nil)
              ++ (if(ELEN >= 256)  Array((MLEN === 256.U)  -> vMLENMasked.m256.asUInt)  else Nil)
              ++ (if(ELEN >= 512)  Array((MLEN === 512.U)  -> vMLENMasked.m512.asUInt)  else Nil)
              ++ (if(ELEN == 1024) Array((MLEN === 1024.U) -> vMLENMasked.m1024.asUInt) else Nil))



///////////////////////mask and compose SEW relative elements/////////////////

  val v0eSlideUp  = Wire(new FullSEW1wVec) //for slideup mask
  val v0eRed      = Wire(new FullSEW1wVec) //for reduction mask
  val v0e         = MuxCase(io.v0maske, Array()
                    ++ (if(RED || FRED) Array((io.majFun === IsRed || io.majFun === IsFRed)          -> v0eRed)     else Nil)
                    ++ (if(SLIDE)       Array((io.majFun === IsSlide && io.slideFun === SlideFun_Up) -> v0eSlideUp) else Nil))
  val vSEWMasked  = Wire(new FullSEWVec)   //store masked vector elements
//  val vlmax      = (1.U << (io.vlmul +& log2Ceil(VLEN).U - 3.U - io.vsew))(vlWidth-1,0)
  val offset      = (if(ELEN >= 64) io.vsrc1e.e64(0) else io.vsrc1e.e32(0))
  val isZip       = (if(COMPRESS) io.majFun === IsZip else false.B)
//form v0eRed
  for(i <- 0 until E8Depth)    v0eRed.e8(i)    := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E16Depth)   v0eRed.e16(i)   := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E32Depth)   v0eRed.e32(i)   := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E64Depth)   v0eRed.e64(i)   := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E128Depth)  v0eRed.e128(i)  := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E256Depth)  v0eRed.e256(i)  := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E512Depth)  v0eRed.e512(i)  := (if(i == 0) 1.U(1.W) else 0.U(1.W))
  for(i <- 0 until E1024Depth) v0eRed.e1024(i) := (if(i == 0) 1.U(1.W) else 0.U(1.W))

//when i < offset, keep origin elements
  for(i <- 0 until E8Depth)    v0eSlideUp.e8(i)    := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e8(i))
  for(i <- 0 until E16Depth)   v0eSlideUp.e16(i)   := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e16(i))
  for(i <- 0 until E32Depth)   v0eSlideUp.e32(i)   := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e32(i))
  for(i <- 0 until E64Depth)   v0eSlideUp.e64(i)   := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e64(i))
  for(i <- 0 until E128Depth)  v0eSlideUp.e128(i)  := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e128(i))
  for(i <- 0 until E256Depth)  v0eSlideUp.e256(i)  := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e256(i))
  for(i <- 0 until E512Depth)  v0eSlideUp.e512(i)  := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e512(i))
  for(i <- 0 until E1024Depth) v0eSlideUp.e1024(i) := Mux(i.U(XLEN.W) < offset, 0.U(1.W), io.v0maske.e1024(i))


//mask elements
  for(i <- 0 until E8Depth)    vSEWMasked.e8(i)    := Mux((v0e.e8(i)).asBool | isZip, io.vSEWOut.e8(i), io.vdvs3e.e8(i))
  for(i <- 0 until E16Depth)   vSEWMasked.e16(i)   := Mux((v0e.e16(i)).asBool, io.vSEWOut.e16(i), io.vdvs3e.e16(i))
  for(i <- 0 until E32Depth)   vSEWMasked.e32(i)   := Mux((v0e.e32(i)).asBool, io.vSEWOut.e32(i), io.vdvs3e.e32(i))
  for(i <- 0 until E64Depth)   vSEWMasked.e64(i)   := Mux((v0e.e64(i)).asBool, io.vSEWOut.e64(i), io.vdvs3e.e64(i))
  for(i <- 0 until E128Depth)  vSEWMasked.e128(i)  := Mux((v0e.e128(i)).asBool, io.vSEWOut.e128(i), io.vdvs3e.e128(i))
  for(i <- 0 until E256Depth)  vSEWMasked.e256(i)  := Mux((v0e.e256(i)).asBool, io.vSEWOut.e256(i), io.vdvs3e.e256(i))
  for(i <- 0 until E512Depth)  vSEWMasked.e512(i)  := Mux((v0e.e512(i)).asBool, io.vSEWOut.e512(i), io.vdvs3e.e512(i))
  for(i <- 0 until E1024Depth) vSEWMasked.e1024(i) := Mux((v0e.e1024(i)).asBool, io.vSEWOut.e1024(i), io.vdvs3e.e1024(i))

  val vsewSel    = if(COMPRESS) Mux(io.majFun === IsZip, ByteWidth, io.vsew - io.vediv + io.isVd2SEW + io.isFullMul)
                   else         io.vsew - io.vediv + io.isVd2SEW + io.isFullMul

//select output
  val veOut = MuxCase(vSEWMasked.e8.asUInt,
               Array((vsewSel === HWordWidth) -> vSEWMasked.e16.asUInt,
                     (vsewSel === WordWidth)  -> vSEWMasked.e32.asUInt)
               ++ (if(ELEN >= 64)   Array((vsewSel === DWordWidth) -> vSEWMasked.e64.asUInt)   else Nil)
               ++ (if(ELEN >= 128)  Array((vsewSel === QWordWidth) -> vSEWMasked.e128.asUInt)  else Nil)
               ++ (if(ELEN >= 256)  Array((vsewSel === OWordWidth) -> vSEWMasked.e256.asUInt)  else Nil)
               ++ (if(ELEN >= 512)  Array((vsewSel === SWordWidth) -> vSEWMasked.e512.asUInt)  else Nil)
               ++ (if(ELEN == 1024) Array((vsewSel === TWordWidth) -> vSEWMasked.e1024.asUInt) else Nil))
  if(COPY) io.veOut := Mux(io.majFun === IsCopy, io.vs2Data, veOut)
  else io.veOut := veOut


}
