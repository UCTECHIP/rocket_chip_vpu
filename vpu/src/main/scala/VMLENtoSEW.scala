// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VMLENtoSEW.scala
*       Author          :       liangzh
*       Revision        :       2020/02/09
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       convert elements all 1 bit bundles to SEW relative bundles
*
*       io.isUnmasked   :       input, control, showing whether results are unmasked or not
*       io.isVd2SEW     :       input, control, showing whether destination is double SEW or not
*       io.addFun       :       input[AddFun_SZ-1:0], control, showing function: add or subtract or reverse subtract
*       io.isFullMul    :       input, control, showing whether multiply results are double SEW width
*       io.vediv        :       input[VEDIV_SZ-1:0], control, to form usefull SEW value, to select vectors
*       io.vsew         :       input[VSEW_SZ-1:0], control, to form usefull SEW value, to select vectors
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, to form select signal
*       io.vl           :       input[log2Ceil(VLEN):0], data, showing expected number of elements
*       io.vstart       :       input[log2Ceil(VLEN)-1:0], data, showing the number of elements to ignore in process
*       io.vm           :       input, control, enable mask
*       io.v0m          :       input, bundle of vectors, data, elements all 1 bit, mask
*       io.vs1m         :       input, bundle of vectors, data, elements all 1 bit, operand of compress inst
*       io.v0maskm      :       output, bundle of vectors, data, elements all 1 bit, to mask MLEN relative bundle of vectors
*       io.vs1zipm      :       output, bundle of vectors, data, elements all 1 bit, compress insts operand 2
*       io.v0maske      :       output, bundle of vectors, data, elements all 1 bit, to mask SEW relative bundle of vectors
*       io.v0merge      :       output, bundle of vectors, data, elements all 1 bit, to merge elements
*       io.v0mul        :       output, bundle of vectors, data, elements all 1 bit, to enable VMulDiv's submodules
*       io.carryIn      :       output, bundle of vectors, data, elements all 1 bit, carry in
*       io.v0fen        :       output, bundle of vectors, data, elements all 1 bit, to enable floating-point submodules
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VMLENtoSEW(params: VPUParams) extends VModule(params) {
  val vlWidth = log2Ceil(VLEN)+1

  val io = IO(new Bundle {
    val isUnmasked = Input(Bool())
    val isVd2SEW   = Input(Bool())
    val addFun     = Input(UInt(AddFun_SZ.W))
    val isFullMul  = Input(Bool())
    val vediv      = Input(UInt(VEDIV_SZ.W))
    val vsew       = Input(UInt(VSEW_SZ.W))
    val vlmul      = Input(UInt(VLMUL_SZ.W))
    val vl         = Input(UInt(vlWidth.W))
    val vstart     = Input(UInt(log2Ceil(VLEN).W))
    val vm         = Input(UInt(1.W))
    val v0m        = Input(new MLENVec)
    val vs1m       = Input(new MLENVec)

    val v0maskm    = Output(new MLENVec)
    val vs1zipm    = Output(new MLENVec)
    val v0maske    = Output(new FullSEW1wVec)
    val v0merge    = Output(new FullSEW1wVec)
    val v0mul      = Output(new FullSEW1wVec)
    val carryIn    = Output(new FullSEW1wVec)
    val v0fen      = Output(new FullFSEW1wVec)
  })

  val v0e       = Wire(new FullSEW1wVec) //store transformed mask signals, MLEN -> SEW
  val v0maskCut = Wire(new FullSEW1wVec) //store cut mask elements
  val v0maskDiv = Wire(new FullSEW1wVec) //store copy EDIV times elements
  val v0mule    = Wire(new FullSEW1wVec) //TODO merge into v0e
  val v0muleCut = Wire(new FullSEW1wVec) //TODO merge into v0maskCut
  val v0muleDiv = Wire(new FullSEW1wVec) //TODO merge into v0maskDiv

  val vsew      = io.vsew - io.vediv
  val vsewSel   = vsew + io.isVd2SEW + io.isFullMul
  val vlmulSel  = (io.vlmul + io.isVd2SEW + io.isFullMul) & 3.U(2.W)

  val isCarryOne    = io.addFun === AddFun_Sub || io.addFun === AddFun_Rev //for sub and reverse sub
  val isCarryPlusS  = io.addFun === AddFun_Adc && io.vlmul === SingReg //for adc, LMUL=1
  val isCarryPlusD  = io.addFun === AddFun_Adc && io.vlmul === DoubReg //for adc, LMUL=2
  val isCarryPlusQ  = io.addFun === AddFun_Adc && io.vlmul === QuadReg //for adc, LMUL=4
  val isCarryPlusO  = io.addFun === AddFun_Adc && io.vlmul === OctuReg //for adc, LMUL=8
  val isCarryMinusS = io.addFun === AddFun_Sbc && io.vlmul === SingReg //for sbc, LMUL=1
  val isCarryMinusD = io.addFun === AddFun_Sbc && io.vlmul === DoubReg //for sbc, LMUL=2
  val isCarryMinusQ = io.addFun === AddFun_Sbc && io.vlmul === QuadReg //for sbc, LMUL=4
  val isCarryMinusO = io.addFun === AddFun_Sbc && io.vlmul === OctuReg //for sbc, LMUL=8

  io.v0merge := v0e

//////////*for masking MLEN relative elements*//////////
  for(i <- 0 until m1Depth)    io.v0maskm.m1(i)    := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m1(i)    | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m2Depth)    io.v0maskm.m2(i)    := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m2(i)    | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m4Depth)    io.v0maskm.m4(i)    := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m4(i)    | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m8Depth)    io.v0maskm.m8(i)    := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m8(i)    | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m16Depth)   io.v0maskm.m16(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m16(i)   | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m32Depth)   io.v0maskm.m32(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m32(i)   | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m64Depth)   io.v0maskm.m64(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m64(i)   | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m128Depth)  io.v0maskm.m128(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m128(i)  | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m256Depth)  io.v0maskm.m256(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m256(i)  | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m512Depth)  io.v0maskm.m512(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m512(i)  | io.isUnmasked | io.vm, 0.U(1.W))
  for(i <- 0 until m1024Depth) io.v0maskm.m1024(i) := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), io.v0m.m1024(i) | io.isUnmasked | io.vm, 0.U(1.W))


//////////*convert v0m, vs1m which width is MLEN to SEW  for other operation*//////////
//for masking elements and enable other modules except VMulDiv module
  v0e.e8 := MuxCase(VecInit(io.v0m.m8.map(_ | io.vm) ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }),
             Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m4.map(_ | io.vm) ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                   (vlmulSel === QuadReg) -> VecInit(io.v0m.m2.map(_ | io.vm) ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                   (vlmulSel === OctuReg) -> VecInit(io.v0m.m1.map(_ | io.vm))))
  v0e.e16 := MuxCase(VecInit(io.v0m.m16.map(_ | io.vm) ++ Seq.fill(E16Depth-m16Depth){ 0.U(1.W) }),
              Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m8.map(_ | io.vm) ++ Seq.fill(E16Depth-m8Depth){ 0.U(1.W) }),
                    (vlmulSel === QuadReg) -> VecInit(io.v0m.m4.map(_ | io.vm) ++ Seq.fill(E16Depth-m4Depth){ 0.U(1.W) }),
                    (vlmulSel === OctuReg) -> VecInit(io.v0m.m2.map(_ | io.vm))))
  v0e.e32 := MuxCase(VecInit(io.v0m.m32.map(_ | io.vm) ++ Seq.fill(E32Depth-m32Depth){ 0.U(1.W) }),
              Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m16.map(_ | io.vm) ++ Seq.fill(E32Depth-m16Depth){ 0.U(1.W) }),
                    (vlmulSel === QuadReg) -> VecInit(io.v0m.m8.map(_ | io.vm)  ++ Seq.fill(E32Depth-m8Depth){ 0.U(1.W) }),
                    (vlmulSel === OctuReg) -> VecInit(io.v0m.m4.map(_ | io.vm))))
  if(ELEN >= 64)
    v0e.e64 := MuxCase(VecInit(io.v0m.m64.map(_ | io.vm) ++ Seq.fill(E64Depth-m64Depth){ 0.U(1.W) }),
                Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m32.map(_ | io.vm) ++ Seq.fill(E64Depth-m32Depth){ 0.U(1.W) }),
                      (vlmulSel === QuadReg) -> VecInit(io.v0m.m16.map(_ | io.vm) ++ Seq.fill(E64Depth-m16Depth){ 0.U(1.W) }),
                      (vlmulSel === OctuReg) -> VecInit(io.v0m.m8.map(_ | io.vm))))
  if(ELEN >= 128)
    v0e.e128 := MuxCase(VecInit(io.v0m.m128.map(_ | io.vm) ++ Seq.fill(E128Depth-m128Depth){ 0.U(1.W) }),
                 Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m64.map(_ | io.vm) ++ Seq.fill(E128Depth-m64Depth){ 0.U(1.W) }),
                       (vlmulSel === QuadReg) -> VecInit(io.v0m.m32.map(_ | io.vm) ++ Seq.fill(E128Depth-m32Depth){ 0.U(1.W) }),
                       (vlmulSel === OctuReg) -> VecInit(io.v0m.m16.map(_ | io.vm))))
  if(ELEN >= 256)
    v0e.e256 := MuxCase(VecInit(io.v0m.m256.map(_ | io.vm) ++ Seq.fill(E256Depth-m256Depth){ 0.U(1.W) }),
                 Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m128.map(_ | io.vm) ++ Seq.fill(E256Depth-m128Depth){ 0.U(1.W) }),
                       (vlmulSel === QuadReg) -> VecInit(io.v0m.m64.map(_ | io.vm)  ++ Seq.fill(E256Depth-m64Depth){ 0.U(1.W) }),
                       (vlmulSel === OctuReg) -> VecInit(io.v0m.m32.map(_ | io.vm))))
  if(ELEN >= 512)
    v0e.e512 := MuxCase(VecInit(io.v0m.m512.map(_ | io.vm) ++ Seq.fill(E512Depth-m512Depth){ 0.U(1.W) }),
                 Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m256.map(_ | io.vm) ++ Seq.fill(E512Depth-m256Depth){ 0.U(1.W) }),
                       (vlmulSel === QuadReg) -> VecInit(io.v0m.m128.map(_ | io.vm) ++ Seq.fill(E512Depth-m128Depth){ 0.U(1.W) }),
                       (vlmulSel === OctuReg) -> VecInit(io.v0m.m64.map(_ | io.vm))))
  if(ELEN == 1024)
    v0e.e1024 := MuxCase(VecInit(io.v0m.m1024.map(_ | io.vm) ++ Seq.fill(E1024Depth-m1024Depth){ 0.U(1.W) }),
                  Array((vlmulSel === DoubReg) -> VecInit(io.v0m.m512.map(_ | io.vm) ++ Seq.fill(E1024Depth-m512Depth){ 0.U(1.W) }),
                        (vlmulSel === QuadReg) -> VecInit(io.v0m.m256.map(_ | io.vm) ++ Seq.fill(E1024Depth-m256Depth){ 0.U(1.W) }),
                        (vlmulSel === OctuReg) -> VecInit(io.v0m.m128.map(_ | io.vm))))

//for enabling VMulDiv submodule
  v0mule.e8 := MuxCase(VecInit(io.v0m.m8.map(_ | io.vm) ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }),
                Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m4.map(_ | io.vm) ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                      (io.vlmul === QuadReg) -> VecInit(io.v0m.m2.map(_ | io.vm) ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                      (io.vlmul === OctuReg) -> VecInit(io.v0m.m1.map(_ | io.vm))))
  v0mule.e16 := MuxCase(VecInit(io.v0m.m16.map(_ | io.vm) ++ Seq.fill(E16Depth-m16Depth){ 0.U(1.W) }),
                 Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m8.map(_ | io.vm) ++ Seq.fill(E16Depth-m8Depth){ 0.U(1.W) }),
                       (io.vlmul === QuadReg) -> VecInit(io.v0m.m4.map(_ | io.vm) ++ Seq.fill(E16Depth-m4Depth){ 0.U(1.W) }),
                       (io.vlmul === OctuReg) -> VecInit(io.v0m.m2.map(_ | io.vm))))
  v0mule.e32 := MuxCase(VecInit(io.v0m.m32.map(_ | io.vm) ++ Seq.fill(E32Depth-m32Depth){ 0.U(1.W) }),
                 Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m16.map(_ | io.vm) ++ Seq.fill(E32Depth-m16Depth){ 0.U(1.W) }),
                       (io.vlmul === QuadReg) -> VecInit(io.v0m.m8.map(_ | io.vm)  ++ Seq.fill(E32Depth-m8Depth){ 0.U(1.W) }),
                       (io.vlmul === OctuReg) -> VecInit(io.v0m.m4.map(_ | io.vm))))
  if(ELEN >= 64)
    v0mule.e64 := MuxCase(VecInit(io.v0m.m64.map(_ | io.vm) ++ Seq.fill(E64Depth-m64Depth){ 0.U(1.W) }),
                   Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m32.map(_ | io.vm) ++ Seq.fill(E64Depth-m32Depth){ 0.U(1.W) }),
                         (io.vlmul === QuadReg) -> VecInit(io.v0m.m16.map(_ | io.vm) ++ Seq.fill(E64Depth-m16Depth){ 0.U(1.W) }),
                         (io.vlmul === OctuReg) -> VecInit(io.v0m.m8.map(_ | io.vm))))
  if(ELEN >= 128)
    v0mule.e128 := MuxCase(VecInit(io.v0m.m128.map(_ | io.vm) ++ Seq.fill(E128Depth-m128Depth){ 0.U(1.W) }),
                    Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m64.map(_ | io.vm) ++ Seq.fill(E128Depth-m64Depth){ 0.U(1.W) }),
                          (io.vlmul === QuadReg) -> VecInit(io.v0m.m32.map(_ | io.vm) ++ Seq.fill(E128Depth-m32Depth){ 0.U(1.W) }),
                          (io.vlmul === OctuReg) -> VecInit(io.v0m.m16.map(_ | io.vm))))
  if(ELEN >= 256)
    v0mule.e256 := MuxCase(VecInit(io.v0m.m256.map(_ | io.vm) ++ Seq.fill(E256Depth-m256Depth){ 0.U(1.W) }),
                    Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m128.map(_ | io.vm) ++ Seq.fill(E256Depth-m128Depth){ 0.U(1.W) }),
                          (io.vlmul === QuadReg) -> VecInit(io.v0m.m64.map(_ | io.vm)  ++ Seq.fill(E256Depth-m64Depth){ 0.U(1.W) }),
                          (io.vlmul === OctuReg) -> VecInit(io.v0m.m32.map(_ | io.vm))))
  if(ELEN >= 512)
    v0mule.e512 := MuxCase(VecInit(io.v0m.m512.map(_ | io.vm) ++ Seq.fill(E512Depth-m512Depth){ 0.U(1.W) }),
                    Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m256.map(_ | io.vm) ++ Seq.fill(E512Depth-m256Depth){ 0.U(1.W) }),
                          (io.vlmul === QuadReg) -> VecInit(io.v0m.m128.map(_ | io.vm) ++ Seq.fill(E512Depth-m128Depth){ 0.U(1.W) }),
                          (io.vlmul === OctuReg) -> VecInit(io.v0m.m64.map(_ | io.vm))))
  if(ELEN == 1024)
    v0mule.e1024 := MuxCase(VecInit(io.v0m.m1024.map(_ | io.vm) ++ Seq.fill(E1024Depth-m1024Depth){ 0.U(1.W) }),
                     Array((io.vlmul === DoubReg) -> VecInit(io.v0m.m512.map(_ | io.vm) ++ Seq.fill(E1024Depth-m512Depth){ 0.U(1.W) }),
                           (io.vlmul === QuadReg) -> VecInit(io.v0m.m256.map(_ | io.vm) ++ Seq.fill(E1024Depth-m256Depth){ 0.U(1.W) }),
                           (io.vlmul === OctuReg) -> VecInit(io.v0m.m128.map(_ | io.vm))))

//for compress module
  for(i <- 0 until m1Depth)    io.vs1zipm.m1(i)    := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m1(i),    0.U(1.W))
  for(i <- 0 until m2Depth)    io.vs1zipm.m2(i)    := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m2(i),    0.U(1.W))
  for(i <- 0 until m4Depth)    io.vs1zipm.m4(i)    := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m4(i),    0.U(1.W))
  for(i <- 0 until m8Depth)    io.vs1zipm.m8(i)    := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m8(i),    0.U(1.W))
  for(i <- 0 until m16Depth)   io.vs1zipm.m16(i)   := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m16(i),   0.U(1.W))
  for(i <- 0 until m32Depth)   io.vs1zipm.m32(i)   := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m32(i),   0.U(1.W))
  for(i <- 0 until m64Depth)   io.vs1zipm.m64(i)   := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m64(i),   0.U(1.W))
  for(i <- 0 until m128Depth)  io.vs1zipm.m128(i)  := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m128(i),  0.U(1.W))
  for(i <- 0 until m256Depth)  io.vs1zipm.m256(i)  := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m256(i),  0.U(1.W))
  for(i <- 0 until m512Depth)  io.vs1zipm.m512(i)  := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m512(i),  0.U(1.W))
  for(i <- 0 until m1024Depth) io.vs1zipm.m1024(i) := Mux(i.U(vlWidth.W) < io.vl, io.vs1m.m1024(i), 0.U(1.W))

//cut elements
  for(i <- 0 until E8Depth)    v0maskCut.e8(i)    := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e8(i)    | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E16Depth)   v0maskCut.e16(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e16(i)   | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E32Depth)   v0maskCut.e32(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e32(i)   | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E64Depth)   v0maskCut.e64(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e64(i)   | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E128Depth)  v0maskCut.e128(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e128(i)  | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E256Depth)  v0maskCut.e256(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e256(i)  | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E512Depth)  v0maskCut.e512(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e512(i)  | io.isUnmasked, 0.U(1.W))
  for(i <- 0 until E1024Depth) v0maskCut.e1024(i) := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0e.e1024(i) | io.isUnmasked, 0.U(1.W))

  for(i <- 0 until E8Depth)    v0muleCut.e8(i)    := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e8(i),    0.U(1.W))
  for(i <- 0 until E16Depth)   v0muleCut.e16(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e16(i),   0.U(1.W))
  for(i <- 0 until E32Depth)   v0muleCut.e32(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e32(i),   0.U(1.W))
  for(i <- 0 until E64Depth)   v0muleCut.e64(i)   := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e64(i),   0.U(1.W))
  for(i <- 0 until E128Depth)  v0muleCut.e128(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e128(i),  0.U(1.W))
  for(i <- 0 until E256Depth)  v0muleCut.e256(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e256(i),  0.U(1.W))
  for(i <- 0 until E512Depth)  v0muleCut.e512(i)  := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e512(i),  0.U(1.W))
  for(i <- 0 until E1024Depth) v0muleCut.e1024(i) := Mux((io.vstart <= i.U(vlWidth.W)) && (i.U(vlWidth.W) < io.vl), v0mule.e1024(i), 0.U(1.W))

//copy elements EDIV times
  if(!EDIV) {
    v0maskDiv := v0maskCut
    v0muleDiv := v0muleCut
  } else {
    v0maskDiv.e8 := MuxCase(v0maskCut.e8,
                                       Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e16.map(i => VecInit(i,i)).flatten),
                                             (io.vediv === FourDiv) -> VecInit(v0maskCut.e32.map(i => VecInit(i,i,i,i)).flatten))
                    ++ (if(ELEN >= 64) Array((io.vediv === EightDiv) -> VecInit(v0maskCut.e64.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    v0maskDiv.e16 := MuxCase(v0maskCut.e16,
                                         Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e32.map(i => VecInit(i,i)).flatten))
                     ++ (if(ELEN >= 64)  Array((io.vediv === FourDiv) -> VecInit(v0maskCut.e64.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                     ++ (if(ELEN >= 128) Array((io.vediv === EightDiv) -> VecInit(v0maskCut.e128.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    v0maskDiv.e32 := MuxCase(v0maskCut.e32, Array()
                     ++ (if(ELEN >= 64)  Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e64.map(i => VecInit(i,i)).flatten)) else Nil)
                     ++ (if(ELEN >= 128) Array((io.vediv === FourDiv) -> VecInit(v0maskCut.e128.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                     ++ (if(ELEN >= 256) Array((io.vediv === EightDiv) -> VecInit(v0maskCut.e256.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 64)
      v0maskDiv.e64 := MuxCase(v0maskCut.e64, Array()
                       ++ (if(ELEN >= 128) Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e128.map(i => VecInit(i,i)).flatten)) else Nil)
                       ++ (if(ELEN >= 256) Array((io.vediv === FourDiv) -> VecInit(v0maskCut.e256.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                       ++ (if(ELEN >= 512) Array((io.vediv === EightDiv) -> VecInit(v0maskCut.e512.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 128)
      v0maskDiv.e128 := MuxCase(v0maskCut.e128, Array()
                        ++ (if(ELEN >= 256)  Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e256.map(i => VecInit(i,i)).flatten)) else Nil)
                        ++ (if(ELEN >= 512)  Array((io.vediv === FourDiv) -> VecInit(v0maskCut.e512.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                        ++ (if(ELEN >= 1024) Array((io.vediv === EightDiv) -> VecInit(v0maskCut.e1024.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 256)
      v0maskDiv.e256 := MuxCase(v0maskCut.e256, Array()
                        ++ (if(ELEN >= 512)  Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e512.map(i => VecInit(i,i)).flatten)) else Nil)
                        ++ (if(ELEN >= 1024) Array((io.vediv === FourDiv) -> VecInit(v0maskCut.e1024.map(i => VecInit(i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 512)
      v0maskDiv.e512 := MuxCase(v0maskCut.e512, Array()
                        ++ (if(ELEN >= 1024) Array((io.vediv === TwoDiv) -> VecInit(v0maskCut.e1024.map(i => VecInit(i,i)).flatten)) else Nil))
    if(ELEN == 1024)
      v0maskDiv.e1024 := v0maskCut.e1024
    //////////////////////////////////////////////
    v0muleDiv.e8 := MuxCase(v0muleCut.e8,
                                       Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e16.map(i => VecInit(i,i)).flatten),
                                             (io.vediv === FourDiv) -> VecInit(v0muleCut.e32.map(i => VecInit(i,i,i,i)).flatten))
                    ++ (if(ELEN >= 64) Array((io.vediv === EightDiv) -> VecInit(v0muleCut.e64.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    v0muleDiv.e16 := MuxCase(v0muleCut.e16,
                                         Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e32.map(i => VecInit(i,i)).flatten))
                     ++ (if(ELEN >= 64)  Array((io.vediv === FourDiv) -> VecInit(v0muleCut.e64.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                     ++ (if(ELEN >= 128) Array((io.vediv === EightDiv) -> VecInit(v0muleCut.e128.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    v0muleDiv.e32 := MuxCase(v0muleCut.e32, Array()
                     ++ (if(ELEN >= 64)  Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e64.map(i => VecInit(i,i)).flatten)) else Nil)
                     ++ (if(ELEN >= 128) Array((io.vediv === FourDiv) -> VecInit(v0muleCut.e128.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                     ++ (if(ELEN >= 256) Array((io.vediv === EightDiv) -> VecInit(v0muleCut.e256.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 64)
      v0muleDiv.e64 := MuxCase(v0muleCut.e64, Array()
                       ++ (if(ELEN >= 128) Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e128.map(i => VecInit(i,i)).flatten)) else Nil)
                       ++ (if(ELEN >= 256) Array((io.vediv === FourDiv) -> VecInit(v0muleCut.e256.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                       ++ (if(ELEN >= 512) Array((io.vediv === EightDiv) -> VecInit(v0muleCut.e512.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 128)
      v0muleDiv.e128 := MuxCase(v0muleCut.e128, Array()
                        ++ (if(ELEN >= 256)  Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e256.map(i => VecInit(i,i)).flatten)) else Nil)
                        ++ (if(ELEN >= 512)  Array((io.vediv === FourDiv) -> VecInit(v0muleCut.e512.map(i => VecInit(i,i,i,i)).flatten)) else Nil)
                        ++ (if(ELEN >= 1024) Array((io.vediv === EightDiv) -> VecInit(v0muleCut.e1024.map(i => VecInit(i,i,i,i,i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 256)
      v0muleDiv.e256 := MuxCase(v0muleCut.e256, Array()
                        ++ (if(ELEN >= 512)  Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e512.map(i => VecInit(i,i)).flatten)) else Nil)
                        ++ (if(ELEN >= 1024) Array((io.vediv === FourDiv) -> VecInit(v0muleCut.e1024.map(i => VecInit(i,i,i,i)).flatten)) else Nil))
    if(ELEN >= 512)
      v0muleDiv.e512 := MuxCase(v0muleCut.e512, Array()
                        ++ (if(ELEN >= 1024) Array((io.vediv === TwoDiv) -> VecInit(v0muleCut.e1024.map(i => VecInit(i,i)).flatten)) else Nil))
    if(ELEN == 1024)
      v0muleDiv.e1024 := v0muleCut.e1024
  }

  io.v0maske := v0maskDiv


  if(FSEW16)         io.v0fen.f16  := io.v0maske.e16
                     io.v0fen.f32  := io.v0maske.e32
  if(FSEWMAX >= 64)  io.v0fen.f64  := io.v0maske.e64
  if(FSEWMAX >= 128) io.v0fen.f128 := io.v0maske.e128

  io.v0mul.e8    := Mux(vsew === ByteWidth,  v0muleDiv.e8,  VecInit(Seq.fill(E8Depth){ 0.U(1.W) }))
  io.v0mul.e16   := Mux(vsew === HWordWidth, v0muleDiv.e16, VecInit(Seq.fill(E16Depth){ 0.U(1.W) }))
  io.v0mul.e32   := Mux(vsew === WordWidth,  v0muleDiv.e32, VecInit(Seq.fill(E32Depth){ 0.U(1.W) }))
  if(ELEN >= 64)   io.v0mul.e64   := Mux(vsew === DWordWidth, v0muleDiv.e64,   VecInit(Seq.fill(E64Depth){ 0.U(1.W) }))
  if(ELEN >= 128)  io.v0mul.e128  := Mux(vsew === QWordWidth, v0muleDiv.e128,  VecInit(Seq.fill(E128Depth){ 0.U(1.W) }))
  if(ELEN >= 256)  io.v0mul.e256  := Mux(vsew === OWordWidth, v0muleDiv.e256,  VecInit(Seq.fill(E256Depth){ 0.U(1.W) }))
  if(ELEN >= 512)  io.v0mul.e512  := Mux(vsew === SWordWidth, v0muleDiv.e512,  VecInit(Seq.fill(E512Depth){ 0.U(1.W) }))
  if(ELEN == 1024) io.v0mul.e1024 := Mux(vsew === TWordWidth, v0muleDiv.e1024, VecInit(Seq.fill(E1024Depth){ 0.U(1.W) }))

  io.carryIn.e8 := MuxCase(VecInit(Seq.fill(E8Depth){ 0.U(1.W) }),
                     Array(isCarryOne    -> VecInit(Seq.fill(E8Depth){ 1.U(1.W) }),
                           isCarryPlusS  -> VecInit(io.v0m.m8         ++ Seq.fill(E8Depth-m8Depth){ 0.U(1.W) }),
                           isCarryMinusS -> VecInit(io.v0m.m8.map(~_) ++ Seq.fill(E8Depth-m8Depth){ 1.U(1.W) }),
                           isCarryPlusD  -> VecInit(io.v0m.m4         ++ Seq.fill(E8Depth-m4Depth){ 0.U(1.W) }),
                           isCarryMinusD -> VecInit(io.v0m.m4.map(~_) ++ Seq.fill(E8Depth-m4Depth){ 1.U(1.W) }),
                           isCarryPlusQ  -> VecInit(io.v0m.m2         ++ Seq.fill(E8Depth-m2Depth){ 0.U(1.W) }),
                           isCarryMinusQ -> VecInit(io.v0m.m2.map(~_) ++ Seq.fill(E8Depth-m2Depth){ 1.U(1.W) }),
                           isCarryPlusO  -> io.v0m.m1,
                           isCarryMinusO -> VecInit(io.v0m.m1.map(~_))))
  io.carryIn.e16 := MuxCase(VecInit(Seq.fill(E16Depth){ 0.U(1.W) }),
                      Array(isCarryOne    -> VecInit(Seq.fill(E16Depth){ 1.U(1.W) }),
                            isCarryPlusS  -> VecInit(io.v0m.m16         ++ Seq.fill(E16Depth-m16Depth){ 0.U(1.W) }),
                            isCarryMinusS -> VecInit(io.v0m.m16.map(~_) ++ Seq.fill(E16Depth-m16Depth){ 1.U(1.W) }),
                            isCarryPlusD  -> VecInit(io.v0m.m8          ++ Seq.fill(E16Depth-m8Depth){ 0.U(1.W) }),
                            isCarryMinusD -> VecInit(io.v0m.m8.map(~_)  ++ Seq.fill(E16Depth-m8Depth){ 1.U(1.W) }),
                            isCarryPlusQ  -> VecInit(io.v0m.m4          ++ Seq.fill(E16Depth-m4Depth){ 0.U(1.W) }),
                            isCarryMinusQ -> VecInit(io.v0m.m4.map(~_)  ++ Seq.fill(E16Depth-m4Depth){ 1.U(1.W) }),
                            isCarryPlusO  -> io.v0m.m2,
                            isCarryMinusO -> VecInit(io.v0m.m2.map(~_))))
  io.carryIn.e32 := MuxCase(VecInit(Seq.fill(E32Depth){ 0.U(1.W) }),
                      Array(isCarryOne    -> VecInit(Seq.fill(E32Depth){ 1.U(1.W) }),
                            isCarryPlusS  -> VecInit(io.v0m.m32         ++ Seq.fill(E32Depth-m32Depth){ 0.U(1.W) }),
                            isCarryMinusS -> VecInit(io.v0m.m32.map(~_) ++ Seq.fill(E32Depth-m32Depth){ 1.U(1.W) }),
                            isCarryPlusD  -> VecInit(io.v0m.m16         ++ Seq.fill(E32Depth-m16Depth){ 0.U(1.W) }),
                            isCarryMinusD -> VecInit(io.v0m.m16.map(~_) ++ Seq.fill(E32Depth-m16Depth){ 1.U(1.W) }),
                            isCarryPlusQ  -> VecInit(io.v0m.m8          ++ Seq.fill(E32Depth-m8Depth){ 0.U(1.W) }),
                            isCarryMinusQ -> VecInit(io.v0m.m8.map(~_)  ++ Seq.fill(E32Depth-m8Depth){ 1.U(1.W) }),
                            isCarryPlusO  -> io.v0m.m4,
                            isCarryMinusO -> VecInit(io.v0m.m4.map(~_))))
  if(ELEN >= 64)
    io.carryIn.e64 := MuxCase(VecInit(Seq.fill(E64Depth){ 0.U(1.W) }),
                        Array(isCarryOne    -> VecInit(Seq.fill(E64Depth){ 1.U(1.W) }),
                              isCarryPlusS  -> VecInit(io.v0m.m64         ++ Seq.fill(E64Depth-m64Depth){ 0.U(1.W) }),
                              isCarryMinusS -> VecInit(io.v0m.m64.map(~_) ++ Seq.fill(E64Depth-m64Depth){ 1.U(1.W) }),
                              isCarryPlusD  -> VecInit(io.v0m.m32         ++ Seq.fill(E64Depth-m32Depth){ 0.U(1.W) }),
                              isCarryMinusD -> VecInit(io.v0m.m32.map(~_) ++ Seq.fill(E64Depth-m32Depth){ 1.U(1.W) }),
                              isCarryPlusQ  -> VecInit(io.v0m.m16         ++ Seq.fill(E64Depth-m16Depth){ 0.U(1.W) }),
                              isCarryMinusQ -> VecInit(io.v0m.m16.map(~_) ++ Seq.fill(E64Depth-m16Depth){ 1.U(1.W) }),
                              isCarryPlusO  -> io.v0m.m8,
                              isCarryMinusO -> VecInit(io.v0m.m8.map(~_))))
  if(ELEN >= 128)
    io.carryIn.e128 := MuxCase(VecInit(Seq.fill(E128Depth){ 0.U(1.W) }),
                         Array(isCarryOne    -> VecInit(Seq.fill(E128Depth){ 1.U(1.W) }),
                               isCarryPlusS  -> VecInit(io.v0m.m128         ++ Seq.fill(E128Depth-m128Depth){ 0.U(1.W) }),
                               isCarryMinusS -> VecInit(io.v0m.m128.map(~_) ++ Seq.fill(E128Depth-m128Depth){ 1.U(1.W) }),
                               isCarryPlusD  -> VecInit(io.v0m.m64          ++ Seq.fill(E128Depth-m64Depth){ 0.U(1.W) }),
                               isCarryMinusD -> VecInit(io.v0m.m64.map(~_)  ++ Seq.fill(E128Depth-m64Depth){ 1.U(1.W) }),
                               isCarryPlusQ  -> VecInit(io.v0m.m32          ++ Seq.fill(E128Depth-m32Depth){ 0.U(1.W) }),
                               isCarryMinusQ -> VecInit(io.v0m.m32.map(~_)  ++ Seq.fill(E128Depth-m32Depth){ 1.U(1.W) }),
                               isCarryPlusO  -> io.v0m.m16,
                               isCarryMinusO -> VecInit(io.v0m.m16.map(~_))))
  if(ELEN >= 256)
    io.carryIn.e256 := MuxCase(VecInit(Seq.fill(E256Depth){ 0.U(1.W) }),
                         Array(isCarryOne    -> VecInit(Seq.fill(E256Depth){ 1.U(1.W) }),
                               isCarryPlusS  -> VecInit(io.v0m.m256         ++ Seq.fill(E256Depth-m256Depth){ 0.U(1.W) }),
                               isCarryMinusS -> VecInit(io.v0m.m256.map(~_) ++ Seq.fill(E256Depth-m256Depth){ 1.U(1.W) }),
                               isCarryPlusD  -> VecInit(io.v0m.m128         ++ Seq.fill(E256Depth-m128Depth){ 0.U(1.W) }),
                               isCarryMinusD -> VecInit(io.v0m.m128.map(~_) ++ Seq.fill(E256Depth-m128Depth){ 1.U(1.W) }),
                               isCarryPlusQ  -> VecInit(io.v0m.m64          ++ Seq.fill(E256Depth-m64Depth){ 0.U(1.W) }),
                               isCarryMinusQ -> VecInit(io.v0m.m64.map(~_)  ++ Seq.fill(E256Depth-m64Depth){ 1.U(1.W) }),
                               isCarryPlusO  -> io.v0m.m32,
                               isCarryMinusO -> VecInit(io.v0m.m32.map(~_))))
  if(ELEN >= 512)
    io.carryIn.e512 := MuxCase(VecInit(Seq.fill(E512Depth){ 0.U(1.W) }),
                         Array(isCarryOne    -> VecInit(Seq.fill(E512Depth){ 1.U(1.W) }),
                               isCarryPlusS  -> VecInit(io.v0m.m512         ++ Seq.fill(E512Depth-m512Depth){ 0.U(1.W) }),
                               isCarryMinusS -> VecInit(io.v0m.m512.map(~_) ++ Seq.fill(E512Depth-m512Depth){ 1.U(1.W) }),
                               isCarryPlusD  -> VecInit(io.v0m.m256         ++ Seq.fill(E512Depth-m256Depth){ 0.U(1.W) }),
                               isCarryMinusD -> VecInit(io.v0m.m256.map(~_) ++ Seq.fill(E512Depth-m256Depth){ 1.U(1.W) }),
                               isCarryPlusQ  -> VecInit(io.v0m.m128         ++ Seq.fill(E512Depth-m128Depth){ 0.U(1.W) }),
                               isCarryMinusQ -> VecInit(io.v0m.m128.map(~_) ++ Seq.fill(E512Depth-m128Depth){ 1.U(1.W) }),
                               isCarryPlusO  -> io.v0m.m64,
                              isCarryMinusO -> VecInit(io.v0m.m64.map(~_))))
  if(ELEN == 1024)
    io.carryIn.e1024 := MuxCase(VecInit(Seq.fill(E1024Depth){ 0.U(1.W) }),
                          Array(isCarryOne    -> VecInit(Seq.fill(e1024Depth){ 1.U(1.W) }),
                                isCarryPlusS  -> VecInit(io.v0m.m1024         ++ Seq.fill(E1024Depth-m1024Depth){ 0.U(1.W) }),
                                isCarryMinusS -> VecInit(io.v0m.m1024.map(~_) ++ Seq.fill(E1024Depth-m1024Depth){ 1.U(1.W) }),
                                isCarryPlusD  -> VecInit(io.v0m.m512          ++ Seq.fill(E1024Depth-m512Depth){ 0.U(1.W) }),
                                isCarryMinusD -> VecInit(io.v0m.m512.map(~_)  ++ Seq.fill(E1024Depth-m512Depth){ 1.U(1.W) }),
                                isCarryPlusQ  -> VecInit(io.v0m.m256          ++ Seq.fill(E1024Depth-m256Depth){ 0.U(1.W) }),
                                isCarryMinusQ -> VecInit(io.v0m.m256.map(~_)  ++ Seq.fill(E1024Depth-m256Depth){ 1.U(1.W) }),
                                isCarryPlusO  -> io.v0m.m128,
                                isCarryMinusO -> VecInit(io.v0m.m128.map(~_))))

}
