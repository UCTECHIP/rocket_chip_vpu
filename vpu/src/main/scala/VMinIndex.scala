// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VMinIndex.scala
*       Author          :       sujy
*       Revision        :       2019/05/09
*       Company         :       UC TECH IP
*       Department      :       STG
*       Description     :       module for execute following
*                               instructions:
*                               vfirst, vmsbf, vmsif, vmsof
*
*       io.vsew         :       input[VSEW_SZ-1:0], control, to form a select signal
*       io.vlmul        :       input[VLMUL_SZ-1:0], control, to form a select signal
*       io.vm           :       input, control, enable mask
*       io.minIdxFun    :       input[MinIdxFun_SZ-1:0], control, showing function: sbf or sif or sof
*       io.vs2m         :       input, bundle of vectors, data, elements all 1 bit, operand
*       io.v0m          :       input, bundle of vectors, control, to mask operand
*       io.vMinIndex    :       output[XLEN-1:0], data, index of the first set element 
*       io.vMSetOut     :       output, bundle of vectors, data, elements all 1 bit, results
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VMinIndex(params: VPUParams) extends VModule(params) {
  val io = IO(new Bundle{
    val vsew      = Input(UInt(VSEW_SZ.W))
    val vlmul     = Input(UInt(VLMUL_SZ.W))
    val vm        = Input(UInt(1.W))

    val minIdxFun = Input(UInt(MinIdxFun_SZ.W))
    val vs2m      = Input(new MLENVec)
    val v0m       = Input(new MLENVec)

    val vMinIndex = Output(SInt(XLEN.W))
    val vMSetOut  = Output(new MLENVec)
  })
  
  val MLEN = Wire(UInt(11.W))
  MLEN := 1.U(11.W) << (io.vsew + 3.U(11.W) - io.vlmul)   //must declare width of shift amount

  val index = VecInit(Range(0, VLEN, 1).map(_.S))

  def execute(depth:Int, index:Vec[SInt], src:Vec[UInt], mask:Vec[UInt], vm:UInt, sel:UInt) = {
    val srcm        = Wire(Vec(depth, UInt(1.W)))
    val indexOut    = Wire(Vec(depth, SInt(XLEN.W)))
    val indexFlag   = Wire(Vec(depth, UInt(1.W)))
    val vMSetOut    = Wire(Vec(depth, UInt(1.W)))
    val firstSetBit = Wire(SInt(XLEN.W))                  //index of the first set mask bit

    for(i <- 0 until depth){
      srcm(i) := src(i) & (mask(i) | vm)

      if(i == depth-1)  
        indexOut(i) := Mux(srcm(i) === 1.U, index(i), -1.S)
      else
        indexOut(i) := Mux(srcm(i) === 1.U, index(i), indexOut(i+1))

      if(i == 0)
        indexFlag(i) := srcm(i)
      else
        indexFlag(i) := srcm(i) | indexFlag(i-1)

      if(i == 0)
        vMSetOut(i) := MuxCase(0.U,
          Array((sel === 0.U) -> 0.U,
                (sel === 1.U) -> Mux(srcm(0) === 1.U, 0.U, 1.U),    //set-before-first mask bit
                (sel === 2.U) -> Mux(srcm(0) === 1.U, 1.U, 0.U),    //set-only-first mask bit
                (sel === 3.U) -> 1.U                                //set-including-first mask bit
                 ))
      else
        vMSetOut(i) := MuxCase(0.U,
          Array((sel === 0.U) -> 0.U,
                (sel === 1.U) -> (Mux(indexFlag(i) === 1.U, 0.U, 1.U)),
                (sel === 2.U) -> (Mux(indexFlag(i) === 1.U && indexFlag(i-1) === 0.U, 1.U, 0.U)),
                (sel === 3.U) -> (Mux(indexFlag(i) === 0.U || (indexFlag(i) === 1.U && indexFlag(i-1) === 0.U), 1.U, 0.U))
                ))
    }

    if(depth == 0)
      firstSetBit := -1.S
    else
      firstSetBit := indexOut(0)  

    (firstSetBit, vMSetOut)
  }  

  val (minIndex1   , vMSetOut1   ) = execute(m1Depth   , index, io.vs2m.m1   , io.v0m.m1   , io.vm, io.minIdxFun)
  val (minIndex2   , vMSetOut2   ) = execute(m2Depth   , index, io.vs2m.m2   , io.v0m.m2   , io.vm, io.minIdxFun)
  val (minIndex4   , vMSetOut4   ) = execute(m4Depth   , index, io.vs2m.m4   , io.v0m.m4   , io.vm, io.minIdxFun)
  val (minIndex8   , vMSetOut8   ) = execute(m8Depth   , index, io.vs2m.m8   , io.v0m.m8   , io.vm, io.minIdxFun)
  val (minIndex16  , vMSetOut16  ) = execute(m16Depth  , index, io.vs2m.m16  , io.v0m.m16  , io.vm, io.minIdxFun)
  val (minIndex32  , vMSetOut32  ) = execute(m32Depth  , index, io.vs2m.m32  , io.v0m.m32  , io.vm, io.minIdxFun) 
  val (minIndex64  , vMSetOut64  ) = execute(m64Depth  , index, io.vs2m.m64  , io.v0m.m64  , io.vm, io.minIdxFun)
  val (minIndex128 , vMSetOut128 ) = execute(m128Depth , index, io.vs2m.m128 , io.v0m.m128 , io.vm, io.minIdxFun)
  val (minIndex256 , vMSetOut256 ) = execute(m256Depth , index, io.vs2m.m256 , io.v0m.m256 , io.vm, io.minIdxFun)
  val (minIndex512 , vMSetOut512 ) = execute(m512Depth , index, io.vs2m.m512 , io.v0m.m512 , io.vm, io.minIdxFun)
  val (minIndex1024, vMSetOut1024) = execute(m1024Depth, index, io.vs2m.m1024, io.v0m.m1024, io.vm, io.minIdxFun)

  io.vMSetOut.m1  := vMSetOut1
  io.vMSetOut.m2  := vMSetOut2
  io.vMSetOut.m4  := vMSetOut4
  io.vMSetOut.m8  := vMSetOut8
  io.vMSetOut.m16 := vMSetOut16
  io.vMSetOut.m32 := vMSetOut32
  
  if(ELEN >= 64   ) io.vMSetOut.m64   := vMSetOut64
  if(ELEN >= 128  ) io.vMSetOut.m128  := vMSetOut128
  if(ELEN >= 256  ) io.vMSetOut.m256  := vMSetOut256
  if(ELEN >= 512  ) io.vMSetOut.m512  := vMSetOut512
  if(ELEN >= 1024 ) io.vMSetOut.m1024 := vMSetOut1024

  io.vMinIndex := MuxCase(-1.S,
    Array((MLEN === 1.U)    -> minIndex1,
          (MLEN === 2.U)    -> minIndex2,
          (MLEN === 4.U)    -> minIndex4,
          (MLEN === 8.U)    -> minIndex8,
          (MLEN === 16.U)   -> minIndex16,
          (MLEN === 32.U)   -> minIndex32,
          (MLEN === 64.U)   -> minIndex64,
          (MLEN === 128.U)  -> minIndex128,
          (MLEN === 256.U)  -> minIndex256,
          (MLEN === 512.U)  -> minIndex512,
          (MLEN === 1024.U) -> minIndex1024
          ))

}
