// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VDmemResp.scala
*       Author          :       sujy
*       Revision        :       2019/06/18
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       VPU DCache response handling module
                                (using finite machine)
*
*       io.en           :       input, ...
*       io.din          :       input[width-1:0], ...
*       io.dout         :       output[width-1:0], ...
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VDmemResp(params: VPUParams, tagBits: Int, addrBits: Int) extends VModule(params) {
  val dataBits  = XLEN max FLEN
  val dataBytes = dataBits/8
  val sizeBits  = log2Ceil(log2Ceil(dataBytes)+1)

  val io = IO(new Bundle{
    val wd     = Input(Bool())
    val lumop  = Input(UInt(5.W))
    //input, CSR ctrl signals
    val vsew   = Input(UInt(VSEW_SZ.W))
    val vlmul  = Input(UInt(VLMUL_SZ.W))
    val vl     = Input(UInt(XLEN.W))    //decimal
    //input, other ctrl signals
    val vm      = Input(UInt(1.W))
    val sign    = Input(Bool())
    val majFun  = Input(UInt(MajFun_SZ.W))
    val nFields = Input(UInt(NFIELDS_SZ.W))
    //input, DCache response interface
    val respValid   = Input(Bool())
    val respTag     = Input(UInt(tagBits.W))   //7.W
    val respSize     = Input(UInt(sizeBits.W))
    val respHasData = Input(Bool())
    val respData    = Input(UInt(dataBits.W))      // XLEN.W
    //input, DCache status signals
    val mEnable = Input(Bool())    //DCache enable flag
    val mDone   = Input(Bool())    //DCache data transfer complete flag
    val faultFirst = Input(Bool())
    val stopLoad   = Input(Bool())
    val killm      = Input(Bool())
    //input, result of last inst
    val vdvs3e = Input(new FullSEWVec)
//    val v0m    = Input(new MLENVec)
    val v0e    = Input(new FullSEW1wVec)
    
    //output
    val vRecvOver = Output(Bool())
    val vRecvOut  = Output(UInt((VLEN*8).W))
  })


  //construct a FIFO buffer
  class bufferIO extends Bundle{
    val wen   = Bool()
    val waddr = UInt(log2Ceil(VLEN).W)
    val wdata = UInt(XLEN.W)
    val raddr = UInt(log2Ceil(VLEN).W)
    val rdata = UInt(XLEN.W)
  }
  
  val elemNumWidth = log2Ceil(VLEN) + 1
 
  val vm    = Wire(UInt(1.W))
  val vsew  = Wire(UInt(VSEW_SZ.W))
  val vlmul = Wire(UInt(VLMUL_SZ.W))
  val vl    = Wire(UInt(XLEN.W)) 

  vm    := Mux(io.lumop === UStrideWhole, 1.U, io.vm)
  vsew  := Mux(io.lumop === UStrideWhole, 0.U, io.vsew)
  vlmul := Mux(io.lumop === UStrideWhole, 0.U, io.vlmul)
  vl    := Mux(io.lumop === UStrideWhole, (VLEN/8).U, io.vl)

  val buf   = Wire(new bufferIO)
  val dpram = Mem(VLEN, UInt(XLEN.W))
  val rcnt  = RegInit(0.U((elemNumWidth-1).W))    

  val isLoad   = Wire(Bool())
  val mEnable  = Wire(Bool())
  val mDone    = Wire(Bool())

  val extdData = Wire(UInt(XLEN.W))
  
//  val v0e = Wire(new FullSEW1wVec)   // [TODO] can be simplified by using vwidth.io.v0e

  val recvData8  = Wire(UInt(8.W))
  val recvData16 = Wire(UInt(16.W))
  val recvData32 = Wire(UInt(32.W))
  val recvData64 = Wire(UInt(64.W))
  val recvOver   = Wire(Bool())
  val recvOut    = RegInit(0.U((VLEN*8).W))  //output register

  
  //--define a finite state machine-
  val s_idle :: s_write :: s_read :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)
 
  //--for-SEGLS--------
  val readDone  = Wire(Bool())
  val elemNum   = RegInit(0.U(elemNumWidth.W))
  val maskIndex = Wire(UInt(log2Ceil(VLEN).W))


  //--segment-load-configuration----
  if(SEGLS){
    val vlmax     = Wire(UInt((elemNumWidth).W))
    val eleOffset = Wire(UInt((log2Ceil(VLEN)+1).W))
    val elemCnt   = RegInit(0.U((elemNumWidth-1).W))
    val maskCnt   = RegInit(0.U((elemNumWidth-1).W))  
    val remainReg = RegInit(0.U(3.W))
    val isValid   = Wire(Bool())  // see whether data from buffer should be written to receive reg or not

    vlmax     := 1.U << (log2Ceil(VLEN).U + vlmul - vsew - 3.U)
    eleOffset := VLEN.U >> (vsew + 3.U - vlmul)
    readDone  := remainReg === 0.U && elemCnt === 0.U
    isValid   := rcnt - (eleOffset * remainReg) < vl

    val nFieldsReal  = Wire(UInt(NFIELDS_SZ.W))
    nFieldsReal := Mux(io.majFun === IsAMO, 0.U, io.nFields)
//    elemNum := (vl * (nFieldsReal +& 1.U))(elemNumWidth-1, 0)   //origin
    elemNum := (vlmax * (nFieldsReal +& 1.U))(elemNumWidth-1, 0)   

    maskIndex := Mux(nFieldsReal === 0.U, buf.raddr, maskCnt)

    when(state === s_write){
      elemCnt   := vlmax - 1.U
      maskCnt   := vl - 1.U
      remainReg := nFieldsReal
    }
    when(state === s_read ){
      elemCnt   := Mux(elemCnt > 0.U, elemCnt - 1.U, vlmax - 1.U)
      maskCnt   := Mux(elemCnt >= vl, maskCnt, Mux((maskCnt > 0.U), maskCnt - 1.U, vl - 1.U))
      remainReg := Mux(elemCnt === 0.U, remainReg - 1.U, remainReg)
    }
    
    recvData8  := Mux(isValid, Mux(io.v0e.e8(maskIndex).toBool , buf.rdata(7,0) , io.vdvs3e.e8(buf.raddr) ), 0.U)
    recvData16 := Mux(isValid, Mux(io.v0e.e16(maskIndex).toBool, buf.rdata(15,0), io.vdvs3e.e16(buf.raddr)), 0.U)
    recvData32 := Mux(isValid, Mux(io.v0e.e32(maskIndex).toBool, buf.rdata(31,0), io.vdvs3e.e32(buf.raddr)), 0.U)
    if(XLEN == 64 && ELEN >= 64)
      recvData64 := Mux(isValid, Mux(io.v0e.e64(maskIndex).toBool, buf.rdata(63,0), io.vdvs3e.e64(buf.raddr)), 0.U)
    else
      recvData64 := 0.U
  }
  else{
    readDone := rcnt === 0.U
    elemNum := vl(elemNumWidth-1, 0)
    maskIndex := buf.raddr

    recvData8  := Mux(io.v0e.e8(maskIndex).toBool , buf.rdata(7,0) , io.vdvs3e.e8(buf.raddr) )
    recvData16 := Mux(io.v0e.e16(maskIndex).toBool, buf.rdata(15,0), io.vdvs3e.e16(buf.raddr))
    recvData32 := Mux(io.v0e.e32(maskIndex).toBool, buf.rdata(31,0), io.vdvs3e.e32(buf.raddr))
    if(XLEN == 64 && ELEN >= 64)
      recvData64 := Mux(io.v0e.e64(maskIndex).toBool, buf.rdata(63,0), io.vdvs3e.e64(buf.raddr))
    else
      recvData64 := 0.U
  }

  //--extend received data----------
  extdData := MuxCase(0.U, Array() 
    ++ Array((io.respSize === 0.U) -> Mux(io.sign,
                                         Cat(Fill(XLEN-8, io.respData(7)), io.respData(7,0)),
                                         Cat(0.U((XLEN-8).W)             , io.respData(7,0))),
             (io.respSize === 1.U) -> Mux(io.sign,
                                         Cat(Fill(XLEN-16, io.respData(15)), io.respData(15,0)),
                                         Cat(0.U((XLEN-16).W)              , io.respData(15,0))))
    ++ (if(XLEN == 32)
          Array((io.respSize === 2.U) -> io.respData(31,0))
        else if(XLEN == 64 && ELEN >= 64) 
          Array((io.respSize === 2.U) -> Mux(io.sign, 
                                            Cat(Fill(XLEN-32, io.respData(31)), io.respData(31,0)), 
                                            Cat(0.U((XLEN-32).W)              , io.respData(31,0))),
                (io.respSize === 3.U) -> io.respData(63,0))
        else Nil))



  //--buffer initialization---------
  buf.wen   := isLoad && io.respValid && io.respHasData && vl =/= 0.U && !io.stopLoad && !io.faultFirst
  buf.waddr := io.respTag
  buf.wdata := extdData
  buf.raddr := rcnt
  buf.rdata := 0.U
  recvOver  := false.B

  isLoad  := io.majFun === IsLoad || (io.majFun === IsAMO && io.wd)
  mEnable := isLoad && io.mEnable   //valid only if current inst is 'load'
  mDone   := isLoad && io.mDone     //valid only if current inst is 'load'


  //--buffer controller-------------
  switch(state){
    is(s_idle){ 
      when(io.killm || io.faultFirst)
        { state := s_idle }
      .elsewhen(io.majFun === IsAMO && !io.wd)
        { state := s_idle } 
      .elsewhen(vl =/= 0.U && mEnable || (io.majFun === IsAMO && io.wd))
        { state := s_write } 
    }
    is(s_write){ 
      when(io.killm)
        { state := s_idle }
      .elsewhen(mDone || io.stopLoad) 
        { state := s_read } 
    }
    is(s_read){ 
      when(io.killm)
        { state := s_idle }
      .elsewhen(readDone) 
        { state := s_done } 
    }
    is(s_done) { state := s_idle }
  }

  when(state === s_idle){
    recvOut := 0.U
  }
  when(state === s_write) { 
    rcnt := elemNum - 1.U          
    when(buf.wen) { dpram(buf.waddr) := buf.wdata } 
  }
  when(state === s_read) { 
    rcnt      := Mux(rcnt === 0.U, elemNum - 1.U, rcnt - 1.U)
    buf.rdata := dpram(buf.raddr)
    
    recvOut   := MuxCase(0.U, Array()
      ++ Array((vsew === 0.U) -> (recvOut << 8  | recvData8 ),
               (vsew === 1.U) -> (recvOut << 16 | recvData16),
               (vsew === 2.U) -> (recvOut << 32 | recvData32))
      ++ (if(XLEN == 64 && ELEN >= 64)
         Array((vsew === 3.U) -> (recvOut << 64 | recvData64))
         else Nil))    
  }
  when(state === s_done){
    recvOver := true.B
    recvOut  := 0.U
  }


  //--output------------------------
  io.vRecvOver := recvOver
  io.vRecvOut  := Mux(recvOver, recvOut, 0.U)
  
}
