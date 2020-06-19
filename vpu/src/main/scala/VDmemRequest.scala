// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VDmemRequest.scala
*       Author          :       yexc
*       Revision        :       2019/05/08
*       Company         :       UC TECH IP
*       Department      :       
*       Description     :       
*
*       io.en           :       input, ...
*       io.din          :       input[width-1:0], ...
*       io.dout         :       output[width-1:0], ...
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._
import vpu.DataGating._

class VDmemRequest(params: VPUParams, tagBits: Int, addrBits: Int) extends VModule(params) {
    val vlenLog2  = log2Ceil(VLEN)
    val dataBits  = XLEN max FLEN
    val dataBytes = dataBits/8
    val sizeBits  = log2Ceil(log2Ceil(dataBytes)+1)

  val io = IO(new Bundle {
    val fromXData1 = Input(UInt(XLEN.W))
    val fromXData2 = Input(UInt(XLEN.W))
    val vs2toXlen  = Input(Vec(VLEN, UInt(XLEN.W)))
    val vdvs3e8    = Input(Vec(E8Depth,  UInt(8.W)))
    val vdvs3e16   = Input(Vec(E16Depth, UInt(16.W)))
    val vdvs3e32   = Input(Vec(E32Depth, UInt(32.W)))
    val vdvs3e64   = Input(Vec(E64Depth, UInt(64.W)))

    val v0m1  = Input(Vec(m1Depth,     UInt(1.W)))
    val v0m2  = Input(Vec(m2Depth,     UInt(1.W)))
    val v0m4  = Input(Vec(m4Depth,     UInt(1.W)))
    val v0m8  = Input(Vec(m8Depth,     UInt(1.W)))
    val v0m16 = Input(Vec(m16Depth,    UInt(1.W)))
    val v0m32 = Input(Vec(m32Depth,    UInt(1.W)))
    val v0m64 = Input(Vec(m64Depth,    UInt(1.W)))
    val vm    = Input(UInt(1.W))
    val vsew     = Input(UInt(VSEW_SZ.W))
    val vlmul    = Input(UInt(VLMUL_SZ.W))
    val vl       = Input(UInt(XLEN.W))
    val majFun   = Input(UInt(MajFun_SZ.W))
    val addrMode = Input(UInt(AddrMode_SZ.W))
    val ldstWidth = Input(UInt(LdStWidth_SZ.W))
    val nFields = Input(UInt(NFIELDS_SZ.W))
    val lumop = Input(UInt(5.W))
    val amoop = Input(UInt(5.W))

    val reqReady = Input(Bool()) 
    val reqValid = Output(Bool())
    val reqAddr  = Output(UInt(addrBits.W))
    val reqTag   = Output(UInt(tagBits.W))
    val reqCmd   = Output(UInt(5.W))
    val reqSize   = Output(UInt(sizeBits.W))
    val s1Kill   = Output(Bool())
    val s1Data   = Output(UInt(dataBits.W))
    val s2Nack   = Input(UInt(1.W))
    val done     = Output(Bool())

    val enable      = Input(Bool())
    val respValid   = Input(Bool())
    val respTag     = Input(UInt(tagBits.W))
   
    val eret       = Input(Bool())
    val respS2Xcpt  = Input(new HellaCacheExceptions)
    val mem_cause  = Output(UInt(XLEN.W))
    val mem_xcpt   = Output(Bool())
    val mem_xcpt_resp = Input(Bool())  
    val mem_mtval  = Output(UInt(XLEN.W))
    val mem_vl     = Output(UInt(XLEN.W))
    val mem_vl_valid = Output(Bool())
    val stopLoad   = Output(Bool())  

    val mem_vstart = Output(UInt(XLEN.W))
    val faultFirst = Output(Bool())
    val replay       = Output(Bool())

    val killx     = Input(Bool())
    val killm      = Input(Bool())
    val killw     = Input(Bool())
    val wd     = Input(Bool())
  })  

  val isNormalUnitStride = (io.addrMode === UStride) && (io.lumop === "b00000".U)
  val isWholeRegisters = (io.addrMode === UStride) && (io.lumop === "b01000".U)
  val isFaultOnlyFirst = (io.addrMode === UStride) && (io.lumop === "b10000".U)

  val vmReal      = Mux(isWholeRegisters, 1.U, io.vm)
  val vsewReal    = Mux(isWholeRegisters, 0.U, io.vsew)
  val vlmulReal   = Mux(isWholeRegisters, 0.U, io.vlmul)
  val vlReal      = Mux(isWholeRegisters, (VLEN/8).U, io.vl)
  val nFieldsReal = Mux(io.majFun === IsAMO, 0.U, io.nFields)

  val mlen         = Wire(UInt((vlenLog2+1).W))
  mlen := 1.U((vlenLog2+1).W) << (vsewReal + 3.U((vlenLog2+1).W) - vlmulReal)

  val vdmemreqimp = Module(new VDmemRequestImp(params))

  def counter(max: UInt) = {
     val x = RegInit(0.U(max.getWidth))
     x := Mux(x === max || io.done, 0.U, Mux(vdmemreqimp.io.reqIn.fire(), x + 1.U, x))
     x
  }
  val enableFlagPre = Wire(Bool())
  val enableFlagReg = RegInit(false.B)
  enableFlagReg := enableFlagPre
  enableFlagPre := Mux(io.enable, true.B, Mux(io.done || vdmemreqimp.io.mem_xcpt, false.B, enableFlagReg))

  val eleOffset = Wire(UInt(vlenLog2.W))
  eleOffset := VLEN.U >> (vsewReal + 3.U - vlmulReal)
  val numCnt = counter(65.U)

  val cntPtr     = RegInit(0.U((vlenLog2+1).W))  // vlenLog2+1
  val cntPtrTmp  = Wire(UInt(((vlenLog2+1).W)))  // vlenLog2+1 
  val nFieldsPtr = RegInit(0.U(NFIELDS_SZ.W))
  val curPtr     = Wire(UInt(vlenLog2.W))
  val carryOut = (nFieldsPtr === nFieldsReal) && enableFlagPre && vdmemreqimp.io.reqIn.fire()

  nFieldsPtr := Mux(io.done, 0.U, Mux(vdmemreqimp.io.reqIn.fire(), Mux(nFieldsPtr === nFieldsReal, 0.U, nFieldsPtr + 1.U), nFieldsPtr))
  cntPtrTmp := Mux(vdmemreqimp.io.reqIn.fire(), cntPtr + 1.U, cntPtr)
  cntPtr := Mux(cntPtr === 65.U || io.done, 0.U, Mux(nFieldsReal === 0.U, cntPtrTmp, Mux(carryOut, cntPtr + 1.U, cntPtr)))

  curPtr := cntPtr + nFieldsPtr * eleOffset  

  // which is better???
  //val tmpPtr     = RegInit(0.U((vlenLog2).W))
  //tmpPtr := Mux(io.done, 0.U, Mux(vdmemreqimp.io.reqIn.fire() && nFieldsReal =/= 0.U, tmpPtr + eleOffset, tmpPtr))
  //curPtr := cntPtr + tmpPtr // curPtr drop the most significant bit

   //*****************************************
   //      generate request command
   //*****************************************
   val amoCmd = MuxCase(0.U, Array(
      (io.amoop === AMOswap) -> M_XA_SWAP,
      (io.amoop === AMOadd)  -> M_XA_ADD,
      (io.amoop === AMOxor)  -> M_XA_XOR,
      (io.amoop === AMOand)  -> M_XA_AND,
      (io.amoop === AMOor)   -> M_XA_OR,
      (io.amoop === AMOmin)  -> M_XA_MIN,
      (io.amoop === AMOmax)  -> M_XA_MAX,
      (io.amoop === AMOminu) -> M_XA_MINU,
      (io.amoop === AMOmaxu) -> M_XA_MAXU
   ))

   val reqCmdPre = MuxCase("b11111".U, Array(
           (io.majFun === IsAMO)   -> amoCmd,
           (io.majFun === IsLoad)  -> "b00000".U,
           (io.majFun === IsStore) -> "b00001".U
         ))

   /*****************************************
      generate request type; 
      ldstWidth: 00 -- load/store byte;
                 01 -- load/store half word;
                 10 -- load/store word;
                 11 -- load/store SEW
   ******************************************/
   val reqSizePre = MuxCase(0.U, Array(
          ((io.ldstWidth === LdStByte) -> "b000".U),
          ((io.ldstWidth === LdStHalf) -> "b001".U),
          ((io.ldstWidth === LdStWord) -> "b010".U),
          ((io.ldstWidth === LdStVSEW) -> vsewReal)
        ))

   /*****************************************
      generate request address;
      ldstWidth: 00 -- load/store byte;
                 01 -- load/store half word;
                 10 -- load/store word;
                 11 -- load/store SEW
   ******************************************/
   val stride     = Wire(UInt(XLEN.W))
   stride := MuxCase(0.U, Array(
       ((io.ldstWidth === LdStVSEW) -> (1.U(XLEN.W) << vsewReal)),
       ((io.ldstWidth === LdStWord) -> 4.U(XLEN.W)),
       ((io.ldstWidth === LdStHalf) -> 2.U(XLEN.W)),
       ((io.ldstWidth === LdStByte) -> 1.U(XLEN.W))
     ))

   val step       = Wire(UInt(XLEN.W))
   step := MuxCase(0.U, Array(
        ((io.addrMode === Strided) -> io.fromXData2),
        ((io.addrMode === UStride) -> stride*(nFieldsReal+&1.U))
     ))

   val baseAddr   = Wire(UInt(XLEN.W))
   val offsetAddr = Wire(UInt(XLEN.W))
   val reqAddrPre  = Wire(UInt(addrBits.W))
   baseAddr   := io.fromXData1
   offsetAddr := Mux(io.addrMode === Indexed || io.majFun === IsAMO, io.vs2toXlen(cntPtr), cntPtr * step)
   reqAddrPre := baseAddr + offsetAddr + (nFieldsPtr * stride)
  

    //*****************************************
    //         generate request tag
    //*****************************************
    val reqTagPre = curPtr


    /*****************************************
       generate request data;
       ldstWidth: 00 -- load/store byte;
                  01 -- load/store half word;
                  10 -- load/store word;
                  11 -- load/store SEW
    ******************************************/
    val sewData = MuxCase(0.U, Array(
         (vsewReal === ByteWidth)  -> Cat(Seq.fill(XLEN/8){io.vdvs3e8(curPtr)}),
         (vsewReal === HWordWidth) -> Cat(Seq.fill(XLEN/16){io.vdvs3e16(curPtr)}),
         (vsewReal === WordWidth)  -> Cat(Seq.fill(XLEN/32){io.vdvs3e32(curPtr)})
        )
++ (if(ELEN >= 64) Array((vsewReal === DWordWidth) -> io.vdvs3e64(curPtr)) else Nil)
      )

    val reqDataPre = MuxCase(0.U, Array(
       (io.ldstWidth === LdStByte) -> Cat(Seq.fill(XLEN/8){io.vdvs3e8(curPtr << vsewReal)}),
       (io.ldstWidth === LdStHalf) -> Cat(Seq.fill(XLEN/16){io.vdvs3e16(curPtr << (vsewReal - 1.U))}),
       (io.ldstWidth === LdStWord) -> Cat(Seq.fill(XLEN/32){io.vdvs3e32(curPtr << (vsewReal - 2.U))}),
       (io.ldstWidth === LdStVSEW) -> sewData
     ))


    //*****************************************
    //         generate request valid
    //*****************************************
    val maskElement = MuxCase(0.U, Array(
                           (mlen === 32.U) -> (io.v0m32(cntPtr) | vmReal),
                           (mlen === 16.U) -> (io.v0m16(cntPtr) | vmReal),
                           (mlen === 8.U)  -> (io.v0m8(cntPtr)  | vmReal),
                           (mlen === 4.U)  -> (io.v0m4(cntPtr)  | vmReal),
                           (mlen === 2.U)  -> (io.v0m2(cntPtr)  | vmReal),
                           (mlen === 1.U)  -> (io.v0m1(cntPtr)  | vmReal))
++ (if(ELEN >= 64)   Array((mlen === 64.U) -> (io.v0m64(cntPtr) | vmReal)) else Nil)
      )  


  vdmemreqimp.io.reqReady := io.reqReady
  vdmemreqimp.io.s2Nack := io.s2Nack
  io.reqValid := vdmemreqimp.io.reqValid
  io.reqAddr  := vdmemreqimp.io.reqAddr
  io.reqTag   := vdmemreqimp.io.reqTag

  io.reqCmd   := reqCmdPre
  io.reqSize  := reqSizePre
  io.s1Kill   := false.B
  //io.reqCmd   := vdmemreqimp.io.reqCmd
  //io.reqSize  := vdmemreqimp.io.reqSize
  //io.s1Kill   := vdmemreqimp.io.s1Kill
  io.s1Data   := vdmemreqimp.io.s1Data
  val totalEleNum = (vlReal(vlenLog2,0) * (nFieldsReal +& 1.U))(vlenLog2,0)

  io.done := vdmemreqimp.io.done

  vdmemreqimp.io.reqIn.bits.reqAddrIn := reqAddrPre
  vdmemreqimp.io.reqIn.bits.reqTagIn := reqTagPre
  vdmemreqimp.io.reqIn.bits.reqDataIn := reqDataPre
  vdmemreqimp.io.reqIn.bits.maskIn := maskElement
  vdmemreqimp.io.reqIn.valid := (numCnt < totalEleNum) && enableFlagPre 

  vdmemreqimp.io.reqReady := io.reqReady
  vdmemreqimp.io.vm := io.vm
  vdmemreqimp.io.respValid := io.respValid
  vdmemreqimp.io.respTag := io.respTag
  vdmemreqimp.io.enable := io.enable
  vdmemreqimp.io.isLoad := (io.majFun === IsLoad) || (io.majFun === IsAMO && io.wd)

  val notFirstFFExp = vdmemreqimp.io.mem_xcpt && isFaultOnlyFirst && (vdmemreqimp.io.mem_vstart =/= 0.U)

  vdmemreqimp.io.respS2Xcpt  := io.respS2Xcpt
  vdmemreqimp.io.mem_xcpt_resp  := io.mem_xcpt_resp
  io.mem_cause := vdmemreqimp.io.mem_cause
  io.mem_xcpt := vdmemreqimp.io.mem_xcpt && !notFirstFFExp
  io.mem_mtval := vdmemreqimp.io.mem_mtval
  io.stopLoad := ((io.majFun === IsLoad) || (io.majFun === IsAMO && io.wd)) && vdmemreqimp.io.mem_xcpt

  // TODO: vdmemreqimp.io.mem_vstart not valid in segement instructions
  io.mem_vstart := vdmemreqimp.io.mem_vstart
  io.mem_vl := vdmemreqimp.io.mem_vstart
  io.mem_vl_valid := notFirstFFExp

  // TODO
  io.faultFirst := false.B
  io.replay := false.B
}




class VDmemRequestImp(params: VPUParams) extends VModule(params) {
    val addrBits = if(XLEN==32) 32 else 40
    val tagBits = 7

    val vlenLog2  = log2Ceil(VLEN)
    val dataBits  = XLEN max FLEN
    val dataBytes = dataBits/8
    val sizeBits  = log2Ceil(log2Ceil(dataBytes)+1)

    class dataIn extends Bundle {
      val reqAddrIn = UInt(addrBits.W)
      val reqTagIn  = UInt(tagBits.W)
      val reqDataIn = UInt(dataBits.W)
      val maskIn    = UInt(1.W)
      override def cloneType = new dataIn().asInstanceOf[this.type]
    }

  val io = IO(new Bundle {
    val reqIn = Flipped(Decoupled(new dataIn))

    val reqReady = Input(Bool()) 
    val reqValid = Output(Bool())
    val reqAddr  = Output(UInt(addrBits.W))
    val s1Data   = Output(UInt(dataBits.W))
    val s2Nack   = Input(UInt(1.W))
    val reqTag   = Output(UInt(tagBits.W))
    //val reqCmd   = Output(UInt(5.W))
    //val reqSize   = Output(UInt(sizeBits.W))
    //val s1Kill   = Output(Bool())

    val respS2Xcpt  = Input(new HellaCacheExceptions)
    val mem_cause  = Output(UInt(XLEN.W))
    val mem_xcpt   = Output(Bool())
    val mem_xcpt_resp = Input(Bool())
    val mem_mtval  = Output(UInt(XLEN.W))
    val mem_vstart = Output(UInt(XLEN.W))

    val isLoad = Input(Bool())

    val vm      = Input(UInt(1.W)) 
    val respValid = Input(Bool())
    val respTag   = Input(UInt(tagBits.W))
    val enable    = Input(Bool())
    val done      = Output(Bool())
  })  

  def checkExceptions(x: Seq[(Bool, UInt)]) = 
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  val s_ready :: s0 :: s1 :: s2 :: s_done :: s_xcpt :: Nil = Enum(6)
  val state = RegInit(s_ready)

  val reqInQueue = Module(new Queue(new dataIn, 2))

  reqInQueue.io.enq <> io.reqIn

  io.done  := (reqInQueue.io.count === 0.U) && (state === s_done)
  val (xcpt, cause) = checkExceptions(List(
    (state === s2 && io.respS2Xcpt.ma.st, "h6".U),
    (state === s2 && io.respS2Xcpt.ma.ld, "h4".U),
    (state === s2 && io.respS2Xcpt.pf.st, "hf".U),
    (state === s2 && io.respS2Xcpt.pf.ld, "hd".U),
    (state === s2 && io.respS2Xcpt.ae.st, "h7".U),
    (state === s2 && io.respS2Xcpt.ae.ld, "h5".U)
  ))

  val next = (state === s_done) || (state === s_xcpt)
  val enable_d = RegNext(io.enable, false.B)
  val reqAddrReg = RegEnable(reqInQueue.io.deq.bits.reqAddrIn, 0.U, next || enable_d)
  val s1DataReg  = RegEnable(reqInQueue.io.deq.bits.reqDataIn, 0.U, next || enable_d)
  val reqTagReg  = RegEnable(reqInQueue.io.deq.bits.reqTagIn,  0.U, next || enable_d)
  val maskReg    = RegEnable(reqInQueue.io.deq.bits.maskIn,    0.U, next || enable_d)

  io.reqValid := (state === s0) && ((maskReg === 1.U) || io.vm.toBool)
  io.reqAddr := reqAddrReg
  io.s1Data := s1DataReg
  io.reqTag := reqTagReg
  reqInQueue.io.deq.ready := (state === s_ready) || (state === s_xcpt)

  //io.reqCmd  := 0.U
  //io.reqSize := 0.U
  //io.s1Kill := false.B

  val memCauseReg = RegEnable(cause, 0.U, xcpt)
  val memMtvalReg = RegEnable(reqAddrReg, 0.U, xcpt)
  val memXcptPre = Wire(Bool())
  val memXcptReg = RegInit(false.B)
  val memVstartReg = RegEnable(reqTagReg, 0.U, xcpt)

  memXcptPre := Mux(xcpt, true.B, Mux(io.mem_xcpt_resp, false.B, memXcptReg))
  memXcptReg := memXcptPre

  io.mem_cause := memCauseReg
  io.mem_mtval := memMtvalReg
  io.mem_xcpt := memXcptReg
  io.mem_vstart := memVstartReg

  switch(state)
  {
    is(s_ready)
    {
       when(reqInQueue.io.deq.valid)
       {
          state := s0
       }
       .otherwise
       {
          state := s_ready
       }
    }

    is(s0)
    {
       when(io.reqReady)
       {
         state := s1
       }
    }

    is(s1)
    {
       state := s2
    }

    is(s2)
    {
       when(io.respValid)
       {
          state := s_done   
       }
       .elsewhen(io.s2Nack.toBool)
       {
          state := s0
       }
       .elsewhen(xcpt)
       {
          state := s_xcpt
       }
       .elsewhen(io.isLoad && ((maskReg === 1.U) || io.vm.toBool))
       {
          state := s2
       }
       .otherwise
       {
          state := s_done
       }
    }
    
    is(s_done)
    {
       state := s_ready
    }
    
    is(s_xcpt) // drop the leftover in the queue
    {
      when(!reqInQueue.io.deq.valid)
      {
         state := s_done
      }
    }
  }
 
}



