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
   
    val killm      = Input(Bool())
    val eret       = Input(Bool())
    val respS2Xcpt  = Input(new HellaCacheExceptions)
    val mem_cause  = Output(UInt(XLEN.W))
    val mem_xcpt   = Output(Bool())
    val mem_mtval  = Output(UInt(XLEN.W))
    val mem_vstart = Output(UInt(XLEN.W))

    val mem_vl     = Output(UInt(XLEN.W))
    val mem_vl_valid = Output(Bool())
    val stopLoad   = Output(Bool())
    val faultFirst = Output(Bool())
    val mem_xcpt_resp = Input(Bool())

    val killx     = Input(Bool())
    val killw     = Input(Bool())
    val replay       = Output(Bool())
    val wd     = Input(Bool())
  })  

  def checkExceptions(x: Seq[(Bool, UInt)]) = 
    (x.map(_._1).reduce(_||_), PriorityMux(x))

    val baseAddr   = Wire(UInt(XLEN.W))
    val offsetAddr = Wire(UInt(XLEN.W))
    val step       = Wire(UInt(XLEN.W))
    val stride     = Wire(UInt(XLEN.W))
    val cntPre     = Wire(UInt(vlenLog2.W))
    val cntPreWrapInc  = Wire(UInt(vlenLog2.W))
    val cntRollback  = Wire(UInt(vlenLog2.W))
    val cntReg     = RegInit(0.U(vlenLog2.W))
    val cntReg_d   = RegInit(0.U(vlenLog2.W))     // slow down
    val cntReg_dd   = RegInit(0.U(vlenLog2.W))     // slow down
    val sewData    = Wire(UInt(XLEN.W))

    val reqAddrReg  = RegInit(0.U(addrBits.W))
    val reqAddrReg_d  = RegInit(0.U(addrBits.W))
    val reqAddrReg_dd  = RegInit(0.U(addrBits.W))
    val reqDataReg  = RegInit(0.U(dataBits.W))
    val reqDataReg_d = RegInit(0.U(dataBits.W))
    val reqSizeReg   = RegInit(0.U(sizeBits.W))
    val reqCmdReg   = RegInit(0.U(5.W))
    val reqTagReg   = RegInit(0.U(tagBits.W))
    val reqValidReg = RegInit(false.B)

    val reqCmdPre   = Wire(UInt(5.W))
    val reqSizePre  = Wire(UInt(sizeBits.W))
    val reqAddrPre  = Wire(UInt(addrBits.W))
    val reqTagPre   = Wire(UInt(tagBits.W))


    val mask         = Wire(UInt(1.W))
    val maskElement  = Wire(UInt(1.W))
    val mlen         = Wire(UInt((vlenLog2+1).W))
    val totalEleNum  = Wire(UInt((vlenLog2+1).W))

    val hasSent      = RegInit(VecInit(Seq.fill(VLEN)(false.B)))
    val hasResp      = RegInit(VecInit(Seq.fill(VLEN)(false.B)))
    val sendSucceed  = RegInit(VecInit(Seq.fill(VLEN)(false.B)))

    val checkFirstAndSecond = Wire(Bool())
    val checkTheLastThree = Wire(Bool())

    val allSucceedPre = Wire(Bool())
    val allSucceedReg = RegInit(false.B)

    val hasSendFirstElePre = Wire(Bool())
    val hasSendFirstEleReg = RegInit(false.B)

    val enableFlagPre = Wire(Bool())
    val enableFlagReg = RegInit(false.B)

    //val ones = Wire(UInt(vlenLog2.W))
    //ones := Cat(Seq.fill(vlenLog2){"h1".U(1.W)})

    val nFieldsReg = RegInit(0.U(NFIELDS_SZ.W))
    val nFieldsPre = Wire(UInt(NFIELDS_SZ.W))
    val curPtrReg = RegInit(0.U(vlenLog2.W))
    val curPtrPre = Wire(UInt(vlenLog2.W))
    val eleOffset = Wire(UInt(vlenLog2.W))

    val theSecondEle   = Wire(UInt(vlenLog2.W))
    val theLastEle   = Wire(UInt(vlenLog2.W))
    val theSecondToLast   = Wire(UInt(vlenLog2.W))
    val theThirdToLast   = Wire(UInt(vlenLog2.W))

    val nFieldsRollback = Wire(UInt(NFIELDS_SZ.W))
    val nFieldsPreWrapInc = Wire(UInt(NFIELDS_SZ.W))
    val nFieldsReg_d   = RegInit(0.U(NFIELDS_SZ.W))     // slow down
    val nFieldsReg_dd  = RegInit(0.U(NFIELDS_SZ.W))     // slow down
    val curPtrReg_d = RegInit(0.U(vlenLog2.W))
    val curPtrReg_dd = RegInit(0.U(vlenLog2.W))

    val vmReal       = Wire(UInt(1.W))
    val vsewReal     = Wire(UInt(VSEW_SZ.W))
    val vlmulReal    = Wire(UInt(VLMUL_SZ.W))
    val vlReal       = Wire(UInt(XLEN.W))
    val nFieldsReal  = Wire(UInt(NFIELDS_SZ.W))

    val isNormalUnitStride = (io.addrMode === UStride) && (io.lumop === "b00000".U)
    val isWholeRegisters = (io.addrMode === UStride) && (io.lumop === "b01000".U)
    val isFaultOnlyFirst = (io.addrMode === UStride) && (io.lumop === "b10000".U)

    val s1KillRegPre = Wire(Bool())
    val s1KillReg = RegInit(false.B)
    val s1Kill_d = RegInit(false.B)

    // xcpt maybe level, instead of pluse
    val (xcpt, cause) = checkExceptions(List(
      (!s1Kill_d && enableFlagPre && io.respS2Xcpt.ma.st, "h6".U),
      (!s1Kill_d && enableFlagPre && io.respS2Xcpt.ma.ld, "h4".U),
      (!s1Kill_d && enableFlagPre && io.respS2Xcpt.pf.st, "hf".U),
      (!s1Kill_d && enableFlagPre && io.respS2Xcpt.pf.ld, "hd".U),
      (!s1Kill_d && enableFlagPre && io.respS2Xcpt.ae.st, "h7".U),
      (!s1Kill_d && enableFlagPre && io.respS2Xcpt.ae.ld, "h5".U)
    ))

    val xcptPluse = Wire(Bool())
    val xcptPluseReg = RegInit(false.B)
    val xcpt_level = Wire(Bool())
    val xcpt_levelReg = RegInit(false.B)

    val firstEleNack = RegInit(false.B)
    val switchNext = sendSucceed(0) || firstEleNack

    // io.killm must be pulled up when io.enable is one,
    // on the other hand, if io.enable is zero in previous cycle, io.killm cannot be pulled up in the next cycle
    val reqValidFirst = io.enable && !io.killx && ((io.v0m1(0) | io.vm) === 1.U) && (vlReal =/= 0.U)
    val s1KillFirst = io.killm
    val reqCmdFirst = reqCmdPre
    val reqSizeFirst = reqSizePre
    val offsetAddrFirst = Mux(io.addrMode === Indexed || io.majFun === IsAMO, io.vs2toXlen(0), 0.U)
    val reqAddrFirst = baseAddr + offsetAddrFirst
    //val reqTagFirst = 0.U
    //val s1Data = reqDataReg

    io.replay := io.enable && !io.reqReady

    // make xcpt to pluse
    xcptPluseReg := xcpt
    xcptPluse := xcpt && !xcptPluseReg
    xcpt_levelReg := xcpt_level
    xcpt_level := Mux(xcptPluse, true.B, Mux(io.done || io.eret, false.B, xcpt_levelReg))
   
    s1KillRegPre := xcptPluse
    s1KillReg := s1KillRegPre
    io.s1Kill := Mux(switchNext, s1KillReg || s1KillRegPre, s1KillFirst)
    s1Kill_d := io.s1Kill
    
    val mem_xcpt_pre = Wire(Bool())
    val mem_xcpt_reg = RegInit(false.B)
    val mem_vstart_reg = RegEnable(curPtrReg_dd, 0.U, xcptPluse)
    val mem_mtval_reg = RegEnable(reqAddrReg_dd, 0.U, xcptPluse)
    val mem_cause_reg = RegEnable(cause, 0.U, xcptPluse)

    val firstEleExcp = xcptPluse && (curPtrReg_dd === 0.U)
    val notFirstFFExp = xcptPluse && isFaultOnlyFirst && (curPtrReg_dd =/= 0.U) 
    // first element's exception is unresumable
    val unresumable = firstEleExcp || (xcptPluse &&
         (cause === "h6".U || 
          cause === "h4".U || 
          cause === "hf".U ||
          cause === "hd".U ||
          cause === "h7".U || 
          cause === "h5".U))

    io.stopLoad := ((io.majFun === IsLoad) || (io.majFun === IsAMO && io.wd)) && (notFirstFFExp || unresumable || firstEleExcp)
    io.faultFirst := firstEleExcp

    // For first element's exception, mem_xcpt_pre should not be pull up;
    // because io.faultFirst has caught it, mem_xcpt_pre signal is only for imprecise exceptions
    mem_xcpt_pre := Mux(notFirstFFExp || firstEleExcp, false.B, Mux(xcptPluse, true.B, Mux(io.mem_xcpt_resp, false.B, mem_xcpt_reg)))
    mem_xcpt_reg := mem_xcpt_pre
    io.mem_xcpt := mem_xcpt_reg && (mem_vstart_reg =/= 0.U)
    io.mem_cause := Mux(firstEleExcp, cause, mem_cause_reg)
    io.mem_vstart := Mux(firstEleExcp, 0.U, mem_vstart_reg)
    io.mem_mtval := Mux(firstEleExcp, reqAddrFirst, mem_mtval_reg)

    io.mem_vl    := dgate(io.mem_vl_valid, curPtrReg_dd)
    io.mem_vl_valid := notFirstFFExp

    vmReal := Mux(isWholeRegisters, 1.U, io.vm)
    vsewReal := Mux(isWholeRegisters, 0.U, io.vsew)
    vlmulReal:= Mux(isWholeRegisters, 0.U, io.vlmul)
    vlReal := Mux(isWholeRegisters, (VLEN/8).U, io.vl)
    nFieldsReal := Mux(io.majFun === IsAMO, 0.U, io.nFields)

    nFieldsReg := Mux(io.done || io.killm, 0.U, nFieldsPre)
    nFieldsReg_d := Mux(io.done || io.killm, 0.U, nFieldsReg)
    nFieldsReg_dd := Mux(io.done || io.killm, 0.U, nFieldsReg_d)
    curPtrReg := Mux(io.done || io.killm, 0.U, curPtrPre)
    curPtrReg_d := Mux(io.done || io.killm, 0.U, curPtrReg)
    curPtrReg_dd := Mux(io.done || io.killm, 0.U, curPtrReg_d)
    cntReg  := Mux(io.done || io.killm, 0.U, cntPre)
    cntReg_d := Mux(io.done || io.killm, 0.U, cntReg)
    cntReg_dd := Mux(io.done || io.killm, 0.U, cntReg_d)
    mlen    := 1.U((vlenLog2+1).W) << (vsewReal + 3.U((vlenLog2+1).W) - vlmulReal)

    // find the last element's index:
    // the second to last element's index,
    // the third to last element's index,
    // the second element's index
    if(!SEGLS)
    {
      theLastEle  := vlReal - 1.U
      theSecondToLast := theLastEle-1.U
      theThirdToLast := theSecondToLast-1.U
      theSecondEle := 1.U
      totalEleNum := vlReal(vlenLog2,0)
    }
    else
    {
      theLastEle  := (vlReal - 1.U) + (nFieldsReal * eleOffset)
      theSecondToLast := Mux(nFieldsReal === 0.U, theLastEle-1.U, theLastEle-eleOffset)
      theThirdToLast := MuxCase(theSecondToLast-eleOffset, Array(
           (nFieldsReal === 0.U) -> (theSecondToLast-1.U),
           (nFieldsReal === 1.U) -> (theLastEle-1.U)
        ))

      theSecondEle := Mux(nFieldsReal === 0.U, 1.U, eleOffset)
      totalEleNum := (vlReal(vlenLog2,0) * (nFieldsReal +& 1.U))(vlenLog2,0)
    }

    // check whether the first and the second elements have been sent;
    checkFirstAndSecond := sendSucceed(0) && sendSucceed(theSecondEle)

    // check whether the last three elements have been sent.
    checkTheLastThree   := sendSucceed(theLastEle) &&
                           sendSucceed(theSecondToLast) &&
                           sendSucceed(theThirdToLast)

    allSucceedPre := MuxCase(sendSucceed(theLastEle), Array(
       (totalEleNum === 2.U) -> checkFirstAndSecond,
       (totalEleNum  >= 3.U) -> checkTheLastThree
    ))

    allSucceedReg := allSucceedPre
    io.done := !allSucceedReg && allSucceedPre
    enableFlagReg := enableFlagPre
  

    reqCmdReg := reqCmdPre
    io.reqCmd  := Mux(switchNext , reqCmdReg, reqCmdFirst)

    reqSizeReg := reqSizePre
    io.reqSize  := Mux(switchNext, reqSizeReg, reqSizeFirst)

    reqAddrReg := reqAddrPre
    io.reqAddr := Mux(switchNext, reqAddrReg, reqAddrFirst)
    reqAddrReg_d := reqAddrReg
    reqAddrReg_dd := reqAddrReg_d

    reqTagReg := reqTagPre
    io.reqTag  := Mux(switchNext, reqTagReg, 0.U)

    reqDataReg_d  := reqDataReg
    io.s1Data  := Mux(switchNext, reqDataReg_d, reqDataReg)

    io.reqValid := Mux(io.enable, reqValidFirst, Mux(RegNext(switchNext, false.B), reqValidReg, reqValidFirst))


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

    reqCmdPre := MuxCase("b11111".U, Array(
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
    reqSizePre := MuxCase(0.U, Array(
          ((io.ldstWidth === LdStByte) -> "b000".U),
          ((io.ldstWidth === LdStHalf) -> "b001".U),
          ((io.ldstWidth === LdStWord) -> "b010".U),
          ((io.ldstWidth === LdStVSEW) -> vsewReal)
        ))
    // TODO: maybe can be simpler --- reqSizeReg := vsewReal


    /*****************************************
       generate request address;
       ldstWidth: 00 -- load/store byte;
                  01 -- load/store half word;
                  10 -- load/store word;
                  11 -- load/store SEW
    ******************************************/
    stride := MuxCase(0.U, Array(
        ((io.ldstWidth === LdStVSEW) -> (1.U(XLEN.W) << vsewReal)),
        ((io.ldstWidth === LdStWord) -> 4.U(XLEN.W)),
        ((io.ldstWidth === LdStHalf) -> 2.U(XLEN.W)),
        ((io.ldstWidth === LdStByte) -> 1.U(XLEN.W))
      ))

    step := MuxCase(0.U, Array(
         ((io.addrMode === Strided) -> io.fromXData2),
         ((io.addrMode === UStride) -> stride*(nFieldsReal+&1.U))
      ))

    baseAddr   := io.fromXData1
    offsetAddr := Mux(io.addrMode === Indexed || io.majFun === IsAMO, io.vs2toXlen(cntPre), cntPre * step)
    if(!SEGLS)
       reqAddrPre := baseAddr + offsetAddr
    else
       reqAddrPre := baseAddr + offsetAddr + (nFieldsPre * stride)
       // TODO: how to deal with vlsseg3b.v when SEW = 32?
       // offsetAddr * (nFieldsReal+&1.U) = cntPre * (step * (nFieldsReal+&1.U))
       //                                or = io.vs2toXlen(cntPre) * (nFieldsReal+&1.U)
       // skip step*(nFieldsReal+&1.U) bytes


    //*****************************************
    //         generate request tag
    //*****************************************
    reqTagPre := curPtrPre
    // TODO: curPtrPre maybe like 0, 4, 1, 5, ..., 3, 7, not 0, 1, 2, 3, ..., 6, 7


    /*****************************************
       generate request data;
       ldstWidth: 00 -- load/store byte;
                  01 -- load/store half word;
                  10 -- load/store word;
                  11 -- load/store SEW
    ******************************************/
    sewData := MuxCase(0.U, Array(
         (vsewReal === ByteWidth)  -> Cat(Seq.fill(XLEN/8){io.vdvs3e8(curPtrPre)}),
         (vsewReal === HWordWidth) -> Cat(Seq.fill(XLEN/16){io.vdvs3e16(curPtrPre)}),
         (vsewReal === WordWidth)  -> Cat(Seq.fill(XLEN/32){io.vdvs3e32(curPtrPre)})
        )
++ (if(ELEN >= 64) Array((vsewReal === DWordWidth) -> io.vdvs3e64(curPtrPre)) else Nil)
      )

    reqDataReg := MuxCase(0.U, Array(
       (io.ldstWidth === LdStByte) -> Cat(Seq.fill(XLEN/8){io.vdvs3e8(curPtrPre << vsewReal)}),
       (io.ldstWidth === LdStHalf) -> Cat(Seq.fill(XLEN/16){io.vdvs3e16(curPtrPre << (vsewReal - 1.U))}),
       (io.ldstWidth === LdStWord) -> Cat(Seq.fill(XLEN/32){io.vdvs3e32(curPtrPre << (vsewReal - 2.U))}),
       (io.ldstWidth === LdStVSEW) -> sewData
     ))

    //*****************************************
    //         generate request valid
    //*****************************************
    maskElement := MuxCase(0.U, Array(
                           (mlen === 32.U) -> (io.v0m32(cntPre) | vmReal),
                           (mlen === 16.U) -> (io.v0m16(cntPre) | vmReal),
                           (mlen === 8.U)  -> (io.v0m8(cntPre)  | vmReal),
                           (mlen === 4.U)  -> (io.v0m4(cntPre)  | vmReal),
                           (mlen === 2.U)  -> (io.v0m2(cntPre)  | vmReal),
                           (mlen === 1.U)  -> (io.v0m1(cntPre)  | vmReal))
++ (if(ELEN >= 64)   Array((mlen === 64.U) -> (io.v0m64(cntPre) | vmReal)) else Nil)
      )  

    // if it havenot been sent yet, then send it.
    // if it have been sent and have a response, but wasnot successful, then send it again.
    // there is a special case when 'vl === 1.U && nFieldsReal === 0.U', since hasSent signal has one cycle delay
    // as a result, I use another signal 'hasSendFirstElePre' which has not cycle delay, to adjust reqValid signal
    // enableFlagPre is used to restrict reqValid signal
    reqValidReg := (io.majFun === IsLoad || io.majFun === IsStore || io.majFun === IsAMO) &&
		   (vlReal =/= 0.U) &&
		   (maskElement === 1.U) &&
                   (!hasSent(curPtrPre) || (hasResp(curPtrPre) && !sendSucceed(curPtrPre))) &&
                   (Mux((vlReal === 1.U) && (nFieldsReal === 0.U), !(hasSendFirstElePre || hasSendFirstEleReg), true.B)) &&
                   enableFlagPre

    hasSendFirstElePre := MuxCase(hasSendFirstEleReg, Array(
      (io.enable || io.respValid || io.s2Nack === 1.U)  -> false.B,
      (curPtrPre === 0.U && io.reqValid && io.reqReady) -> true.B
    ))

    hasSendFirstEleReg := hasSendFirstElePre 

    //***********************************************************
    //         generate cntReg & nFieldsReg & curPtrReg pointers
    //***********************************************************
    mask := MuxCase(0.U, Array(
                           (mlen === 32.U) -> (io.v0m32(cntReg) | vmReal),
                           (mlen === 16.U) -> (io.v0m16(cntReg) | vmReal),
                           (mlen === 8.U)  -> (io.v0m8(cntReg)  | vmReal),
                           (mlen === 4.U)  -> (io.v0m4(cntReg)  | vmReal),
                           (mlen === 2.U)  -> (io.v0m2(cntReg)  | vmReal),
                           (mlen === 1.U)  -> (io.v0m1(cntReg)  | vmReal))
++ (if(ELEN >= 64)   Array((mlen === 64.U) -> (io.v0m64(cntReg) | vmReal)) else Nil)
       )

    eleOffset := VLEN.U >> (vsewReal + 3.U - vlmulReal)

    if(!SEGLS)
    {
      val hasSendCurEle = hasResp(curPtrReg) && sendSucceed(curPtrReg)
      val canSkipCurEle = (io.reqValid && io.reqReady) || mask === 0.U || (hasResp(cntReg) && sendSucceed(cntReg))

      nFieldsPreWrapInc := 0.U
      nFieldsRollback := 0.U
      nFieldsPre := 0.U

      curPtrPre := cntPre
      cntPreWrapInc := Mux(canSkipCurEle && !xcpt_level, Mux(cntReg === theLastEle || !enableFlagPre, 0.U, cntReg + 1.U), cntReg)
      cntRollback := Mux(hasSendCurEle, cntReg_dd, Mux(cntReg_dd < cntReg, cntReg_dd, cntReg))
      cntPre := Mux(io.enable, 0.U, Mux(io.s2Nack === 1.U, cntRollback, cntPreWrapInc))
    }
    else
    {
      val hasSendCurEle = hasResp(curPtrReg) && sendSucceed(curPtrReg)
      val canSkipCurEle = (io.reqValid && io.reqReady) || mask === 0.U || hasSendCurEle

      curPtrPre := cntPre + nFieldsPre * eleOffset  // TODO: curPtrPre := cntPre + nFieldsPre << (N.U - vsewReal - 3.U + vlmulReal); N = log2(VLEN)
      // TODO: when mask === 0.U, nFields should be 0.U, cntReg should increase itself.
      // Answer: maybe not, how to recognize which element has been masked or not, if you only increse cntReg pointer.
      // For example, when cntReg=1 and mask=0 and nFields=1, element[0] and element[2] all should be masked, not only just element[0].
      nFieldsPreWrapInc := Mux(canSkipCurEle, Mux(nFieldsReg === nFieldsReal || !enableFlagPre, 0.U, nFieldsReg + 1.U), nFieldsReg)
      nFieldsRollback := Mux(hasSendCurEle, nFieldsReg_dd, Mux(Cat(cntReg_dd, nFieldsReg_dd) < Cat(cntReg, nFieldsReg), nFieldsReg_dd, nFieldsReg))
      nFieldsPre := Mux(io.enable, 0.U, Mux(io.s2Nack === 1.U, nFieldsRollback, nFieldsPreWrapInc))

      val cntPreWrapIncTmp  = Wire(UInt(vlenLog2.W))
      val carryOut = (nFieldsReg === nFieldsReal) && (canSkipCurEle) && enableFlagPre

      cntPreWrapIncTmp := Mux(canSkipCurEle, Mux(cntReg === (vlReal-1.U) || !enableFlagPre, 0.U, cntReg + 1.U), cntReg)
      cntPreWrapInc := Mux(nFieldsReal === 0.U, cntPreWrapIncTmp, Mux(carryOut, Mux(cntReg === (vlReal-1.U) || !enableFlagPre, 0.U, cntReg + 1.U), cntReg))
      cntRollback := Mux(hasSendCurEle, cntReg_dd, Mux(cntReg_dd < cntReg, cntReg_dd, cntReg))
      cntPre := Mux(io.enable, 0.U, Mux(io.s2Nack === 1.U, cntRollback, cntPreWrapInc))
    }

    enableFlagPre := Mux(io.enable, true.B, Mux(io.done || RegNext(io.killm, false.B), false.B, enableFlagReg))

    
    when(io.done)  // cannot use 'io.enable' signal here, for it will has one cycle delay in the pipeline
    {
       for(i <- 0 until VLEN)
       {
          hasSent(i) := false.B
          hasResp(i) := false.B
          sendSucceed(i) := false.B
          firstEleNack := false.B
       }
    }
   .elsewhen(notFirstFFExp || unresumable || firstEleExcp)
   {
       for(i <- 0 until VLEN)
       {
          hasSent(i) := true.B
          hasResp(i) := true.B
          sendSucceed(i) := true.B
          firstEleNack := false.B
       }
   }
   .elsewhen(xcpt)
   {
//       hasSent(curPtrReg_dd) := false.B
       hasResp(curPtrReg_dd) := true.B    // false.B or true.B
       sendSucceed(curPtrReg_dd) := false.B

       // s1Kill valid
       when(!sendSucceed(curPtrReg_d))
       {
          hasSent(curPtrReg_d) := false.B
       }
       // s1Kill valid
       when(!sendSucceed(curPtrReg))
       {
          hasSent(curPtrReg) := false.B
       }
    }
    .elsewhen(io.killm)
    {
       hasSent(0) := false.B
       hasResp(0) := false.B
       sendSucceed(0) := false.B
    }
    .elsewhen(((io.v0m1(0) | io.vm) === 0.U) && RegNext(io.enable, false.B))
    {
       hasResp(0) := true.B
       sendSucceed(0) := true.B
    }
 

    when(io.reqValid && io.reqReady && !xcpt)
    {
       hasSent(io.reqTag) := true.B
       //hasResp(io.reqTag) := false.B
       sendSucceed(io.reqTag) := false.B
    }


    when((io.respValid || (!io.s2Nack && !s1Kill_d)) && !xcpt && RegNext(RegNext(io.reqValid && io.reqReady, false.B), false.B))
    {
       when(hasSent(curPtrReg_dd))
       {
         hasResp(curPtrReg_dd) := true.B
         sendSucceed(curPtrReg_dd) := true.B
       }
    }

    when(io.s2Nack === 1.U)
    {
       hasSent(curPtrReg_dd) := false.B
       when(curPtrReg_dd === 0.U)
       {
         firstEleNack := true.B
       }
       sendSucceed(curPtrReg_dd) := false.B
    }

    when(mask === 0.U && enableFlagPre && curPtrReg =/= 0.U)
    {
       hasResp(curPtrReg) := true.B
       sendSucceed(curPtrReg) := true.B
    }

  
}



