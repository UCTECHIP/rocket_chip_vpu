// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VPU_64V64E1L.scala
*       Author          :       liangzh
*       Revision        :       2020/04/09
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       top module of VPU for LMULMAX=1
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VPU_64V64E1L(params: VPUParams, tagBits: Int, addrBits: Int) extends VModule(params) {

  require(isPow2(VLEN) && VLEN >= 32)
  require(isPow2(ELEN) && ELEN >= 32 && ELEN <= 1024)
  require(isPow2(SELEN) && SELEN >= 1 && SELEN <= 8)
  require(isPow2(XLEN) && XLEN >= 32 && XLEN <= 64)
  require(isPow2(FLEN) && FLEN >= 32 && FLEN <= 64)
  require(isPow2(FSEWMAX) && FSEWMAX >= 32 && FSEWMAX <= 128)
  require(VLEN == ELEN && VLEN >= XLEN && VLEN >= FLEN)
  require(ELEN >= XLEN && ELEN >= FLEN && ELEN >= FSEWMAX)
  require(LMULMAX == 1)
  require(SELEN == 8) //TODO release SELEN


  val io = IO(new VPUIO(VLEN, XLEN, FLEN, tagBits, addrBits))
  /////////////////simplify input signals' name/////////////////
  val instReady          = Wire(Bool())
  val coreFire           = io.core.req.valid && instReady
  //for killing process
  val killx              = io.core.ctrl_killx
  val killm              = io.core.ctrl_killm
  val killw              = io.core.ctrl_killw
  //from Rocket Data -- VEXE state valid
  val fromXData1         = io.core.fromXData1
  val fromXData2         = io.core.fromXData2
  val fromFData          = io.fpu.req.fromFData
  /////////////////simplify input signals' name/////////////////

  /////////////////////instantiate modules//////////////////////
  val vdecode    = Module(new VDecode(params))
  val vexcp      = Module(new VDecodeExcp(params))
  val vcsr       = Module(new VCSR(params))
  val vsplit     = Module(new VSplit(params))
  val vm2e       = Module(new VMLENtoSEW(params))
  val vwidth     = Module(new VWidthConvert(params))
  val valc       = Module(new VAllocate(params))
  val vadd       = Module(new VFullAdder(params))
  val vbit       = Module(new VBitwise(params))
  val vshift     = Module(new VShift(params))
  val vcmp       = Module(new VCompare(params))
  val vpopc      = Module(new VPopc(params))
  val vmidx      = Module(new VMinIndex(params))
  val viota      = Module(new VIota(params))
  val vidx       = Module(new VIndex(params))
  val vmerge     = Module(new VMerge(params))
  val vmuldiv    = Module(new VMulDiv(params))
  val vred       = Module(new VReduction(params))
  val vmv        = Module(new VScalarMove(params))
  val vslide     = Module(new VSlide(params))
  val vrgather   = Module(new VRGather(params))
  val vcompress  = Module(new VCompress(params))
  val vfwidth    = Module(new VFWidthConvert(params))
  val vfma       = Module(new VFMA(params))
  val vfdiv      = Module(new VFDivSqrt(params))
  val vf2i       = Module(new VFIConvert(params))
  val vi2f       = Module(new VIFConvert(params))
  val vfcmp      = Module(new VFCompare(params))
  val vfclass    = Module(new VFClass(params))
  val vfsgnj     = Module(new VFSignInject(params))
  val vfun1      = Module(new VFunSelFront(params))
  val vfun2      = Module(new VFunSelBack(params))
  val vmask      = Module(new VMaskOut(params))
  val vdmemwidth = Module(new VDmemWidthConvert(params))
  val vdmemreq   = Module(new VDmemRequest(params, tagBits, addrBits))
  val vdmemresp  = Module(new VDmemResp(params, tagBits, addrBits))
  /////////////////////instantiate modules//////////////////////

  //////////////////////define registers////////////////////////
  //registers for VEXE stage
  val exe_reg_enable      = RegInit(false.B)
  val exe_reg_fromXData1  = RegInit(0.U(XLEN.W))
  val exe_reg_fromXData2  = RegInit(0.U(XLEN.W))
  val exe_reg_fromFData   = RegInit(0.U((FLEN+1).W))
  val exe_reg_ctrlSigs    = RegInit(0.U.asTypeOf(new VPUCtrlSigs))
  val exe_reg_vs1Data     = RegInit(0.U((8*VLEN).W))
  val exe_reg_vs2Data     = RegInit(0.U((8*VLEN).W))
  val exe_reg_vdvs3Data   = RegInit(0.U((8*VLEN).W))
  val exe_reg_v0Mask      = RegInit(0.U(VLEN.W))
  //registers for relay
  val relay_reg_Data0     = RegInit(0.U(ELEN.W))
  val relay_reg_Data1     = RegInit(0.U(ELEN.W))
  val relay_reg_Data2     = RegInit(0.U(ELEN.W))
  val relay_reg_Data3     = RegInit(0.U(ELEN.W))
  val relay_reg_Data4     = RegInit(0.U(ELEN.W))
  val relay_reg_Data5     = RegInit(0.U(ELEN.W))
  val relay_reg_Data6     = RegInit(0.U(ELEN.W))
  val relay_reg_Data7     = RegInit(0.U(ELEN.W))
  val relay_reg_vxsat     = RegInit(0.U(VPUConstants.XSAT_SZ.W))
  val relay_reg_fflags    = RegInit(0.U(VPUConstants.FFLAGS_SZ.W))
  //registes for VWB stage
  val exe_enable = Wire(Bool())
  val wb_reg_enable       = RegNext(exe_enable, false.B)
  val wb_reg_ctrlSigs     = RegEnable(exe_reg_ctrlSigs,    0.U.asTypeOf(new VPUCtrlSigs),   exe_enable)
  val wb_reg_veOut        = RegEnable(vmask.io.veOut,      0.U((8*VLEN).W),                 exe_enable)
  val wb_reg_vmOut        = RegEnable(vmask.io.vmOut,      0.U(VLEN.W),                     exe_enable)
  val wb_reg_scalarXOut   = RegEnable(vfun2.io.scalarXOut, 0.U(XLEN.W),                     exe_enable)
  val wb_reg_vxsat        = RegEnable(relay_reg_vxsat,     0.U(VPUConstants.XSAT_SZ.W),     exe_enable)
  val wb_reg_fflags       = RegEnable(relay_reg_fflags,    0.U(VPUConstants.FFLAGS_SZ.W),   exe_enable)
  //registers for upload illegal insts
  val illInst             = Wire(Bool())
  val llInst              = Wire(Bool())
  val upload_reg_illInst0 = RegNext(illInst, false.B)
  val upload_reg_illInst1 = RegNext(upload_reg_illInst0 && !killx, false.B)
  val upload_reg_illInst2 = RegNext(upload_reg_illInst1 && !killm, false.B)
  val upload_reg_inst0    = RegEnable(io.core.req.bits.inst, 0.U(32.W), illInst)
  val upload_reg_inst1    = RegEnable(upload_reg_inst0, 0.U(32.W), upload_reg_illInst0 && !killx)
  val upload_reg_inst2    = RegEnable(upload_reg_inst1, 0.U(32.W), upload_reg_illInst1 && !killm)
  //registers for kill vector insts
  val eInst               = exe_reg_ctrlSigs.isCSRInst || exe_reg_ctrlSigs.isALInst
  val ePipem              = RegNext(exe_reg_enable && eInst && !killx, false.B)
  val ePipew              = RegNext(ePipem && !killm, false.B)
  val mInst               = exe_reg_ctrlSigs.isLdInst || exe_reg_ctrlSigs.isStInst || exe_reg_ctrlSigs.isAMOInst
  val mPipem              = RegNext(exe_reg_enable && mInst && !killx, false.B)
  val mPipew              = RegNext(mPipem && !killm, false.B)
  //status for main FSM
  def idle      = 0.U(6.W)
  def inVLoad   = 1.U(6.W)
  def inVStore  = 2.U(6.W)
  def inExpt    = 3.U(6.W)
  def inOCFull  = 4.U(6.W)
  def inMCFull  = 5.U(6.W)
  def inOCPart0 = 6.U(6.W)
  def inOCPart1 = 7.U(6.W)
  def inOCPart2 = 8.U(6.W)
  def inOCPart3 = 9.U(6.W)
  def inOCPart4 = 10.U(6.W)
  def inOCPart5 = 11.U(6.W)
  def inOCPart6 = 12.U(6.W)
  def inOCPart7 = 13.U(6.W)
  def inMCWait0  = 14.U(6.W)
  def inMCWait1  = 15.U(6.W)
  def inMCWait2  = 16.U(6.W)
  def inMCPart0w = 17.U(6.W)
  def inMCPart1p = 18.U(6.W)
  def inMCPart1w = 19.U(6.W)
  def inMCPart2p = 20.U(6.W)
  def inMCPart2w = 21.U(6.W)
  def inMCPart3p = 22.U(6.W)
  def inMCPart3w = 23.U(6.W)
  def inMCPart4p = 24.U(6.W)
  def inMCPart4w = 25.U(6.W)
  def inMCPart5p = 26.U(6.W)
  def inMCPart5w = 27.U(6.W)
  def inMCPart6p = 28.U(6.W)
  def inMCPart6w = 29.U(6.W)
  def inMCPart7p = 30.U(6.W)
  def inMCPart7w = 31.U(6.W)
  def finish     = 32.U(6.W)
  val vState = RegInit(idle)
  //////////////////////define registers////////////////////////

  //////////////////////important signals//////////////////////
  //simplify name
  val iSigs = vdecode.io.sigs
  val eSigs = exe_reg_ctrlSigs
  val wSigs = wb_reg_ctrlSigs
  //from vdecode module
  lazy val isLdInst     = vdecode.io.sigs.isLdInst || vdecode.io.sigs.isAMOInst && iSigs.funct6(0)
  lazy val isStInst     = vdecode.io.sigs.isStInst || vdecode.io.sigs.isAMOInst && !iSigs.funct6(0)
  //one cycle full vector operation AL insts
  lazy val isOCFullInst = IsPopc <= iSigs.majFun  && iSigs.majFun <= IsMIdx   || 
                          IsIdx  <= iSigs.majFun  && iSigs.majFun <= IsMBit   || 
                          IsMv   <= iSigs.majFun  && iSigs.majFun <= IsSlide1 || 
                          iSigs.majFun === IsFMv  && iSigs.isScalarXOut       || 
                          iSigs.majFun === IsCopy
  //multiply cycles full vector operation AL insts
  lazy val isMCFullInst = iSigs.majFun === IsCSR || iSigs.majFun === IsIota || iSigs.majFun === IsZip || 
                          iSigs.majFun === IsFMv && iSigs.isSEWOut
  //one cycle part vector operation AL insts
  lazy val isOCPartInst = iSigs.majFun <= IsMerge || iSigs.majFun === IsGather
  //multiply cycles part vector operation AL insts
  lazy val isMCPartInst = iSigs.majFun === IsMulDiv || iSigs.majFun === IsMulAdd || iSigs.majFun === IsRed || 
                          IsFMA  <= iSigs.majFun    && iSigs.majFun <= IsFMerge  || 
                          IsFDiv <= iSigs.majFun    && iSigs.majFun <= IsFRed
  ////////////////////////////////////////////////////////
  lazy val inOCFullState = RegEnable(isOCFullInst || iSigs.majFun === IsCSR, false.B, llInst)
  lazy val inMCFullState = eSigs.majFun === IsIota || eSigs.majFun === IsZip || 
                           eSigs.majFun === IsFMv && eSigs.isSEWOut
  lazy val inRedState    = eSigs.majFun === IsRed || eSigs.majFun === IsFRed //for overwrite relay_reg_Data0
  lazy val inMulDivState = eSigs.majFun === IsMulDiv  //for vxsat
  lazy val inShiftState  = eSigs.majFun === IsShift   //for vxsat

  ////////////////////////////////////////////////////////
  //multiply cycles part vector operation valid
  def isMCReqValid  = vState === inMCWait2 && !killw || vState === inMCPart1p || 
                      vState === inMCPart2p          || vState === inMCPart3p || 
                      vState === inMCPart4p          || vState === inMCPart5p || 
                      vState === inMCPart6p          || vState === inMCPart7p
  def isMCFPRespValid = vfma.io.resp.valid  || vfdiv.io.resp.valid || 
                        vfcmp.io.resp.valid || vi2f.io.resp.valid  || vf2i.io.resp.valid  || 
                        vfwidth.io.respValid && (eSigs.majFun === IsFClass || eSigs.majFun === IsFSgnJ || 
                                                 eSigs.majFun === IsFCvt && eSigs.src1Field(2,0) >= 4.U(3.W) || 
                                                 eSigs.majFun === IsFMerge)
  def isMCRespValid = vmuldiv.io.resp.valid || vred.io.vRedOut.valid || isMCFPRespValid
  ////////////////////////////////////////////////////////
  //from vdmemreq and vdmemresp modules 
  lazy val faultFirst = vdmemreq.io.faultFirst
  lazy val vSendOver  = vdmemreq.io.done && eSigs.majFun === IsStore || 
                        vdmemreq.io.done && eSigs.majFun === IsAMO && !eSigs.funct6(0)
  lazy val vRecvOver  = vdmemresp.io.vRecvOver && eSigs.majFun === IsLoad || 
                        vdmemresp.io.vRecvOver && eSigs.majFun === IsAMO && eSigs.funct6(0)


  //for redirection
//  def needCSR    = vState === inOCFull && coreFire && eSigs.majFun === IsCSR
  def needEXEVs1 = vState === inOCFull && coreFire && eSigs.dstField === iSigs.src1Field && !eSigs.isScalarXOut
  def needEXEVs2 = vState === inOCFull && coreFire && eSigs.dstField === iSigs.src2Field && !eSigs.isScalarXOut
  def needEXEVd  = vState === inOCFull && coreFire && eSigs.dstField === iSigs.dstField && !eSigs.isScalarXOut
  def needEXEV0  = vState === inOCFull && coreFire && eSigs.dstField === 0.U(5.W) && !eSigs.isScalarXOut
  def needWBVs1  = coreFire && wb_reg_enable && wSigs.dstField === iSigs.src1Field && !wSigs.isScalarXOut
  def needWBVs2  = coreFire && wb_reg_enable && wSigs.dstField === iSigs.src2Field && !wSigs.isScalarXOut
  def needWBVd   = coreFire && wb_reg_enable && wSigs.dstField === iSigs.dstField && !wSigs.isScalarXOut
  def needWBV0   = coreFire && wb_reg_enable && wSigs.dstField === 0.U(5.W) && !iSigs.vm.asBool && !wSigs.isScalarXOut

  //vstart enable and value
  val vstartEn = exe_enable || vRecvOver || vSendOver || vdmemreq.io.mem_xcpt
  val vstartIn = MuxCase(0.U(log2Ceil(VLEN).W),
                   Array(vdmemreq.io.mem_xcpt -> vdmemreq.io.mem_vstart))

  //ignore some operation that elements cannot be divided into sub-elements
  def noDivInst = eSigs.majFun === IsMBit  || eSigs.majFun === IsPopc   || eSigs.majFun === IsFirst || 
                  eSigs.majFun === IsMIdx  || eSigs.majFun === IsIota   || eSigs.majFun === IsIdx   || 
                  eSigs.majFun === IsSlide || eSigs.majFun === IsSlide1 || eSigs.majFun === IsMv    || 
                  eSigs.majFun === IsFMv   || eSigs.majFun === IsZip    || eSigs.majFun === IsCmp   || 
                  eSigs.majFun === IsFCmp  || eSigs.majFun === IsAdd && 
                  (eSigs.addFun === AddFun_Adc || eSigs.addFun === AddFun_Sbc || eSigs.isMLENOut) || 
                  eSigs.majFun === IsRed && eSigs.funct6(4,0) === RedFun_Sum
  val vediv     = (if(EDIV) Mux(noDivInst, 0.U, vcsr.io.vediv) else 0.U(2.W))

  val vxsat     = MuxCase(vadd.io.vAddSat, Array()
                  ++ (if(SATMUL) Array(inMulDivState -> vmuldiv.io.vMulSat)  else Nil)
                  ++ (if(NCLIP)  Array(inShiftState  -> vshift.io.vShiftSat) else Nil))


  illInst    := coreFire && vexcp.io.excpFlag
  llInst     := coreFire && !vexcp.io.excpFlag
  instReady  := vState === idle || vState === inOCFull
  exe_enable := MuxCase(vState === finish,
                  Array(inOCFullState -> (exe_reg_enable && eInst && !killx),
                        inMCFullState -> (viota.io.resp.valid || vcompress.io.respValid || 
                                          eSigs.majFun === IsFMv && eSigs.isSEWOut)))

  val inStep = Wire(Vec(7, Bool()))
  inStep(0) := vState === inOCPart1 || vState === inMCPart1p || vState === inMCPart1w
  inStep(1) := vState === inOCPart2 || vState === inMCPart2p || vState === inMCPart2w
  inStep(2) := vState === inOCPart3 || vState === inMCPart3p || vState === inMCPart3w
  inStep(3) := vState === inOCPart4 || vState === inMCPart4p || vState === inMCPart4w
  inStep(4) := vState === inOCPart5 || vState === inMCPart5p || vState === inMCPart5w
  inStep(5) := vState === inOCPart6 || vState === inMCPart6p || vState === inMCPart6w
  inStep(6) := vState === inOCPart7 || vState === inMCPart7p || vState === inMCPart7w
  //////////////////////important signals//////////////////////

  ///////////////////wire signals to output/////////////////////
  io.reqValid               := vdmemreq.io.reqValid
  io.reqAddr                := vdmemreq.io.reqAddr
  io.reqTag                 := vdmemreq.io.reqTag
  io.reqCmd                 := vdmemreq.io.reqCmd
  io.reqSize                := vdmemreq.io.reqSize
  io.s1Kill                 := vdmemreq.io.s1Kill
  io.s1Data                 := vdmemreq.io.s1Data

  io.core.req.ready         := true.B
  io.core.resp.toXData      := Mux(wSigs.majFun === IsCSR, vcsr.io.mem_reg_vl, wb_reg_scalarXOut)
  io.core.resp.rd           := wSigs.dstField
  io.core.fflags_ready      := !(inMCWait0 <= vState && vState <= finish && eSigs.wflags || wb_reg_enable && wSigs.wflags)
  io.core.vcsr_fflags.valid := wb_reg_enable && wSigs.wflags
  io.core.vcsr_fflags.bits  := wb_reg_fflags
  io.core.vxsat_ready       := !(inOCPart0 <= vState && vState <= finish && eSigs.isSAT || wb_reg_enable && wSigs.isSAT)
  io.core.vcsr_vxsat.valid  := wb_reg_enable && wSigs.isSAT
  io.core.vcsr_vxsat.bits   := wb_reg_vxsat
  io.core.vcsr_vstart       := vcsr.io.vstart
  io.core.vcsr_vl           := vcsr.io.mem_reg_vl      //TODO see whether data conflict or not
  io.core.vcsr_vtype        := vcsr.io.mem_reg_vtype   //TODO see whether data conflict or not
  io.core.vxcpt_precise     := faultFirst || upload_reg_illInst2 && !killw
  io.core.vxcpt_imprecise   := vdmemreq.io.mem_xcpt
  io.core.vcause            := Mux(faultFirst || vdmemreq.io.mem_xcpt, vdmemreq.io.mem_cause, 2.U)
  io.core.vmtval            := Mux(faultFirst || vdmemreq.io.mem_xcpt, vdmemreq.io.mem_mtval, upload_reg_inst2)
  io.core.vnack             := RegNext(RegNext(io.core.req.fire() && !instReady, false.B) && !killx || 
                                       vdmemreq.io.replay, false.B) && !killm
  io.core.dcache_blocked    := vState === inVLoad || vState === inVStore

  val fpuResp                = Wire(Flipped(Decoupled(new VPUFPUResponse(FLEN))))
  fpuResp.valid             := exe_reg_enable && eSigs.isScalarXOut && eSigs.majFun === IsFMv
  fpuResp.bits.toFData      := vfun2.io.scalarFOut
  fpuResp.bits.rd           := eSigs.dstField
  io.fpu.resp               <> Queue(enq = fpuResp, entries = 2)
  io.fpu.req.rs1            := eSigs.src1Field
  ///////////////////wire signals to output/////////////////////


//////////////////////////////////////////decode state//////////////////////////////////////

  vdecode.io.inst     := io.core.req.bits.inst

//  vexcp.io.vill       := Mux(needCSR, vcsr.io.villNext,  vcsr.io.vill)
//  vexcp.io.vediv      := Mux(needCSR, vcsr.io.vedivNext, vcsr.io.vediv)
//  vexcp.io.vsew       := Mux(needCSR, vcsr.io.vsewNext,  vcsr.io.vsew)
//  vexcp.io.vlmul      := Mux(needCSR, vcsr.io.vlmulNext, vcsr.io.vlmul)
  vexcp.io.coreFire   := coreFire
  vexcp.io.vill       := vcsr.io.vill
  vexcp.io.vediv      := vcsr.io.vediv
  vexcp.io.vsew       := vcsr.io.vsew
  vexcp.io.vlmul      := vcsr.io.vlmul
  vexcp.io.vstart     := vcsr.io.vstart
  vexcp.io.src1Field  := iSigs.src1Field
  vexcp.io.src2Field  := iSigs.src2Field
  vexcp.io.dstField   := iSigs.dstField
  vexcp.io.isALInst   := iSigs.isALInst
  vexcp.io.isLdInst   := iSigs.isLdInst
  vexcp.io.isStInst   := iSigs.isStInst
  vexcp.io.isAMOInst  := iSigs.isAMOInst
  vexcp.io.isSrc12SEW := iSigs.isSrc12SEW
  vexcp.io.isSrc22SEW := iSigs.isSrc22SEW
  vexcp.io.isVd2SEW   := iSigs.isVd2SEW
  vexcp.io.isSEWOut   := iSigs.isSEWOut
  vexcp.io.isMLENOut  := iSigs.isMLENOut
  vexcp.io.src1Typ    := iSigs.src1Typ
  vexcp.io.majFun     := iSigs.majFun
  vexcp.io.mulFun     := iSigs.mulFun
  vexcp.io.slideFun   := iSigs.funct6(0)
  vexcp.io.nFields    := iSigs.funct6(5,3)
  vexcp.io.addrMode   := iSigs.funct6(1,0)
  vexcp.io.ldstWidth  := iSigs.ldstWidth
  vexcp.io.vm         := iSigs.vm
  vexcp.io.isFullMul  := iSigs.isFullMul

/////////////////////read regfile///////////////////////////

  val vRegFile = Mem(32, UInt(VLEN.W))

  //read source 1, source 2, destination 8 registers, and mask single register
  val v0    = vRegFile(0.U)
  val vs1   = for(i <- 0 until 8) yield vRegFile(iSigs.src1Field + i.U)
  val vs2   = for(i <- 0 until 8) yield vRegFile(iSigs.src2Field + i.U)
  val vdvs3 = for(i <- 0 until 8) yield vRegFile(iSigs.dstField + i.U)
  //8 vectors a set
  val vs1Full    = Cat(vs1.reverse)
  val vs2Full    = Cat(vs2.reverse)
  val vdvs3Full  = Cat(vdvs3.reverse)

//////////////////////////////////////////decode state//////////////////////////////////////


//////////////////////////////////////exe regs assignment///////////////////////////////////

  exe_reg_enable  := llInst

  when(llInst) {
    exe_reg_ctrlSigs   := iSigs
    exe_reg_vs1Data    := Mux(needWBVs1, wb_reg_veOut, Mux(needEXEVs1, vmask.io.veOut, vs1Full))
    exe_reg_vs2Data    := Mux(needWBVs2, wb_reg_veOut, Mux(needEXEVs2, vmask.io.veOut, vs2Full))
    exe_reg_vdvs3Data  := Mux(needWBVd, wb_reg_veOut, Mux(needEXEVd, vmask.io.veOut, vdvs3Full))
    exe_reg_v0Mask     := Mux(needWBV0, wb_reg_vmOut, Mux(needEXEV0, vmask.io.vmOut, v0))
  }
  when(exe_reg_enable) {
    exe_reg_fromXData1 := fromXData1
    exe_reg_fromXData2 := fromXData2
  }
  when(vState === inMCWait1) {
    exe_reg_fromFData  := fromFData
  }


  when(vState === inOCPart0 || vState === inMCPart0w && isMCRespValid || inRedState)    { relay_reg_Data0 := vfun1.io.vSEWData }
  when((vState === inOCPart1 || vState === inMCPart1w && isMCRespValid) && !inRedState) { relay_reg_Data1 := vfun1.io.vSEWData }
  when((vState === inOCPart2 || vState === inMCPart2w && isMCRespValid) && !inRedState) { relay_reg_Data2 := vfun1.io.vSEWData }
  when((vState === inOCPart3 || vState === inMCPart3w && isMCRespValid) && !inRedState) { relay_reg_Data3 := vfun1.io.vSEWData }
  when((vState === inOCPart4 || vState === inMCPart4w && isMCRespValid) && !inRedState) { relay_reg_Data4 := vfun1.io.vSEWData }
  when((vState === inOCPart5 || vState === inMCPart5w && isMCRespValid) && !inRedState) { relay_reg_Data5 := vfun1.io.vSEWData }
  when((vState === inOCPart6 || vState === inMCPart6w && isMCRespValid) && !inRedState) { relay_reg_Data6 := vfun1.io.vSEWData }
  when((vState === inOCPart7 || vState === inMCPart7w && isMCRespValid) && !inRedState) { relay_reg_Data7 := vfun1.io.vSEWData }

  when(vState === finish) {
    relay_reg_vxsat := 0.U(VPUConstants.XSAT_SZ)
  }.elsewhen(eSigs.majFun === IsAdd || eSigs.majFun === IsShift || eSigs.majFun === IsMulDiv && vmuldiv.io.resp.valid) {
    relay_reg_vxsat := relay_reg_vxsat | vxsat
  }

  when(vState === finish) {
   relay_reg_fflags := 0.U(VPUConstants.FFLAGS_SZ.W)
  }.elsewhen(isMCFPRespValid) {
   relay_reg_fflags := relay_reg_fflags | vfun1.io.fflags
  }

//////////////////////////////////////exe regs assignment///////////////////////////////////


//////////////////////////////////////////execute state/////////////////////////////////////

  vcsr.io.vstartEn         := vstartEn
  vcsr.io.vstartIn         := vstartIn
  vcsr.io.reducevlEn       := vdmemreq.io.mem_vl_valid
  vcsr.io.reducedvl        := vdmemreq.io.mem_vl
  vcsr.io.enable           := eSigs.majFun === IsCSR && exe_reg_enable && !killx
  vcsr.io.killm            := ePipem && killm
  vcsr.io.src1Field        := eSigs.src1Field
  vcsr.io.dstField         := eSigs.dstField
  vcsr.io.fromXData1       := fromXData1
  vcsr.io.fromXData2       := fromXData2
  vcsr.io.vtypei           := Cat(eSigs.funct6(4,0), eSigs.vm, eSigs.src2Field)
  vcsr.io.csrType          := eSigs.funct6(5)

  vsplit.io.vs1Data        := exe_reg_vs1Data
  vsplit.io.vs2Data        := exe_reg_vs2Data
  vsplit.io.vdvs3Data      := exe_reg_vdvs3Data
  vsplit.io.v0Mask         := exe_reg_v0Mask
  vsplit.io.isSrc12SEW     := eSigs.isSrc12SEW
  vsplit.io.isSrc22SEW     := eSigs.isSrc22SEW
  vsplit.io.isVd2SEW       := eSigs.isVd2SEW
  vsplit.io.isFullMul      := eSigs.isFullMul
  vsplit.io.inStep         := inStep

  vm2e.io.isUnmasked       := eSigs.isUnmasked
  vm2e.io.isVd2SEW         := eSigs.isVd2SEW
  vm2e.io.addFun           := eSigs.addFun
  vm2e.io.isFullMul        := eSigs.isFullMul
  vm2e.io.vediv            := vediv
  vm2e.io.vsew             := vcsr.io.vsew
  vm2e.io.vlmul            := vcsr.io.vlmul
  vm2e.io.vl               := vcsr.io.vl
  vm2e.io.vstart           := vcsr.io.vstart
  vm2e.io.vm               := eSigs.vm
  vm2e.io.v0m              := vsplit.io.v0m
  vm2e.io.vs1m             := vsplit.io.vs1m

  vwidth.io.isSrc22SEW     := eSigs.isSrc22SEW
  vwidth.io.isVd2SEW       := eSigs.isVd2SEW
  vwidth.io.sign           := eSigs.sign
  vwidth.io.src1Typ        := eSigs.src1Typ
  vwidth.io.majFun         := eSigs.majFun
  vwidth.io.vm             := eSigs.vm
  vwidth.io.vlmul          := vcsr.io.vlmul
  vwidth.io.fromXData1     := Mux(exe_reg_enable, fromXData1, exe_reg_fromXData1)
  vwidth.io.src1Field      := eSigs.src1Field
  vwidth.io.vs1e           := vsplit.io.vs1e
  vwidth.io.vs2e           := vsplit.io.vs2e

  valc.io.vm2e_io_carryIn   := vm2e.io.carryIn
  valc.io.vm2e_io_v0merge   := vm2e.io.v0merge
  valc.io.vm2e_io_v0mul     := vm2e.io.v0mul
  valc.io.vm2e_io_v0maske   := vm2e.io.v0maske
  valc.io.vm2e_io_v0fen     := vm2e.io.v0fen
  valc.io.isFullMul         := eSigs.isFullMul
  valc.io.isSrc22SEW        := eSigs.isSrc22SEW
  valc.io.isVd2SEW          := eSigs.isVd2SEW
  valc.io.inStep            := inStep

//--------------------------------------------fix point modules---------------------------------------//
  val vadd_io_vsrc1e = (if(MULADD || QMULADD) 
                          Mux(eSigs.majFun === IsMulAdd, vmuldiv.io.resp.bits.vMulDivOut, vwidth.io.vsrc1e)
                        else 
                          vwidth.io.vsrc1e)
  val vadd_io_vsrc2e = (if(MULADD) 
                          Mux(eSigs.majFun === IsMulAdd && eSigs.prodAddWhat === Add_Vd, vsplit.io.vdvs3e, vwidth.io.vsrc2e)
                        else 
                          vwidth.io.vsrc2e)
  vadd.io.vsrc1e           := vadd_io_vsrc1e
  vadd.io.vsrc2e           := vadd_io_vsrc2e
  vadd.io.carryIn          := valc.io.carryIn
  vadd.io.addFun           := eSigs.addFun
  vadd.io.sign             := eSigs.sign
  vadd.io.vsew             := vcsr.io.vsew - vediv
  vadd.io.vxrm             := io.core.vcsr_vxrm
  vadd.io.isRND            := eSigs.isRND
  vadd.io.isSAT            := eSigs.isSAT

  vbit.io.vsrc1e           := vwidth.io.vsrc1e
  vbit.io.vsrc2e           := vwidth.io.vsrc2e
  vbit.io.bitFun           := eSigs.funct6(2,0)

  vshift.io.vsrc1t         := vwidth.io.vsrc1t
  vshift.io.vsrc2e         := vwidth.io.vsrc2e
  vshift.io.shiftFun       := eSigs.shiftFun
  vshift.io.isSrc22SEW     := eSigs.isSrc22SEW
  vshift.io.vxrm           := io.core.vcsr_vxrm
  vshift.io.isRND          := eSigs.isRND
  vshift.io.isSAT          := eSigs.isSAT
  vshift.io.sign           := eSigs.sign
  vshift.io.vsew           := vcsr.io.vsew - vediv

  vcmp.io.vsrc1e           := vwidth.io.vsrc1e
  vcmp.io.vsrc2e           := vwidth.io.vsrc2e
  vcmp.io.cmpFun           := eSigs.cmpFun
  vcmp.io.sign             := eSigs.sign
  vcmp.io.vlmul            := vcsr.io.vlmul

  vmerge.io.v0merge        := valc.io.v0merge
  vmerge.io.vsrc1e         := vwidth.io.vsrc1e
  vmerge.io.vsrc2e         := vwidth.io.vsrc2e
  vmerge.io.vsrc1f         := vfwidth.io.vsrc1f
  vmerge.io.vsrc2f         := vfwidth.io.vsrc2f

  val vmuldiv_io_vsrc2e = (if(MULADD || QMULADD)
                             Mux(eSigs.majFun === IsMulAdd && eSigs.prodAddWhat === Add_Vs2, 
                                 vsplit.io.vdvs3e, vwidth.io.vsrc2e)
                           else 
                             vwidth.io.vsrc2e)
  vmuldiv.io.kill            := false.B
  vmuldiv.io.vsew            := vcsr.io.vsew - vediv
  vmuldiv.io.isVd2SEW        := eSigs.isVd2SEW
  vmuldiv.io.isFullMul       := eSigs.isFullMul
  vmuldiv.io.sign            := eSigs.sign
  vmuldiv.io.vxrm            := io.core.vcsr_vxrm
  vmuldiv.io.isRND           := eSigs.isRND
  vmuldiv.io.isSAT           := eSigs.isSAT
  vmuldiv.io.req.valid       := isMCReqValid && (eSigs.majFun === IsMulDiv || eSigs.majFun === IsMulAdd)
  vmuldiv.io.req.bits.fn     := eSigs.mulFun
  vmuldiv.io.req.bits.dw     := 1.U(1.W)
  vmuldiv.io.req.bits.vsrc1e := vwidth.io.vsrc1e
  vmuldiv.io.req.bits.vsrc2e := vmuldiv_io_vsrc2e
  vmuldiv.io.req.bits.v0en   := valc.io.v0mul
  vmuldiv.io.resp.ready      := true.B

  vred.io.vsew             := vcsr.io.vsew
  vred.io.redFun           := eSigs.funct6(4,0)
  vred.io.enable           := isMCReqValid && eSigs.majFun === IsRed
  vred.io.firstELEN        := vState === inMCWait2 && !killw
  vred.io.vsrc1e           := vwidth.io.vsrc1e
  vred.io.vsrc2e           := vwidth.io.vsrc2e
  vred.io.vdvs3e           := vsplit.io.vdvs3e
  vred.io.v0e              := valc.io.v0en

  vrgather.io.vediv        := vediv
  vrgather.io.vsew         := vcsr.io.vsew
  vrgather.io.vlmul        := vcsr.io.vlmul
  vrgather.io.src1Typ      := eSigs.src1Typ
  vrgather.io.fromXData1   := Mux(exe_reg_enable, fromXData1, exe_reg_fromXData1)
  vrgather.io.vsrc1e       := vwidth.io.vsrc1e
  vrgather.io.vsrc2e       := vsplit.io.vs2E
//--------------------------------------------fix-point modules---------------------------------------//

//-----------------------------------------floating-point modules-------------------------------------//
  vfwidth.io.reqValid      := isMCReqValid
  vfwidth.io.isSrc12SEW    := eSigs.isSrc12SEW
  vfwidth.io.isSrc22SEW    := eSigs.isSrc22SEW
  vfwidth.io.isVd2SEW      := eSigs.isVd2SEW
  vfwidth.io.src1Typ       := eSigs.src1Typ
  vfwidth.io.majFun        := eSigs.majFun
  vfwidth.io.fcvtFun       := eSigs.src1Field
  vfwidth.io.frm           := io.core.vcsr_frm
  vfwidth.io.fromXData1    := Mux(exe_reg_enable, fromXData1, exe_reg_fromXData1)
  vfwidth.io.fromFData     := exe_reg_fromFData //using fp scalar insts are treat as multi cycle insts
  vfwidth.io.vs1e          := vsplit.io.vs1e
  vfwidth.io.vs2e          := vsplit.io.vs2e
  vfwidth.io.vdvs3e        := vsplit.io.vdvs3e

  val isFMAReq = eSigs.majFun === IsFMA  || 
                 eSigs.majFun === IsFRed && 
                 (eSigs.funct6(4,0) === RedFun_FSum  || 
                  eSigs.funct6(4,0) === RedFun_FOSum || 
                  eSigs.funct6(4,0) === RedFun_FWSum || 
                  eSigs.funct6(4,0) === RedFun_FWOSum)
  vfma.io.kill             := false.B
  vfma.io.vsew             := vcsr.io.vsew - vediv
  vfma.io.isVd2SEW         := eSigs.isVd2SEW
  vfma.io.prodAddWhat      := eSigs.prodAddWhat
  vfma.io.redsum           := eSigs.majFun === IsFRed
  vfma.io.firstElen        := vfwidth.io.respValid && vState === inMCPart0w
  vfma.io.req.valid        := vfwidth.io.respValid && isFMAReq
  vfma.io.req.bits.fn.op   := eSigs.fmaFun
  vfma.io.req.bits.fn.rm   := io.core.vcsr_frm
  vfma.io.req.bits.vsrc1e  := vfwidth.io.vsrc1f
  vfma.io.req.bits.vsrc2e  := vfwidth.io.vsrc2f
  vfma.io.req.bits.vdvs3   := vfwidth.io.vdvs3f
  vfma.io.req.bits.v0en    := valc.io.v0fen

  vfdiv.io.kill            := false.B
  vfdiv.io.vsew            := vcsr.io.vsew - vediv
  vfdiv.io.req.valid       := vfwidth.io.respValid && (eSigs.majFun === IsFDiv)
  vfdiv.io.req.bits.fn.op  := eSigs.funct6(1,0)
  vfdiv.io.req.bits.fn.rm  := io.core.vcsr_frm
  vfdiv.io.req.bits.vsrc1e := vfwidth.io.vsrc1f
  vfdiv.io.req.bits.vsrc2e := vfwidth.io.vsrc2f
  vfdiv.io.req.bits.v0en   := valc.io.v0fen
  vfdiv.io.resp.ready      := true.B

  val isFtoIReq = eSigs.majFun === IsFCvt && eSigs.src1Field(2,0) <= 1.U(3.W)
  vf2i.io.req.valid        := vfwidth.io.respValid && isFtoIReq
  vf2i.io.vsew             := vcsr.io.vsew - vediv
  vf2i.io.req.bits.fn.op   := eSigs.src1Field
  vf2i.io.req.bits.fn.rm   := io.core.vcsr_frm
  vf2i.io.req.bits.vsrc2e  := vfwidth.io.vsrc2f

  val isItoFReq = eSigs.majFun === IsFCvt && 
                  (1.U(3.W) < eSigs.src1Field(2,0) && eSigs.src1Field(2,0) < 4.U(3.W))
  vi2f.io.req.valid        := vfwidth.io.respValid && isItoFReq
  vi2f.io.vsew             := vcsr.io.vsew - vediv
  vi2f.io.req.bits.fn.op   := eSigs.src1Field
  vi2f.io.req.bits.fn.rm   := io.core.vcsr_frm
  vi2f.io.req.bits.vsrc2e  := vwidth.io.vsrc2e
  vi2f.io.req.bits.v0en    := valc.io.v0fen

  val isFCmpReq = eSigs.majFun === IsFCmp || 
                  eSigs.majFun === IsFRed && 
                  (eSigs.funct6(4,0) === RedFun_FMin || 
                   eSigs.funct6(4,0) === RedFun_FMax)
  vfcmp.io.req.valid       := vfwidth.io.respValid && isFCmpReq
  vfcmp.io.req.bits.fn.op  := eSigs.cmpFun
  vfcmp.io.req.bits.fn.rm  := io.core.vcsr_frm
  vfcmp.io.vsew            := vcsr.io.vsew
  vfcmp.io.vlmul           := vcsr.io.vlmul
  vfcmp.io.redsum          := eSigs.majFun === IsFRed
  vfcmp.io.firstElen       := vfwidth.io.respValid && vState === inMCPart0w
  vfcmp.io.req.bits.vsrc1e := vfwidth.io.vsrc1f
  vfcmp.io.req.bits.vsrc2e := vfwidth.io.vsrc2f
  vfcmp.io.req.bits.v0en   := valc.io.v0fen

  vfclass.io.vsrc2f        := vfwidth.io.vsrc2f

  vfsgnj.io.vsrc1f         := vfwidth.io.vsrc1f
  vfsgnj.io.vsrc2f         := vfwidth.io.vsrc2f
  vfsgnj.io.fsgnjFun       := eSigs.funct6(1,0)

  vfun1.io.isSEWOut        := eSigs.isSEWOut
  vfun1.io.isMLENOut       := eSigs.isMLENOut
  vfun1.io.isVd2SEW        := eSigs.isVd2SEW
  vfun1.io.majFun          := eSigs.majFun
  vfun1.io.fcvtFun         := eSigs.src1Field
  vfun1.io.isFullMul       := eSigs.isFullMul
  vfun1.io.isFMAReq        := isFMAReq
  vfun1.io.isFCmpReq       := isFCmpReq
  vfun1.io.vsew            := vcsr.io.vsew - vediv
  vfun1.io.vAddCarry       := vadd.io.vAddCarry
  vfun1.io.vAddSum         := vadd.io.vAddSum
  vfun1.io.vBitwise        := vbit.io.vBitwise
  vfun1.io.vShiftOut       := vshift.io.vShiftOut
  vfun1.io.vCmpOut         := vcmp.io.vCmpOut
  vfun1.io.vMinMaxOut      := vcmp.io.vMinMaxOut
  vfun1.io.vMergeOut       := vmerge.io.vMergeOut
  vfun1.io.vMulDivOut      := vmuldiv.io.resp.bits.vMulDivOut
  vfun1.io.vRedOut         := vred.io.vRedOut.bits
  vfun1.io.vRGatherOut     := vrgather.io.vRGatherOut
  vfun1.io.vFClassOut      := vfclass.io.vFClassOut
  vfun1.io.vFtoIOut        := vf2i.io.resp.bits.vFPToIntOut
  vfun1.io.vItoFOut        := vi2f.io.resp.bits.vIntToFPOut
  vfun1.io.vsrc2f          := vfwidth.io.vsrc2f
  vfun1.io.vFMAOut         := vfma.io.resp.bits.vFMAOut
  vfun1.io.vFDivOut        := vfdiv.io.resp.bits.vFDivSqrtOut
  vfun1.io.vFCmpOut        := vfcmp.io.resp.bits.vFCmpOut
  vfun1.io.vFMinMaxOut     := vfcmp.io.resp.bits.vFMinMaxCmpOut
  vfun1.io.vFMergeOut      := vmerge.io.vFMergeOut
  vfun1.io.vFSgnJOut       := vfsgnj.io.vFSgnJOut
  vfun1.io.v0fen           := valc.io.v0fexc
  vfun1.io.vFMAExc         := vfma.io.resp.bits.exc
  vfun1.io.vFDivExc        := vfdiv.io.resp.bits.exc
  vfun1.io.vFCmpExc        := vfcmp.io.resp.bits.exc
  vfun1.io.vItoFExc        := vi2f.io.resp.bits.exc
  vfun1.io.vFtoIExc        := vf2i.io.resp.bits.exc
  vfun1.io.vFtoFExc        := vfwidth.io.vFtoFExc

  vpopc.io.vs2m            := vsplit.io.vs2m
  vpopc.io.v0m             := vsplit.io.v0m
  vpopc.io.vsew            := vcsr.io.vsew
  vpopc.io.vlmul           := vcsr.io.vlmul
  vpopc.io.vm              := eSigs.vm

  vmidx.io.vsew            := vcsr.io.vsew
  vmidx.io.vlmul           := vcsr.io.vlmul
  vmidx.io.vm              := eSigs.vm
  vmidx.io.minIdxFun       := eSigs.src1Field(1,0)
  vmidx.io.vs2m            := vsplit.io.vs2m
  vmidx.io.v0m             := vsplit.io.v0m

  viota.io.req.valid       := eSigs.majFun === IsIota && vState === inMCFull
  viota.io.req.bits.vs2    := exe_reg_vs2Data(VLEN-1,0)
  viota.io.req.bits.v0     := exe_reg_v0Mask
  viota.io.resp.ready      := true.B
  viota.io.vsew            := vcsr.io.vsew
  viota.io.vlmul           := vcsr.io.vlmul
  viota.io.vm              := eSigs.vm

  vmv.io.fromXData1        := fromXData1
  vmv.io.fromFData         := exe_reg_fromFData
  vmv.io.vs2E              := vsplit.io.vs2E
  vmv.io.vdvs3E            := vsplit.io.vdvs3E
  vmv.io.vsew              := vcsr.io.vsew

  vslide.io.vsrc1e         := vwidth.io.vsrc1e
  vslide.io.vs2Data        := exe_reg_vs2Data
  vslide.io.vl             := vcsr.io.vl
  vslide.io.vsew           := vcsr.io.vsew
  vslide.io.vlmul          := vcsr.io.vlmul
  vslide.io.majFun         := eSigs.majFun
  vslide.io.slideFun       := eSigs.funct6(0)

  vcompress.io.vs1m        := vm2e.io.vs1zipm
  vcompress.io.vs2e        := vsplit.io.vs2E
  vcompress.io.vdvs3e      := vsplit.io.vdvs3E
  vcompress.io.enable      := eSigs.majFun === IsZip && vState === inMCFull
  vcompress.io.kill        := false.B //TODO release value
  vcompress.io.vsew        := vcsr.io.vsew
  vcompress.io.vlmul       := vcsr.io.vlmul

  val vSEWFullData = Cat(relay_reg_Data7, relay_reg_Data6, relay_reg_Data5, relay_reg_Data4, 
                         relay_reg_Data3, relay_reg_Data2, relay_reg_Data1, relay_reg_Data0)
  val vSEWHalfData = Cat(relay_reg_Data7(ELEN/2-1,0), relay_reg_Data6(ELEN/2-1,0), relay_reg_Data5(ELEN/2-1,0), 
                         relay_reg_Data4(ELEN/2-1,0), relay_reg_Data3(ELEN/2-1,0), relay_reg_Data2(ELEN/2-1,0), 
                         relay_reg_Data1(ELEN/2-1,0), relay_reg_Data0(ELEN/2-1,0))
  vfun2.io.majFun          := eSigs.majFun
  vfun2.io.vlmul           := vcsr.io.vlmul
  vfun2.io.vSEWData        := Mux(!eSigs.isSrc12SEW && eSigs.isSrc22SEW && !eSigs.isVd2SEW, vSEWHalfData, vSEWFullData)
  vfun2.io.vIotaOut        := viota.io.resp.bits.vIotaOut
  vfun2.io.vIndexOut       := vidx.io.vIndexOut
  vfun2.io.vSlideOut       := vslide.io.vSlideOut
  vfun2.io.vCompressOut    := vcompress.io.vCompressOut
  vfun2.io.vFromXOut       := vmv.io.vFromXOut
  vfun2.io.vFromFOut       := vmv.io.vFromFOut
  vfun2.io.vMSetOut        := vmidx.io.vMSetOut
  vfun2.io.vMBitwise       := vbit.io.vMBitwise
  vfun2.io.vPopcOut        := vpopc.io.vPopcOut
  vfun2.io.vMinIndex       := vmidx.io.vMinIndex.asUInt
  vfun2.io.vXToXData       := vmv.io.vXToXData
  vfun2.io.vFToXData       := vmv.io.vFToXData
  vfun2.io.vFToFData       := vmv.io.vFToFData

  vmask.io.vsrc1e          := vwidth.io.vsrc1e
  vmask.io.v0maske         := vm2e.io.v0maske
  vmask.io.vSEWOut         := vfun2.io.vSEWOut
  vmask.io.vdvs3e          := vsplit.io.vdvs3E
  vmask.io.v0maskm         := vm2e.io.v0maskm
  vmask.io.vMLENOut        := vfun2.io.vMLENOut
  vmask.io.vdm             := vsplit.io.vdm
  vmask.io.vs2Data         := exe_reg_vs2Data
  vmask.io.majFun          := eSigs.majFun
  vmask.io.isFullMul       := eSigs.isFullMul
  vmask.io.slideFun        := eSigs.funct6(0)
  vmask.io.isVd2SEW        := eSigs.isVd2SEW
  vmask.io.vediv           := vediv
  vmask.io.vsew            := vcsr.io.vsew
  vmask.io.vlmul           := vcsr.io.vlmul

//////////////////////////////////////////execute state/////////////////////////////////////


/////////////////////////////////////////mem state//////////////////////////////////////////

  vdmemwidth.io.vsew       := vcsr.io.vsew
  vdmemwidth.io.vs2e       := vsplit.io.vs2E

  vdmemreq.io.fromXData1   := Mux(exe_reg_enable, fromXData1, exe_reg_fromXData1)
  vdmemreq.io.fromXData2   := Mux(exe_reg_enable, fromXData2, exe_reg_fromXData2)
  vdmemreq.io.vs2toXlen    := vdmemwidth.io.vs2toXlen
  vdmemreq.io.vdvs3e8      := vsplit.io.vdvs3E.e8
  vdmemreq.io.vdvs3e16     := vsplit.io.vdvs3E.e16
  vdmemreq.io.vdvs3e32     := vsplit.io.vdvs3E.e32
  vdmemreq.io.vdvs3e64     := vsplit.io.vdvs3E.e64
  vdmemreq.io.v0m1         := vsplit.io.v0m.m1
  vdmemreq.io.v0m2         := vsplit.io.v0m.m2
  vdmemreq.io.v0m4         := vsplit.io.v0m.m4
  vdmemreq.io.v0m8         := vsplit.io.v0m.m8
  vdmemreq.io.v0m16        := vsplit.io.v0m.m16
  vdmemreq.io.v0m32        := vsplit.io.v0m.m32
  vdmemreq.io.v0m64        := vsplit.io.v0m.m64
  vdmemreq.io.vm           := eSigs.vm
  vdmemreq.io.vsew         := vcsr.io.vsew
  vdmemreq.io.vlmul        := vcsr.io.vlmul
  vdmemreq.io.vl           := vcsr.io.vl
  vdmemreq.io.majFun       := eSigs.majFun
  vdmemreq.io.addrMode     := eSigs.funct6(1,0)
  vdmemreq.io.ldstWidth    := eSigs.ldstWidth
  vdmemreq.io.nFields      := eSigs.funct6(5,3)
  vdmemreq.io.lumop        := eSigs.src2Field
  vdmemreq.io.amoop        := eSigs.funct6(5,1)
  vdmemreq.io.reqReady     := io.reqReady
  vdmemreq.io.s2Nack       := io.s2Nack
  vdmemreq.io.enable       := mPipew && !killw && vcsr.io.vl =/= 0.U(XLEN.W)
  vdmemreq.io.respValid    := io.respValid
  vdmemreq.io.respTag      := io.respTag
  vdmemreq.io.killm        := false.B
  vdmemreq.io.eret         := io.core.eret
  vdmemreq.io.respS2Xcpt   := io.respS2Xcpt
  vdmemreq.io.mem_xcpt_resp:= io.core.vxcpt_imprecise_resp
  vdmemreq.io.killx        := false.B
  vdmemreq.io.killw        := false.B
  vdmemreq.io.wd           := eSigs.funct6(0)
 
  vdmemresp.io.lumop       := eSigs.src2Field
  vdmemresp.io.vsew        := vcsr.io.vsew
  vdmemresp.io.vlmul       := vcsr.io.vlmul
  vdmemresp.io.vl          := vcsr.io.vl
  vdmemresp.io.mEnable     := mPipew && !killw && vcsr.io.vl =/= 0.U(XLEN.W)
  vdmemresp.io.wd          := eSigs.funct6(0)
  vdmemresp.io.vm          := eSigs.vm
  vdmemresp.io.sign        := eSigs.sign
  vdmemresp.io.majFun      := eSigs.majFun
  vdmemresp.io.nFields     := eSigs.funct6(5,3)
  vdmemresp.io.stopLoad    := vdmemreq.io.stopLoad
  vdmemresp.io.killm       := false.B
  vdmemresp.io.faultFirst  := faultFirst
  vdmemresp.io.mDone       := vdmemreq.io.done
  vdmemresp.io.respValid   := io.respValid
  vdmemresp.io.respTag     := io.respTag
  vdmemresp.io.respSize    := io.respSize
  vdmemresp.io.respHasData := io.respHasData
  vdmemresp.io.respData    := io.respData
  vdmemresp.io.vdvs3e      := vsplit.io.vdvs3E
  vdmemresp.io.v0e         := vm2e.io.v0merge
//  vdmemresp.io.v0m         := vsplit.io.v0m


/////////////////////////////////////////mem state//////////////////////////////////////////

//////////////////////////////////////////wb state//////////////////////////////////////////

  val alwben = vcsr.io.vl =/= 0.U(XLEN.W) && vcsr.io.vstart < vcsr.io.vl && 
               wSigs.majFun =/= IsCopy && wb_reg_enable
  val ldwben = vcsr.io.vl =/= 0.U(XLEN.W) && vcsr.io.vstart < vcsr.io.vl && vRecvOver && vState === inVLoad
  val cpwben = (if(COPY) wSigs.majFun === IsCopy && wb_reg_enable else false.B)

  val veIn    = Wire(UInt((8*VLEN).W))
  val isVeOut = (alwben || cpwben) && wSigs.isSEWOut
  val isVmOut = alwben && wSigs.isMLENOut
  val isVRecv = ldwben && eSigs.isSEWOut
//             && (mem_reg_ctrlSigs.majFun === IsLoad || mem_reg_ctrlSigs.majFun === IsAMO)

  val wbvlmul = (vcsr.io.vlmul + wSigs.isVd2SEW + wSigs.isFullMul) & 3.U(2.W)

  lazy val alwen1 = alwben && (wSigs.isSEWOut || wSigs.isMLENOut)
  lazy val alwen2 = alwben && wSigs.isSEWOut && wbvlmul >= DoubReg
  lazy val alwen3 = alwben && wSigs.isSEWOut && wbvlmul >= QuadReg
  lazy val alwen4 = alwben && wSigs.isSEWOut && wbvlmul >= QuadReg
  lazy val alwen5 = alwben && wSigs.isSEWOut && wbvlmul === OctuReg
  lazy val alwen6 = alwben && wSigs.isSEWOut && wbvlmul === OctuReg
  lazy val alwen7 = alwben && wSigs.isSEWOut && wbvlmul === OctuReg
  lazy val alwen8 = alwben && wSigs.isSEWOut && wbvlmul === OctuReg

  val nFields = ((eSigs.funct6(5,3) +& 1.U(3.W)) << vcsr.io.vlmul)(3,0)
  lazy val ldwen1 = ldwben && eSigs.isSEWOut
  lazy val ldwen2 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul >= DoubReg || nFields >= 2.U(4.W))
  lazy val ldwen3 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul >= QuadReg || nFields >= 3.U(4.W))
  lazy val ldwen4 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul >= QuadReg || nFields >= 4.U(4.W))
  lazy val ldwen5 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul === OctuReg || nFields >= 5.U(4.W))
  lazy val ldwen6 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul === OctuReg || nFields >= 6.U(4.W))
  lazy val ldwen7 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul === OctuReg || nFields >= 7.U(4.W))
  lazy val ldwen8 = ldwben && eSigs.isSEWOut && (vcsr.io.vlmul === OctuReg || nFields === 8.U(4.W))

  lazy val cpwen1 = cpwben
  lazy val cpwen2 = cpwben && wSigs.src1Field >= TwoField
  lazy val cpwen3 = cpwben && wSigs.src1Field >= FouField
  lazy val cpwen4 = cpwben && wSigs.src1Field >= FouField
  lazy val cpwen5 = cpwben && wSigs.src1Field === EigField
  lazy val cpwen6 = cpwben && wSigs.src1Field === EigField
  lazy val cpwen7 = cpwben && wSigs.src1Field === EigField
  lazy val cpwen8 = cpwben && wSigs.src1Field === EigField

  val wen1 = alwen1 || ldwen1 || cpwen1
  val wen2 = alwen2 || ldwen2 || cpwen2
  val wen3 = alwen3 || ldwen3 || cpwen3
  val wen4 = alwen4 || ldwen4 || cpwen4
  val wen5 = alwen5 || ldwen5 || cpwen5
  val wen6 = alwen6 || ldwen6 || cpwen6
  val wen7 = alwen7 || ldwen7 || cpwen7
  val wen8 = alwen8 || ldwen8 || cpwen8

  veIn := MuxCase(0.U((8*VLEN).W),
            Array(isVeOut -> wb_reg_veOut,
                  isVmOut -> Cat(0.U((7*VLEN).W), wb_reg_vmOut),
                  isVRecv -> vdmemresp.io.vRecvOut))

  val vaddr = Mux(alwben, wSigs.dstField, eSigs.dstField)

  when(wen1) { vRegFile(vaddr) := veIn(VLEN-1, 0) }
  when(wen2) { vRegFile(vaddr + 1.U) := veIn(2*VLEN-1, VLEN) }
  when(wen3) { vRegFile(vaddr + 2.U) := veIn(3*VLEN-1, 2*VLEN) }
  when(wen4) { vRegFile(vaddr + 3.U) := veIn(4*VLEN-1, 3*VLEN) }
  when(wen5) { vRegFile(vaddr + 4.U) := veIn(5*VLEN-1, 4*VLEN) }
  when(wen6) { vRegFile(vaddr + 5.U) := veIn(6*VLEN-1, 5*VLEN) }
  when(wen7) { vRegFile(vaddr + 6.U) := veIn(7*VLEN-1, 6*VLEN) }
  when(wen8) { vRegFile(vaddr + 7.U) := veIn(8*VLEN-1, 7*VLEN) }

//////////////////////////////////////////wb state//////////////////////////////////////////


/////////////////////////////////////////control path///////////////////////////////////////

  val exevlmul = (vcsr.io.vlmul + 
                  (eSigs.isVd2SEW || eSigs.isSrc22SEW) + 
                  eSigs.isFullMul) & 3.U(2.W)
  def killEXE = exe_reg_enable && killx || ePipem && killm || ePipew && killw
  def killMEM = exe_reg_enable && killx || mPipem && killm || mPipew && killw
  def killILL = upload_reg_illInst0 && killx || upload_reg_illInst1 && killm || upload_reg_illInst2
  switch(vState) {
    is(idle) {
      when(illInst) {
        vState := inExpt
      }.elsewhen(llInst && isLdInst) {
        vState := inVLoad
      }.elsewhen(llInst && isStInst) {
        vState := inVStore
      }.elsewhen(llInst && isOCFullInst) {
        vState := inOCFull
      }.elsewhen(llInst && isMCFullInst) {
        vState := inMCWait0  //TODO can kill module inside process
      }.elsewhen(llInst && isOCPartInst) {
        vState := inOCPart0
      }.elsewhen(llInst && isMCPartInst) {
        vState := inMCWait0
      }
    }
    is(inVLoad) {
      when(vRecvOver || killMEM || faultFirst || vcsr.io.vl === 0.U(XLEN.W)) {
        vState := idle
      }
    }
    is(inVStore) {
      when(vSendOver || killMEM || faultFirst || vcsr.io.vl === 0.U(XLEN.W)) {
        vState := idle
      }
    }
    is(inExpt) {
      when(killILL) {
        vState := idle
      }
    }
    is(inOCFull) {
      when(!coreFire) {
        vState := idle
      }.elsewhen(illInst) {
        vState := inExpt
      }.elsewhen(killx || llInst && isLdInst) {
        vState := inVLoad
      }.elsewhen(killx || llInst && isStInst) {
        vState := inVStore
      }.elsewhen(killx || llInst && isMCFullInst) {
        vState := inMCWait0
      }.elsewhen(killx || llInst && isOCPartInst) {
        vState := inOCPart0
      }.elsewhen(killx || llInst && isMCPartInst) {
        vState := inMCWait0
      }
    }
    is(inMCFull) {
      when(vcompress.io.respValid || viota.io.resp.valid || 
           eSigs.majFun === IsFMv && eSigs.isSEWOut || killEXE) {
        vState := idle
      }
    }
    is(inOCPart0) {
      when(exe_reg_enable && killx) {
        vState := idle
      }.elsewhen(exevlmul === SingReg) {
        vState := finish
      }.otherwise {
        vState := inOCPart1
      }
    }
    is(inOCPart1) {
      when(ePipem && killm) {
        vState := idle
      }.elsewhen(exevlmul === DoubReg) {
        vState := finish
      }.otherwise {
        vState := inOCPart2
      }
    }
    is(inOCPart2) {
      when(ePipew && killw) {
        vState := idle
      }.otherwise {
        vState := inOCPart3
      }
    }
    is(inOCPart3) {
      when(exevlmul === QuadReg) {
        vState := finish
      }.otherwise {
        vState := inOCPart4
      }
    }
    is(inOCPart4) {
      vState := inOCPart5
    }
    is(inOCPart5) {
      vState := inOCPart6
    }
    is(inOCPart6) {
      vState := inOCPart7
    }
    is(inOCPart7) {
      vState := finish
    }
    is(inMCWait0) {
      when(exe_reg_enable && killx) {
        vState := idle
      }.otherwise {
        vState := inMCWait1
      }
    }
    is(inMCWait1) {
      when(ePipem && killm || eSigs.majFun === IsCSR) {
        vState := idle
      }.otherwise {
        vState := inMCWait2
      }
    }
    is(inMCWait2) {
      when(ePipew && killw) {
        vState := idle
      }.elsewhen(inMCFullState) {
        vState := inMCFull
      }.otherwise {
        vState := inMCPart0w
      }
    }
    is(inMCPart0w) {
      when(exevlmul === SingReg && isMCRespValid) {
        vState := finish
      }.elsewhen(exevlmul =/= SingReg && isMCRespValid) {
        vState := inMCPart1p
      }
    }
    is(inMCPart1p) {
      vState := inMCPart1w
    }
    is(inMCPart1w) {
      when(exevlmul === DoubReg && isMCRespValid) {
        vState := finish
      }.elsewhen(exevlmul =/= DoubReg && isMCRespValid) {
        vState := inMCPart2p
      }
    }
    is(inMCPart2p) {
      vState := inMCPart2w
    }
    is(inMCPart2w) {
      when(isMCRespValid) {
        vState := inMCPart3p
      }
    }
    is(inMCPart3p) {
      vState := inMCPart3w
    }
    is(inMCPart3w) {
      when(exevlmul === QuadReg && isMCRespValid) {
        vState := finish
      }.elsewhen(exevlmul =/= QuadReg && isMCRespValid) {
        vState := inMCPart4p
      }
    }
    is(inMCPart4p) {
      vState := inMCPart4w
    }
    is(inMCPart4w) {
      when(isMCRespValid) {
        vState := inMCPart5p
      }
    }
    is(inMCPart5p) {
      vState := inMCPart5w
    }
    is(inMCPart5w) {
      when(isMCRespValid) {
        vState := inMCPart6p
      }
    }
    is(inMCPart6p) {
      vState := inMCPart6w
    }
    is(inMCPart6w) {
      when(isMCRespValid) {
        vState := inMCPart7p
      }
    }
    is(inMCPart7p) {
      vState := inMCPart7w
    }
    is(inMCPart7w) {
      when(isMCRespValid) {
        vState := finish
      }
    }
    is(finish) {
      vState := idle
    }
  }

}
