/*
inst and data input order: 
	first a CSR inst for setting VPU CSR environment,
	then two load insts with incresing data sequence for inputting data, 
	then sixteen arithmetic or logic insts for data changing,
        final a store inst for data output;
print insts' name, insts' fields and data for better understanding waveform.
*/

package vpu

import chisel3.iotesters._
import chisel3.util._
import java.io.File
import java.io.PrintWriter

class VPUTest(c: VPU_64V64E1L) extends PeekPokeTester(c) {
  val writer = new PrintWriter(new File("/home/liangzh/Desktop/ChiselProjects/Vector/src/test/scala/VPU/VPUResult"))

/////////////////////////////////environment setting////////////////////////////
  //set CSR and initial value for vector regfile
  val vsew       = rnd.nextInt(log2Ceil(c.VLEN / 8))
  val vlmul      = rnd.nextInt(log2Ceil(c.LMULMAX) + 1)
  val vtype      = BigInt(vsew << 2 | vlmul)
  val SEW        = 8 << vsew
  val LMUL       = 1 << vlmul
  val VLMAX      = LMUL * c.VLEN / SEW
  val MLEN       = SEW / LMUL
  val AVL        = rnd.nextInt(2*VLMAX+1)
  val vl = if(rs1 == "00000") VLMAX 
           else if(AVL <= VLMAX) AVL
           else if(AVL < 2*VLMAX) AVL/2 + 1
           else VLMAX
  val atomVal    = rnd.nextInt(1 << 8)
  val atomValStr = "0"*(2-atomVal.toHexString.length) + atomVal.toHexString
  val elemValStr = ("0"*(2-atomVal.toHexString.length) + atomVal.toHexString)*(1 << vsew)
  val elemVal    = BigInt(elemValStr, 16)

  //print environment setting
  writer.println("VLEN  -- the number of bits in a vector register       = " + c.VLEN)
  writer.println("XLEN  -- the number of bits in a Rocket GPR            = " + c.XLEN)
  writer.println()
  writer.println("VLMAX -- maximum number of elements in a calculation   = " + VLMAX)
  writer.println("AVL   -- application vector length                     = " + AVL)
  writer.println("vl    -- actual vector length                          = " + vl)
  writer.println()
  writer.println("vsew  -- dynamic standard width(SEW) encoding          = " + vsew)
  writer.println("SEW   -- dynamic standard width                        = " + SEW)
  writer.println("vlmul -- vector register grouping encoding             = " + vlmul)
  writer.println("LMUL  -- vector register grouping                      = " + LMUL)
  writer.println()
  writer.println("MLEN  -- the size of each mask element in bits         = " + MLEN)
  writer.println()
  writer.println("atomic value(Hex) -- to construct load input values    = " + atomValStr)
  writer.println("element value(Hex) -- the first load input value       = " + elemValStr)
  writer.println()
  writer.println()
/////////////////////////////////environment setting////////////////////////////

  def VTYPEI: String = "0"*(11-vtype.toString(2).length)+vtype.toString(2)
  def vm: String = BigInt(1, scala.util.Random).toString(2)
  def VlmulRF:  String = { val rf = (BigInt(5, scala.util.Random) & (32 - LMUL)).toString(2); "0"*(5-rf.length)+rf }
  def SingleRF: String = { val rf = BigInt(5, scala.util.Random).toString(2); "0"*(5-rf.length)+rf }
  def VmlenRF(start: BigInt): String = { val rf = (BigInt(vlmul, scala.util.Random) + start).toString(2); "0"*(5-rf.length)+rf }

  val vtypei = VTYPEI
  val ldDstField = Array(VlmulRF, VlmulRF)
  val ldForVs1 = ldDstField(0)
  val ldForVs2 = ldDstField(1)
  val (rs1, rs2) = (SingleRF, SingleRF)
  val (vs1m, vs2m) = (VmlenRF(BigInt(ldDstField(0), 2)), VmlenRF(BigInt(ldDstField(1), 2)))
  val (vs1s, vs2s) = (VmlenRF(BigInt(ldDstField(0), 2)), VmlenRF(BigInt(ldDstField(1), 2)))
  val (rd, vde, vdm, vds) = (SingleRF, VlmulRF, SingleRF, SingleRF)
  val (vs2e, vs3e) = (VlmulRF, VlmulRF)
  val imm = SingleRF

///////////////////////////instructions name-code pairs/////////////////////////
  //AL Insts
  def VADD_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("000000"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VADD_VV     ")
  def VADD_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000000"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VADD_VX     ")
  def VADD_VI     (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("000000"+vm +vs2e   +imm    +"011"+vde+"1010111", "VADD_VI     ")
  def VSUB_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("000010"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VSUB_VV     ")
  def VSUB_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000010"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VSUB_VX     ")
  def VRSUB_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000011"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VRSUB_VX    ")
  def VRSUB_VI    (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("000011"+vm +vs2e   +imm    +"011"+vde+"1010111", "VRSUB_VI    ")
  val addInsts = Array(VADD_VV(), VADD_VX(), VADD_VI(), 
                       VSUB_VV(), VSUB_VX(), 
                       VRSUB_VX(), VRSUB_VI())

  def VWADDU_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110000"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWADDU_VV   ")
  def VWADDU_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110000"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWADDU_VX   ")
  def VWADD_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110001"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWADD_VV    ")
  def VWADD_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110001"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWADD_VX    ")
  def VWSUBU_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110010"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWSUBU_VV   ")
  def VWSUBU_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110010"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWSUBU_VX   ")
  def VWSUB_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110011"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWSUB_VV    ")
  def VWSUB_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110011"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWSUB_VX    ")
  def VWADDU_WV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110100"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWADDU_WV   ")
  def VWADDU_WX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110100"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWADDU_WX   ")
  def VWADD_WV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110101"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWADD_WV    ")
  def VWADD_WX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110101"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWADD_WX    ")
  def VWSUBU_WV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110110"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWSUBU_WV   ")
  def VWSUBU_WX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110110"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWSUBU_WX   ")
  def VWSUB_WV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("110111"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWSUB_WV    ")
  def VWSUB_WX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("110111"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWSUB_WX    ")
  val waddInsts = Array(VWADDU_VV(), VWADDU_VX(), VWADD_VV(), VWADD_VX(), 
                        VWSUBU_VV(), VWSUBU_VX(), VWSUB_VV(), VWSUB_VX(), 
                        VWADDU_WV(), VWADDU_WX(), VWADD_WV(), VWADD_WX(), 
                        VWSUBU_WV(), VWSUBU_WX(), VWSUB_WV(), VWSUB_WX())

  def VADC_VVM    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1)                  = ("010000"+"1"+vs2e   +vs1e   +"000"+vde+"1010111", "VADC_VVM    ")
  def VMADC_VVM   (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1)                  = ("010001"+"1"+vs2e   +vs1e   +"000"+vdm+"1010111", "VMADC_VVM   ")
  def VADC_VXM    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1)                       = ("010000"+"1"+vs2e   +rs1    +"100"+vde+"1010111", "VADC_VXM    ")
  def VMADC_VXM   (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1)                       = ("010001"+"1"+vs2e   +rs1    +"100"+vdm+"1010111", "VMADC_VXM   ")
  def VADC_VIM    (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm)                       = ("010000"+"1"+vs2e   +imm    +"011"+vde+"1010111", "VADC_VIM    ")
  def VMADC_VIM   (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm)                       = ("010001"+"1"+vs2e   +imm    +"011"+vdm+"1010111", "VMADC_VIM   ")
  def VSBC_VVM    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1)                  = ("010010"+"1"+vs2e   +vs1e   +"000"+vde+"1010111", "VSBC_VVM    ")
  def VMSBC_VVM   (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1)                  = ("010011"+"1"+vs2e   +vs1e   +"000"+vdm+"1010111", "VMSBC_VVM   ")
  def VSBC_VXM    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1)                       = ("010010"+"1"+vs2e   +rs1    +"100"+vde+"1010111", "VSBC_VXM    ")
  def VMSBC_VXM   (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1)                       = ("010011"+"1"+vs2e   +rs1    +"100"+vdm+"1010111", "VMSBC_VXM   ")
  val adcInsts = Array(VADC_VVM(), VMADC_VVM(), VADC_VXM(), VMADC_VXM(), VADC_VIM(), VMADC_VIM(), 
                       VSBC_VVM(), VMSBC_VVM(), VSBC_VXM(), VMSBC_VXM())

  def VAND_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("001001"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VAND_VV     ")
  def VAND_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("001001"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VAND_VX     ")
  def VAND_VI     (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("001001"+vm +vs2e   +imm    +"011"+vde+"1010111", "VAND_VI     ")
  def VOR_VV      (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("001010"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VOR_VV      ")
  def VOR_VX      (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("001010"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VOR_VX      ")
  def VOR_VI      (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("001010"+vm +vs2e   +imm    +"011"+vde+"1010111", "VOR_VI      ")
  def VXOR_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("001011"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VXOR_VV     ")
  def VXOR_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("001011"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VXOR_VX     ")
  def VXOR_VI     (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("001011"+vm +vs2e   +imm    +"011"+vde+"1010111", "VXOR_VI     ")
  val bitInsts = Array(VAND_VV(), VAND_VX(), VAND_VI(), 
                       VOR_VV(), VOR_VX(), VOR_VI(),
                       VXOR_VV(), VXOR_VX(), VXOR_VI())

  def VSLL_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100101"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VSLL_VV     ")
  def VSLL_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100101"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VSLL_VX     ")
  def VSLL_VI     (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("100101"+vm +vs2e   +imm    +"011"+vde+"1010111", "VSLL_VI     ")
  def VSRL_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101000"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VSRL_VV     ")
  def VSRL_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101000"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VSRL_VX     ")
  def VSRL_VI     (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("101000"+vm +vs2e   +imm    +"011"+vde+"1010111", "VSRL_VI     ")
  def VSRA_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101001"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VSRA_VV     ")
  def VSRA_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101001"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VSRA_VX     ")
  def VSRA_VI     (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("101001"+vm +vs2e   +imm    +"011"+vde+"1010111", "VSRA_VI     ")
  val shiftInsts = Array(VSLL_VV(), VSLL_VX(), VSLL_VI(), 
                         VSRL_VV(), VSRL_VX(), VSRL_VI(), 
                         VSRA_VV(), VSRA_VX(), VSRA_VI()) 

  def VNSRL_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101100"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VNSRL_VV    ")
  def VNSRL_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101100"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VNSRL_VX    ")
  def VNSRL_VI    (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("101100"+vm +vs2e   +imm    +"011"+vde+"1010111", "VNSRL_VI    ")
  def VNSRA_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101101"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VNSRA_VV    ")
  def VNSRA_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101101"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VNSRA_VX    ")
  def VNSRA_VI    (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("101101"+vm +vs2e   +imm    +"011"+vde+"1010111", "VNSRA_VI    ")
  val nshiftInsts = Array(VNSRL_VV(), VNSRL_VX(), VNSRL_VI(), 
                          VNSRA_VV(), VNSRA_VX(), VNSRA_VI())

  def VMSEQ_VV    (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("011000"+vm +vs2e   +vs1e   +"000"+vdm+"1010111", "VMSEQ_VV    ")
  def VMSEQ_VX    (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011000"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSEQ_VX    ")
  def VMSEQ_VI    (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("011000"+vm +vs2e   +imm    +"011"+vdm+"1010111", "VMSEQ_VI    ")
  def VMSNE_VV    (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("011001"+vm +vs2e   +vs1e   +"000"+vdm+"1010111", "VMSNE_VV    ")
  def VMSNE_VX    (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011001"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSNE_VX    ")
  def VMSNE_VI    (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("011001"+vm +vs2e   +imm    +"011"+vdm+"1010111", "VMSNE_VI    ")
  def VMSLTU_VV   (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("011010"+vm +vs2e   +vs1e   +"000"+vdm+"1010111", "VMSLTU_VV   ")
  def VMSLTU_VX   (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011010"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSLTU_VX   ")
  def VMSLT_VV    (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("011011"+vm +vs2e   +vs1e   +"100"+vdm+"1010111", "VMSLT_VV    ")
  def VMSLT_VX    (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011011"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSLT_VX    ")
  def VMSLEU_VV   (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("011100"+vm +vs2e   +vs1e   +"000"+vdm+"1010111", "VMSLEU_VV   ")
  def VMSLEU_VX   (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011100"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSLEU_VX   ")
  def VMSLEU_VI   (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("011100"+vm +vs2e   +imm    +"011"+vdm+"1010111", "VMSLEU_VI   ")
  def VMSLE_VV    (vdm: String = vdm, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("011101"+vm +vs2e   +vs1e   +"000"+vdm+"1010111", "VMSLE_VV    ")
  def VMSLE_VX    (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011101"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSLE_VX    ")
  def VMSLE_VI    (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("011101"+vm +vs2e   +imm    +"011"+vdm+"1010111", "VMSLE_VI    ")
  def VMSGTU_VX   (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011110"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSGTU_VX   ")
  def VMSGTU_VI   (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("011110"+vm +vs2e   +imm    +"011"+vdm+"1010111", "VMSGTU_VI   ")
  def VMSGT_VX    (vdm: String = vdm, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("011111"+vm +vs2e   +rs1    +"100"+vdm+"1010111", "VMSGT_VX    ")
  def VMSGT_VI    (vdm: String = vdm, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("011111"+vm +vs2e   +imm    +"011"+vdm+"1010111", "VMSGT_VI    ")
  val cmpInsts = Array(VMSEQ_VV(), VMSEQ_VX(), VMSEQ_VI(), 
                       VMSNE_VV(), VMSNE_VX(), VMSNE_VI(),
                       VMSLTU_VV(), VMSLTU_VX(), 
                       VMSLT_VV(), VMSLT_VX(), 
                       VMSLEU_VV(), VMSLEU_VX(), VMSLEU_VI(), 
                       VMSLE_VV(), VMSLE_VX(), VMSLE_VI(), 
                       VMSGTU_VX(), VMSGTU_VI(), 
                       VMSGT_VX(), VMSGT_VI())

  def VMINU_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("000100"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VMINU_VV    ")
  def VMINU_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000100"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VMINU_VX    ")
  def VMIN_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("000101"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VMIN_VV     ")
  def VMIN_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000101"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VMIN_VX     ")
  def VMAXU_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("000110"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VMAXU_VV    ")
  def VMAXU_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000110"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VMAXU_VX    ")
  def VMAX_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("000111"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VMAX_VV     ")
  def VMAX_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("000111"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VMAX_VX     ")
  val minInsts = Array(VMINU_VV(), VMINU_VX(), 
                       VMIN_VV(), VMIN_VX(), 
                       VMAXU_VV(), VMAXU_VX(), 
                       VMAX_VV(), VMAX_VX())

  def VMERGE_VVM  (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("010111"+vm +vs2e   +vs1e   +"000"+vde+"1010111", "VMERGE_VVM  ")
  def VMERGE_VXM  (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("010111"+vm +vs2e   +rs1    +"100"+vde+"1010111", "VMERGE_VXM  ")
  def VMERGE_VIM  (vde: String = vde, vs2e: String = ldForVs2, imm:  String = imm,      vm: String = vm) = ("010111"+vm +vs2e   +imm    +"011"+vde+"1010111", "VMERGE_VIM  ")
  val mergeInsts = Array(VMERGE_VVM(), VMERGE_VXM(), VMERGE_VIM())

  def VPOPC_M     (rd: String = rd,   vs2m: String = vs2m,                              vm: String = vm) = ("010000"+vm +vs2m   +"10000"+"010"+rd +"1010111", "VPOPC_M     ")
  val popcInst = Array(VPOPC_M())

  def VFIRST_M    (rd: String = rd,   vs2m: String = vs2m,                              vm: String = vm) = ("010000"+vm +vs2m   +"10001"+"010"+rd +"1010111", "VFIRST_M    ")
  val firstInst = Array(VFIRST_M())

  def VMSBF_M     (vdm: String = vdm, vs2m: String = vs2m,                              vm: String = vm) = ("010100"+vm +vs2m   +"00001"+"010"+vdm+"1010111", "VMSBF_M     ")
  def VMSOF_M     (vdm: String = vdm, vs2m: String = vs2m,                              vm: String = vm) = ("010100"+vm +vs2m   +"00010"+"010"+vdm+"1010111", "VMSOF_M     ")
  def VMSIF_M     (vdm: String = vdm, vs2m: String = vs2m,                              vm: String = vm) = ("010100"+vm +vs2m   +"00011"+"010"+vdm+"1010111", "VMSIF_M     ")
  val midxInsts = Array(VMSBF_M(), VMSOF_M(), VMSIF_M())

  def VIOTA_M     (vde: String = vde, vs2m: String = vs2m,                              vm: String = vm) = ("010100"+vm +vs2m   +"10000"+"010"+vde+"1010111", "VIOTA_M     ")
  val iotaInst = Array(VIOTA_M())

  def VID_V       (vde: String = vde,                                                   vm: String = vm) = ("010100"+vm +"00000"+"10001"+"010"+vde+"1010111", "VID_V       ")
  val idInst = Array(VID_V())

  def VMANDNOT_MM (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011000"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMANDNOT_MM ")
  def VMAND_MM    (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011001"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMAND_MM    ")
  def VMOR_MM     (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011010"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMOR_MM     ")
  def VMXOR_MM    (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011011"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMXOR_MM    ")
  def VMORNOT_MM  (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011100"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMORNOT_MM  ")
  def VMNAND_MM   (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011101"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMNAND_MM   ")
  def VMNOR_MM    (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011110"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMNOR_MM    ")
  def VMXNOR_MM   (vdm: String = vdm, vs2m: String = vs2m,     vs1m: String = vs1m)                      = ("011111"+"1"+vs2m   +vs1m   +"010"+vdm+"1010111", "VMXNOR_MM   ")
  val mbitInsts = Array(VMANDNOT_MM(), VMAND_MM(), VMOR_MM(), VMXOR_MM(), VMORNOT_MM(), VMNAND_MM(), VMNOR_MM(), VMXNOR_MM())

  def VMUL_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100101"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VMUL_VV     ")
  def VMUL_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100101"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VMUL_VX     ")
  def VMULH_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100111"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VMULH_VV    ")
  def VMULH_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100111"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VMULH_VX    ")
  def VMULHU_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100100"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VMULHU_VV   ")
  def VMULHU_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100100"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VMULHU_VX   ")
  def VMULHSU_VV  (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100110"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VMULHSU_VV  ")
  def VMULHSU_VX  (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100110"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VMULHSU_VX  ")
  val mulInsts = Array(VMUL_VV(), VMUL_VX(), 
                       VMULH_VV(), VMULH_VX(), 
                       VMULHU_VV(), VMULHU_VX(), 
                       VMULHSU_VV(), VMULHSU_VX())

  def VWMUL_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("111011"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWMUL_VV    ")
  def VWMUL_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111011"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMUL_VX    ")
  def VWMULU_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("111000"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWMULU_VV   ")
  def VWMULU_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111000"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMULU_VX   ")
  def VWMULSU_VV  (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("111010"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWMULSU_VV  ")
  def VWMULSU_VX  (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111010"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMULSU_VX  ")
  val wmulInsts = Array(VWMUL_VV(), VWMUL_VX(), 
                        VWMULU_VV(), VWMULU_VX(), 
                        VWMULSU_VV(), VWMULSU_VX())

  def VDIV_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100001"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VDIV_VV     ")
  def VDIV_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100001"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VDIV_VX     ")
  def VDIVU_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100000"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VDIVU_VV    ")
  def VDIVU_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100000"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VDIVU_VX    ")
  def VREM_VV     (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100011"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VREM_VV     ")
  def VREM_VX     (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100011"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VREM_VX     ")
  def VREMU_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("100010"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VREMU_VV    ")
  def VREMU_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("100010"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VREMU_VX    ")
  val divInsts = Array(VDIV_VV(), VDIV_VX(), VDIVU_VV(), VDIVU_VX(), 
                       VREM_VV(), VREM_VX(), VREMU_VV(), VREMU_VX())

  def VMADD_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101001"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VMADD_VV    ")
  def VMADD_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101001"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VMADD_VX    ")
  def VNMSUB_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101011"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VNMSUB_VV   ")
  def VNMSUB_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101011"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VNMSUB_VX   ")
  def VMACC_VV    (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101101"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VMACC_VV    ")
  def VMACC_VX    (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101101"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VMACC_VX    ")
  def VNMSAC_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("101111"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VNMSAC_VV   ")
  def VNMSAC_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("101111"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VNMSAC_VX   ")
  val maddInsts = Array(VMADD_VV(), VMADD_VX(),
                        VNMSUB_VV(), VNMSUB_VX(), 
                        VMACC_VV(), VMACC_VX(), 
                        VNMSAC_VV(), VNMSAC_VX())

  def VWMACCU_VV  (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("111100"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWMACCU_VV  ")
  def VWMACCU_VX  (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111100"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMACCU_VX  ")
  def VWMACC_VV   (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("111101"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWMACC_VV   ")
  def VWMACC_VX   (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111101"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMACC_VX   ")
  def VWMACCSU_VV (vde: String = vde, vs2e: String = ldForVs2, vs1e: String = ldForVs1, vm: String = vm) = ("111110"+vm +vs2e   +vs1e   +"010"+vde+"1010111", "VWMACCSU_VV ")
  def VWMACCSU_VX (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111110"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMACCSU_VX ")
  def VWMACCUS_VX (vde: String = vde, vs2e: String = ldForVs2, rs1:  String = rs1,      vm: String = vm) = ("111111"+vm +vs2e   +rs1    +"110"+vde+"1010111", "VWMACCUS_VX ")
  val wmaddInsts = Array(VWMACCU_VV(), VWMACCU_VX(), 
                         VWMACC_VV(), VWMACC_VX(), 
                         VWMACCSU_VV(), VWMACCSU_VX(), 
                         VWMACCUS_VX())

  def VWREDSUMU_VS(vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("110000"+vm +vs2e   +vs1s   +"000"+vds+"1010111", "VWREDSUMU_VS")
  def VWREDSUM_VS (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("110001"+vm +vs2e   +vs1s   +"000"+vds+"1010111", "VWREDSUM_VS ")
  def VREDSUM_VS  (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000000"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDSUM_VS  ")
  def VREDAND_VS  (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000001"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDAND_VS  ")
  def VREDOR_VS   (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000010"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDOR_VS   ")
  def VREDXOR_VS  (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000011"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDXOR_VS  ")
  def VREDMINU_VS (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000100"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDMINU_VS ")
  def VREDMIN_VS  (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000101"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDMIN_VS  ")
  def VREDMAXU_VS (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000110"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDMAXU_VS ")
  def VREDMAX_VS  (vds: String = vds, vs2e: String = ldForVs2, vs1s: String = vs1s,     vm: String = vm) = ("000111"+vm +vs2e   +vs1s   +"010"+vds+"1010111", "VREDMAX_VS  ")
  val redInsts = Array(VWREDSUMU_VS(), VWREDSUM_VS(), VREDSUM_VS(),
                       VREDAND_VS(), VREDOR_VS(), VREDXOR_VS(),
                       VREDMINU_VS(), VREDMIN_VS(),
                       VREDMAXU_VS(), VREDMAX_VS())
  //integer scalar move instructions
  def VMV_X_S     (rd:  String = rd,  vs2s: String = vs2s)                                               = ("010000"+"1"+vs2s   +"00000"+"010"+rd +"1010111", "VMV_X_S     ")
  def VMV_S_X     (vds: String = vds,                          rs1:  String = rs1)                       = ("010000"+"1"+"00000"+rs1    +"110"+vds+"1010111", "VMV_S_X     ")
  val mvInsts = Array(VMV_X_S(), VMV_S_X())

  //CSR Insts
  def VSETVLI(rd: String = rd, rs1: String = rs1, vtypei: String = vtypei)    = ("0"+vtypei          +rs1    +"111"+rd +"1010111", "VSETVLI    ")
  def VSETVL (rd: String = rd, rs1: String = rs1, rs2: String = rs2)          = ("1"+"000000"+rs2    +rs1    +"111"+rd +"1010111", "VSETVL     ")
  val CSRInsts = Array(VSETVLI(), VSETVL())

  //Load Insts
  def VLB_V  (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"100"+vm+"00000"+rs1+"000"+vde+"0000111", "VLB_V      ")
  def VLBU_V (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"000"+vm+"00000"+rs1+"000"+vde+"0000111", "VLBU_V     ")
  def VLSB_V (vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"110"+vm+rs2    +rs1+"000"+vde+"0000111", "VLSB_V     ")
  def VLSBU_V(vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"010"+vm+rs2    +rs1+"000"+vde+"0000111", "VLSBU_V    ")
  def VLXB_V (vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"111"+vm+vs2e   +rs1+"000"+vde+"0000111", "VLXB_V     ")
  def VLXBU_V(vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"011"+vm+vs2e   +rs1+"000"+vde+"0000111", "VLXBU_V    ")
  def LBInsts(vde: String) = Array(VLB_V(vde), VLBU_V(vde),
                                   VLSB_V(vde), VLSBU_V(vde),
                                   VLXB_V(vde), VLXBU_V(vde))

  def VLH_V  (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"100"+vm+"00000"+rs1+"101"+vde+"0000111", "VLH_V      ")
  def VLHU_V (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"000"+vm+"00000"+rs1+"101"+vde+"0000111", "VLHU_V     ")
  def VLSH_V (vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"110"+vm+rs2    +rs1+"101"+vde+"0000111", "VLSH_V     ")
  def VLSHU_V(vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"010"+vm+rs2    +rs1+"101"+vde+"0000111", "VLSHU_V    ")
  def VLXH_V (vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"111"+vm+vs2e   +rs1+"101"+vde+"0000111", "VLXH_V     ")
  def VLXHU_V(vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"011"+vm+vs2e   +rs1+"101"+vde+"0000111", "VLXHU_V    ")
  def LHInsts(vde: String) = Array(VLH_V(vde), VLHU_V(vde), 
                                   VLSH_V(vde), VLSHU_V(vde), 
                                   VLXH_V(vde), VLXHU_V(vde))

  def VLW_V  (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"100"+vm+"00000"+rs1+"110"+vde+"0000111", "VLW_V      ")
  def VLWU_V (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"000"+vm+"00000"+rs1+"110"+vde+"0000111", "VLWU_V     ")
  def VLSW_V (vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"110"+vm+rs2    +rs1+"110"+vde+"0000111", "VLSW_V     ")
  def VLSWU_V(vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"010"+vm+rs2    +rs1+"110"+vde+"0000111", "VLSWU_V    ")
  def VLXW_V (vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"111"+vm+vs2e   +rs1+"110"+vde+"0000111", "VLXW_V     ")
  def VLXWU_V(vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"011"+vm+vs2e   +rs1+"110"+vde+"0000111", "VLXWU_V    ")
  def LWInsts(vde: String) = Array(VLW_V(vde), VLWU_V(vde),
                                   VLSW_V(vde), VLSWU_V(vde), 
                                   VLXW_V(vde), VLXWU_V(vde))

  def VLE_V  (vde: String,  rs1: String = rs1,                      vm: String = "1") = ("000"+"000"+vm+"00000"+rs1+"111"+vde+"0000111", "VLE_V      ")
  def VLSE_V (vde: String,  rs1: String = rs1, rs2:  String = rs2,  vm: String = "1") = ("000"+"010"+vm+rs2    +rs1+"111"+vde+"0000111", "VLSE_V     ")
  def VLXE_V (vde: String,  rs1: String = rs1, vs2e: String = vs2e, vm: String = "1") = ("000"+"011"+vm+vs2e   +rs1+"111"+vde+"0000111", "VLXE_V     ")
  def LEInsts(vde: String) = Array(VLE_V(vde), VLSE_V(vde), VLXE_V(vde))

  //Store Insts
  def VSB_V  (vs3e: String, rs1: String = rs1,                      vm: String = vm) = ("000"+"000"+vm+"00000"+rs1+"000"+vs3e+"0100111", "VSB_V      ")
  def VSSB_V (vs3e: String, rs1: String = rs1, rs2:  String = rs2,  vm: String = vm) = ("000"+"010"+vm+rs2    +rs1+"000"+vs3e+"0100111", "VSSB_V     ")
  def VSXB_V (vs3e: String, rs1: String = rs1, vs2e: String = vs2e, vm: String = vm) = ("000"+"011"+vm+vs2e   +rs1+"000"+vs3e+"0100111", "VSXB_V     ")
  def SBInsts(vs3e: String) = Array(VSB_V(vs3e), VSSB_V(vs3e), VSXB_V(vs3e))

  def VSH_V  (vs3e: String, rs1: String = rs1,                      vm: String = vm) = ("000"+"000"+vm+"00000"+rs1+"101"+vs3e+"0100111", "VSH_V      ")
  def VSSH_V (vs3e: String, rs1: String = rs1, rs2:  String = rs2,  vm: String = vm) = ("000"+"010"+vm+rs2    +rs1+"101"+vs3e+"0100111", "VSSH_V     ")
  def VSXH_V (vs3e: String, rs1: String = rs1, vs2e: String = vs2e, vm: String = vm) = ("000"+"011"+vm+vs2e   +rs1+"101"+vs3e+"0100111", "VSXH_V     ")
  def SHInsts(vs3e: String) = Array(VSH_V(vs3e), VSSH_V(vs3e), VSXH_V(vs3e))

  def VSW_V  (vs3e: String, rs1: String = rs1,                      vm: String = vm) = ("000"+"000"+vm+"00000"+rs1+"110"+vs3e+"0100111", "VSW_V      ")
  def VSSW_V (vs3e: String, rs1: String = rs1, rs2:  String = rs2,  vm: String = vm) = ("000"+"010"+vm+rs2    +rs1+"110"+vs3e+"0100111", "VSSW_V     ")
  def VSXW_V (vs3e: String, rs1: String = rs1, vs2e: String = vs2e, vm: String = vm) = ("000"+"011"+vm+vs2e   +rs1+"110"+vs3e+"0100111", "VSXW_V     ")
  def SWInsts(vs3e: String) = Array(VSW_V(vs3e), VSSW_V(vs3e), VSXW_V(vs3e))

  def VSE_V  (vs3e: String, rs1: String = rs1,                      vm: String = vm) = ("000"+"000"+vm+"00000"+rs1+"111"+vs3e+"0100111", "VSE_V      ")
  def VSSE_V (vs3e: String, rs1: String = rs1, rs2:  String = rs2,  vm: String = vm) = ("000"+"010"+vm+rs2    +rs1+"111"+vs3e+"0100111", "VSSE_V     ")
  def VSXE_V (vs3e: String, rs1: String = rs1, vs2e: String = vs2e, vm: String = vm) = ("000"+"011"+vm+vs2e   +rs1+"111"+vs3e+"0100111", "VSXE_V     ")
  def SEInsts(vs3e: String) = Array(VSE_V(vs3e), VSSE_V(vs3e), VSXE_V(vs3e))
///////////////////////////instructions name-code pairs/////////////////////////


  def InstHex(inst: BigInt) = "0"*(8-inst.toString(16).length)+inst.toString(16)

  //delay x cycles, under 8 cycles
  step(rnd.nextInt(8))

/////////////////////////////////////CSR Inst///////////////////////////////////
  for(i <- 0 until 1) {
    val (csrCode, csrName) = CSRInsts(rnd.nextInt(2))
    val csrInst = BigInt(csrCode, 2)

    writer.println("CSRInstName  = " + csrName + " CSRInst = " + InstHex(csrInst))
    writer.println()

    //poke a CSR instruction
    poke(c.io.core.req.valid, 1)
    poke(c.io.core.req.bits.inst, csrInst)
    step(1)
    poke(c.io.core.fromXData1, AVL)
    poke(c.io.core.fromXData2, vtype)
  }
/////////////////////////////////////CSR Inst///////////////////////////////////


///////////////////////////////////Load Insts///////////////////////////////////
  var j = 0
  for(i <- 0 until 2) {
    val vde              = ldDstField(i)
    val LdInsts          = if(SEW == 8) LBInsts(vde) ++ LEInsts(vde)
                           else if(SEW == 16) LBInsts(vde) ++ LHInsts(vde) ++ LEInsts(vde)
                           else if(SEW == 32) LBInsts(vde) ++ LHInsts(vde) ++ LWInsts(vde) ++ LEInsts(vde)
                           else LEInsts(vde)
    val ldIndex          = if(SEW == 8) rnd.nextInt(9)
                           else if(SEW == 16) rnd.nextInt(15)
                           else if(SEW == 32) rnd.nextInt(21)
                           else rnd.nextInt(3)
    val (ldCode, ldName) = LdInsts(ldIndex)
    val ldInst           = BigInt(ldCode, 2)
    val baseAddr         = BigInt(5, scala.util.Random)
    val strideConst      = BigInt(5, scala.util.Random)

    writer.println("LoadInstName = " + ldName + " LoadInst = " + InstHex(ldInst) + " vm = 1" + " vd = " + BigInt(vde, 2))

    //poke load instruction
    poke(c.io.core.req.valid, 1)
    poke(c.io.core.req.bits.inst, ldInst)
    step(1)
    poke(c.io.core.fromXData1, baseAddr)
    poke(c.io.core.fromXData2, strideConst)
    step(1)

    var storeReq   = Array((BigInt(0),BigInt(0),BigInt(0)),(BigInt(0),BigInt(0),BigInt(0)))
    //poke data
    while(peek(c.io.core.req.ready) == 0) {
      val reqValid: BigInt = peek(c.io.reqValid)
      val reqTag: BigInt   = peek(c.io.reqTag)
      val reqSize: BigInt  = peek(c.io.reqSize)
      val respHasData      = peek(c.io.reqCmd)+1
      val respData         = elemVal + j
      val reqReady         = 1
      val s2Nack           = 0
      storeReq = storeReq ++ Array((reqValid,reqTag,reqSize))

      poke(c.io.respValid, storeReq(0)._1)
      poke(c.io.respTag, storeReq(0)._2)
      poke(c.io.respSize, storeReq(0)._3)
      poke(c.io.respHasData, respHasData)
      poke(c.io.respData, respData)
      poke(c.io.reqReady, reqReady)
      poke(c.io.s2Nack, s2Nack)
      step(1)
      j = j + storeReq(0)._1.toInt
      storeReq = storeReq drop 1
    }
  }
///////////////////////////////////Load Insts///////////////////////////////////

  writer.println()

/////////////////////////////////////AL Insts///////////////////////////////////
  for(i <- 0 until 8) {
    val baseALInsts      = addInsts ++ waddInsts ++ adcInsts ++ bitInsts ++ shiftInsts ++ nshiftInsts ++ cmpInsts ++ minInsts ++ mergeInsts
    val maskInsts        = popcInst ++ firstInst ++ midxInsts ++ iotaInst ++ idInst ++ mbitInsts
//    val alInsts          = baseALInsts ++ maskInsts ++ 
//                           (if(c.MULDIV) mulInsts ++ wmulInsts ++ divInsts else Nil) ++ 
//                           (if(c.MULADD) maddInsts ++ wmaddInsts else Nil) ++ 
//                           (if(c.RED) redInsts else Nil) ++ 
//                           (if(c.MV) mvInsts else Nil)
    val alInsts          = redInsts
    val alInstSum        = alInsts.length
    val (alCode, alName) = alInsts(rnd.nextInt(alInstSum))
    val alInst           = BigInt(alCode, 2)
    val alDstField       = (alInst & BigInt("00000000000000000000111110000000", 2)) >> 7
    val alSrc1Field      = (alInst & BigInt("00000000000011111000000000000000", 2)) >> 15
    val alSrc2Field      = (alInst & BigInt("00000001111100000000000000000000", 2)) >> 20
    val alVmField        = (alInst & BigInt("00000010000000000000000000000000", 2)) >> 25
    val alDstFieldStr    = " "*(2-alDstField.toString.length) + alDstField.toString
    val alSrc1FieldStr   = " "*(2-alSrc1Field.toString.length) + alSrc1Field.toString
    val alSrc2FieldStr   = " "*(2-alSrc2Field.toString.length) + alSrc2Field.toString

  writer.println("ALInstName = " + alName + " ALInst = " + InstHex(alInst) + " DstField = " + alDstFieldStr + " Src2Field = " + alSrc2FieldStr + " Src1Field = " + alSrc1FieldStr + " vm = " + alVmField)

    //poke a AL instruction
    poke(c.io.core.req.valid, 1)
    poke(c.io.core.req.bits.inst, alInst)
    step(1)
    while(peek(c.io.core.req.ready) == 0)
      step(1)
  }
/////////////////////////////////////AL Insts///////////////////////////////////

  writer.println()

//////////////////////////////////Store Insts///////////////////////////////////
  for(i <- 0 until 1) {
    val stInsts          = if(SEW == 8) SBInsts(vs3e) ++ SEInsts(vs3e)
                           else if(SEW == 16) SBInsts(vs3e) ++ SHInsts(vs3e) ++ SEInsts(vs3e)
                           else if(SEW == 32) SBInsts(vs3e) ++ SHInsts(vs3e) ++ SWInsts(vs3e) ++ SEInsts(vs3e)
                           else SEInsts(vs3e)
    val stInstSum        = stInsts.length
    val (stCode, stName) = stInsts(rnd.nextInt(stInstSum))
    val stInst           = BigInt(stCode, 2)
    val stDstField       = (stInst & BigInt("00000000000000000000111110000000", 2)) >> 7
    val stSrc1Field      = (stInst & BigInt("00000000000011111000000000000000", 2)) >> 15
    val stSrc2Field      = (stInst & BigInt("00000001111100000000000000000000", 2)) >> 20
    val stVmField        = (stInst & BigInt("00000010000000000000000000000000", 2)) >> 25
    val stDstFieldStr    = " "*(2-stDstField.toString.length) + stDstField.toString
    val stSrc1FieldStr   = " "*(2-stSrc1Field.toString.length) + stSrc1Field.toString
    val stSrc2FieldStr   = " "*(2-stSrc2Field.toString.length) + stSrc2Field.toString

    writer.println("StoreInstName = " + stName + " StoreInst = " + InstHex(stInst) + " vs3 = " + stDstFieldStr + " rs1 = " + stSrc1FieldStr + " src2Field = " + stSrc2FieldStr + " vm = " + stVmField)

    //poke a store instruction
    poke(c.io.core.req.valid, 1)
    poke(c.io.core.req.bits.inst, stInst)
    step(1)
/*
    for(k <- 0 until AVL) {
        poke(c.io.respValid, 1)
        poke(c.io.respTag, peek(c.io.reqTag))
        poke(c.io.respSize, peek(c.io.reqSize))
        poke(c.io.respHasData, 0)
        poke(c.io.respData, 0)
        poke(c.io.reqReady, 1)
        poke(c.io.s2Nack, 0)
      step(1)
    }
*/
    var storeReq   = Array((BigInt(0),BigInt(0),BigInt(0)),(BigInt(0),BigInt(0),BigInt(0)))
    //poke data
    while(peek(c.io.core.req.ready) == 0) {
      val reqValid: BigInt = peek(c.io.reqValid)
      val reqTag: BigInt   = peek(c.io.reqTag)
      val reqSize: BigInt  = peek(c.io.reqSize)
      val respHasData      = peek(c.io.reqCmd)+1
      val respData         = elemVal + j
      val reqReady         = 1
      val s2Nack           = 0
      storeReq = storeReq ++ Array((reqValid,reqTag,reqSize))

      poke(c.io.respValid, storeReq(0)._1)
      poke(c.io.respTag, storeReq(0)._2)
      poke(c.io.respSize, storeReq(0)._3)
      poke(c.io.respHasData, respHasData)
      poke(c.io.respData, respData)
      poke(c.io.reqReady, reqReady)
      poke(c.io.s2Nack, s2Nack)
      step(1)
      j = j + storeReq(0)._1.toInt
      storeReq = storeReq drop 1
    }
  }
//////////////////////////////////Store Insts///////////////////////////////////
  poke(c.io.core.req.valid, 0)
  step(5)
  writer.close()
}
