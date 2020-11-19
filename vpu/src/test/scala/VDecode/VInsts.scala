// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

object VInsts {
  def OneBitString: String = BigInt(1, scala.util.Random).toString(2)
  def FiveBitsString: String = { val rf = BigInt(5, scala.util.Random).toString(2); "0"*(5-rf.length)+rf }
  def ElevenBitsString: String = { val rf = BigInt(11, scala.util.Random).toString(2); "0"*(11-rf.length)+rf }

  val vmField   = OneBitString
  val src1Field = FiveBitsString
  val src2Field = FiveBitsString
  val destField = FiveBitsString
  val zimm      = ElevenBitsString

  val csr_insts = Array(
    "VSETVLI          " -> BigInt("0" + zimm                 + src2Field + "111" + destField + "1010111", 2),
    "VSETVL           " -> BigInt("1" + "000000" + src1Field + src2Field + "111" + destField + "1010111", 2)
  )
  val al_insts = Array(
    "VADD_VV          " -> BigInt("000000" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VADD_VX          " -> BigInt("000000" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VADD_VI          " -> BigInt("000000" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSUB_VV          " -> BigInt("000010" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSUB_VX          " -> BigInt("000010" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VRSUB_VX         " -> BigInt("000011" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VRSUB_VI         " -> BigInt("000011" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VWADDU_VV        " -> BigInt("110000" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWADDU_VX        " -> BigInt("110000" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWADD_VV         " -> BigInt("110001" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWADD_VX         " -> BigInt("110001" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWSUBU_VV        " -> BigInt("110010" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWSUBU_VX        " -> BigInt("110010" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWSUB_VV         " -> BigInt("110011" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWSUB_VX         " -> BigInt("110011" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWADDU_WV        " -> BigInt("110100" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWADDU_WX        " -> BigInt("110100" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWADD_WV         " -> BigInt("110101" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWADD_WX         " -> BigInt("110101" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWSUBU_WV        " -> BigInt("110110" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWSUBU_WX        " -> BigInt("110110" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWSUB_WV         " -> BigInt("110111" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWSUB_WX         " -> BigInt("110111" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VADC_VVM         " -> BigInt("010000" + "0"     + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VADC_VXM         " -> BigInt("010000" + "0"     + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VADC_VIM         " -> BigInt("010000" + "0"     + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMADC_VVM        " -> BigInt("010001" + "0"     + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMADC_VXM        " -> BigInt("010001" + "0"     + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMADC_VIM        " -> BigInt("010001" + "0"     + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMADC_VV         " -> BigInt("010001" + "1"     + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMADC_VX         " -> BigInt("010001" + "1"     + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMADC_VI         " -> BigInt("010001" + "1"     + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSBC_VVM         " -> BigInt("010010" + "0"     + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSBC_VXM         " -> BigInt("010010" + "0"     + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSBC_VVM        " -> BigInt("010011" + "0"     + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSBC_VXM        " -> BigInt("010011" + "0"     + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSBC_VV         " -> BigInt("010011" + "1"     + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSBC_VX         " -> BigInt("010011" + "1"     + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VZEXT_VF8        " -> BigInt("010010" + vmField + src1Field + "00010"   + "010" + destField + "1010111", 2),
    "VSEXT_VF8        " -> BigInt("010010" + vmField + src1Field + "00011"   + "010" + destField + "1010111", 2),
    "VZEXT_VF4        " -> BigInt("010010" + vmField + src1Field + "00100"   + "010" + destField + "1010111", 2),
    "VSEXT_VF4        " -> BigInt("010010" + vmField + src1Field + "00101"   + "010" + destField + "1010111", 2),
    "VZEXT_VF2        " -> BigInt("010010" + vmField + src1Field + "00110"   + "010" + destField + "1010111", 2),
    "VSEXT_VF2        " -> BigInt("010010" + vmField + src1Field + "00111"   + "010" + destField + "1010111", 2),
    "VAND_VV          " -> BigInt("001001" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VAND_VX          " -> BigInt("001001" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VAND_VI          " -> BigInt("001001" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VOR_VV           " -> BigInt("001010" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VOR_VX           " -> BigInt("001010" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VOR_VI           " -> BigInt("001010" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VXOR_VV          " -> BigInt("001011" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VXOR_VX          " -> BigInt("001011" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VXOR_VI          " -> BigInt("001011" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSLL_VV          " -> BigInt("100101" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSLL_VX          " -> BigInt("100101" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSLL_VI          " -> BigInt("100101" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSRL_VV          " -> BigInt("101000" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSRL_VX          " -> BigInt("101000" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSRL_VI          " -> BigInt("101000" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSRA_VV          " -> BigInt("101001" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSRA_VX          " -> BigInt("101001" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSRA_VI          " -> BigInt("101001" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VNSRL_WV         " -> BigInt("101100" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VNSRL_WX         " -> BigInt("101100" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VNSRL_WI         " -> BigInt("101100" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VNSRA_WV         " -> BigInt("101101" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VNSRA_WX         " -> BigInt("101101" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VNSRA_WI         " -> BigInt("101101" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMSEQ_VV         " -> BigInt("011000" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSEQ_VX         " -> BigInt("011000" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSEQ_VI         " -> BigInt("011000" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMSNE_VV         " -> BigInt("011001" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSNE_VX         " -> BigInt("011001" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSNE_VI         " -> BigInt("011001" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMSLTU_VV        " -> BigInt("011010" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSLTU_VX        " -> BigInt("011010" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSLT_VV         " -> BigInt("011011" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSLT_VX         " -> BigInt("011011" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSLEU_VV        " -> BigInt("011100" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSLEU_VX        " -> BigInt("011100" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSLEU_VI        " -> BigInt("011100" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMSLE_VV         " -> BigInt("011101" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMSLE_VX         " -> BigInt("011101" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSLE_VI         " -> BigInt("011101" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMSGTU_VX        " -> BigInt("011110" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSGTU_VI        " -> BigInt("011110" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMSGT_VX         " -> BigInt("011111" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMSGT_VI         " -> BigInt("011111" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VMINU_VV         " -> BigInt("000100" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMINU_VX         " -> BigInt("000100" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMIN_VV          " -> BigInt("000101" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMIN_VX          " -> BigInt("000101" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMAXU_VV         " -> BigInt("000110" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMAXU_VX         " -> BigInt("000110" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMAX_VV          " -> BigInt("000111" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMAX_VX          " -> BigInt("000111" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2)
  )
  val merge_insts = Array(
    "VMERGE_VVM       " -> BigInt("010111" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VMERGE_VXM       " -> BigInt("010111" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VMERGE_VIM       " -> BigInt("010111" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2)
  )
  val mul_insts = Array(
    "VMUL_VV          " -> BigInt("100101" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMUL_VX          " -> BigInt("100101" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VMULH_VV         " -> BigInt("100111" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMULH_VX         " -> BigInt("100111" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VMULHU_VV        " -> BigInt("100100" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMULHU_VX        " -> BigInt("100100" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VMULHSU_VV       " -> BigInt("100110" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMULHSU_VX       " -> BigInt("100110" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMUL_VV         " -> BigInt("111011" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWMUL_VX         " -> BigInt("111011" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMULU_VV        " -> BigInt("111000" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWMULU_VX        " -> BigInt("111000" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMULSU_VV       " -> BigInt("111010" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWMULSU_VX       " -> BigInt("111010" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VDIV_VV          " -> BigInt("100001" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VDIV_VX          " -> BigInt("100001" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VDIVU_VV         " -> BigInt("100000" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VDIVU_VX         " -> BigInt("100000" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VREM_VV          " -> BigInt("100011" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREM_VX          " -> BigInt("100011" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VREMU_VV         " -> BigInt("100010" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREMU_VX         " -> BigInt("100010" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2)
  )
  val madd_insts = Array(
    "VMADD_VV         " -> BigInt("101001" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMADD_VX         " -> BigInt("101001" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VNMSUB_VV        " -> BigInt("101011" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VNMSUB_VX        " -> BigInt("101011" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VMACC_VV         " -> BigInt("101101" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMACC_VX         " -> BigInt("101101" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VNMSAC_VV        " -> BigInt("101111" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VNMSAC_VX        " -> BigInt("101111" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMACCU_VV       " -> BigInt("111100" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWMACCU_VX       " -> BigInt("111100" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMACC_VV        " -> BigInt("111101" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWMACC_VX        " -> BigInt("111101" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMACCSU_VV      " -> BigInt("111111" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWMACCSU_VX      " -> BigInt("111111" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VWMACCUS_VX      " -> BigInt("111110" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2)
  )
  val sadd_insts = Array(
    "VSADDU_VV        " -> BigInt("100000" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSADDU_VX        " -> BigInt("100000" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSADDU_VI        " -> BigInt("100000" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSADD_VV         " -> BigInt("100001" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSADD_VX         " -> BigInt("100001" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSADD_VI         " -> BigInt("100001" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSSUBU_VV        " -> BigInt("100010" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSSUBU_VX        " -> BigInt("100010" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSSUB_VV         " -> BigInt("100011" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSSUB_VX         " -> BigInt("100011" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2)
  )
  val aadd_insts = Array(
    "VAADDU_VV        " -> BigInt("001000" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VAADDU_VX        " -> BigInt("001000" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VAADD_VV         " -> BigInt("001001" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VAADD_VX         " -> BigInt("001001" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VASUBU_VV        " -> BigInt("001010" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VASUBU_VX        " -> BigInt("001010" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VASUB_VV         " -> BigInt("001011" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VASUB_VX         " -> BigInt("001011" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2)
  )
  val smul_insts = Array(
    "VSMUL_VV         " -> BigInt("100111" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSMUL_VX         " -> BigInt("100111" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2)
  )
  val ssr_insts = Array(
    "VSSRL_VV         " -> BigInt("101010" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSSRL_VX         " -> BigInt("101010" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSSRL_VI         " -> BigInt("101010" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSSRA_VV         " -> BigInt("101011" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VSSRA_VX         " -> BigInt("101011" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSSRA_VI         " -> BigInt("101011" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2)
  )
  val nclip_insts = Array(
    "VNCLIPU_WV       " -> BigInt("101110" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VNCLIPU_WX       " -> BigInt("101110" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VNCLIPU_WI       " -> BigInt("101110" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VNCLIP_WV        " -> BigInt("101111" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VNCLIP_WX        " -> BigInt("101111" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VNCLIP_WI        " -> BigInt("101111" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2)
  )
  val fma_insts = Array(
    "VFADD_VV         " -> BigInt("000000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFADD_VF         " -> BigInt("000000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFSUB_VV         " -> BigInt("000010" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFSUB_VF         " -> BigInt("000010" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFRSUB_VF        " -> BigInt("100111" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWADD_VV        " -> BigInt("110000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWADD_VF        " -> BigInt("110000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWSUB_VV        " -> BigInt("110010" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWSUB_VF        " -> BigInt("110010" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWADD_WV        " -> BigInt("110100" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWADD_WF        " -> BigInt("110100" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWSUB_WV        " -> BigInt("110110" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWSUB_WF        " -> BigInt("110110" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFMUL_VV         " -> BigInt("100100" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMUL_VF         " -> BigInt("100100" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWMUL_VV        " -> BigInt("111000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWMUL_VF        " -> BigInt("111000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFMACC_VV        " -> BigInt("101100" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMACC_VF        " -> BigInt("101100" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFNMACC_VV       " -> BigInt("101101" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFNMACC_VF       " -> BigInt("101101" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFMSAC_VV        " -> BigInt("101110" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMSAC_VF        " -> BigInt("101110" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFNMSAC_VV       " -> BigInt("101111" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFNMSAC_VF       " -> BigInt("101111" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFMADD_VV        " -> BigInt("101000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMADD_VF        " -> BigInt("101000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFNMADD_VV       " -> BigInt("101001" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFNMADD_VF       " -> BigInt("101001" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFMSUB_VV        " -> BigInt("101010" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMSUB_VF        " -> BigInt("101010" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFNMSUB_VV       " -> BigInt("101011" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFNMSUB_VF       " -> BigInt("101011" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWMACC_VV       " -> BigInt("111100" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWMACC_VF       " -> BigInt("111100" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWNMACC_VV      " -> BigInt("111101" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWNMACC_VF      " -> BigInt("111101" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWMSAC_VV       " -> BigInt("111110" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWMSAC_VF       " -> BigInt("111110" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFWNMSAC_VV      " -> BigInt("111111" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWNMSAC_VF      " -> BigInt("111111" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2)
  )
  val fcvt_insts = Array(
    "VFCVT_XU_F_V     " -> BigInt("010010" + vmField + src1Field + "00000"   + "001" + destField + "1010111", 2),
    "VFCVT_X_F_V      " -> BigInt("010010" + vmField + src1Field + "00001"   + "001" + destField + "1010111", 2),
    "VFCVT_F_XU_V     " -> BigInt("010010" + vmField + src1Field + "00010"   + "001" + destField + "1010111", 2),
    "VFCVT_F_X_V      " -> BigInt("010010" + vmField + src1Field + "00011"   + "001" + destField + "1010111", 2),
    "VFCVT_RTZ_XU_F_V " -> BigInt("010010" + vmField + src1Field + "00110"   + "001" + destField + "1010111", 2),
    "VFCVT_RTZ_X_F_V  " -> BigInt("010010" + vmField + src1Field + "00111"   + "001" + destField + "1010111", 2),
    "VFWCVT_XU_F_V    " -> BigInt("010010" + vmField + src1Field + "01000"   + "001" + destField + "1010111", 2),
    "VFWCVT_X_F_V     " -> BigInt("010010" + vmField + src1Field + "01001"   + "001" + destField + "1010111", 2),
    "VFWCVT_F_XU_V    " -> BigInt("010010" + vmField + src1Field + "01010"   + "001" + destField + "1010111", 2),
    "VFWCVT_F_X_V     " -> BigInt("010010" + vmField + src1Field + "01011"   + "001" + destField + "1010111", 2),
    "VFWCVT_F_F_V     " -> BigInt("010010" + vmField + src1Field + "01100"   + "001" + destField + "1010111", 2),
    "VFWCVT_RTZ_XU_F_V" -> BigInt("010010" + vmField + src1Field + "01110"   + "001" + destField + "1010111", 2),
    "VFWCVT_RTZ_X_F_V " -> BigInt("010010" + vmField + src1Field + "01111"   + "001" + destField + "1010111", 2),
    "VFNCVT_XU_F_W    " -> BigInt("010010" + vmField + src1Field + "10000"   + "001" + destField + "1010111", 2),
    "VFNCVT_X_F_W     " -> BigInt("010010" + vmField + src1Field + "10001"   + "001" + destField + "1010111", 2),
    "VFNCVT_F_XU_W    " -> BigInt("010010" + vmField + src1Field + "10010"   + "001" + destField + "1010111", 2),
    "VFNCVT_F_X_W     " -> BigInt("010010" + vmField + src1Field + "10011"   + "001" + destField + "1010111", 2),
    "VFNCVT_F_F_W     " -> BigInt("010010" + vmField + src1Field + "10100"   + "001" + destField + "1010111", 2),
    "VFNCVT_ROD_F_F_W " -> BigInt("010010" + vmField + src1Field + "10101"   + "001" + destField + "1010111", 2),
    "VFNCVT_RTZ_XU_F_W" -> BigInt("010010" + vmField + src1Field + "10110"   + "001" + destField + "1010111", 2),
    "VFNCVT_RTZ_X_F_W " -> BigInt("010010" + vmField + src1Field + "10111"   + "001" + destField + "1010111", 2)
  )
  val fcmp_insts = Array(
    "VMFEQ_VV         " -> BigInt("011000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VMFEQ_VF         " -> BigInt("011000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VMFNE_VV         " -> BigInt("011100" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VMFNE_VF         " -> BigInt("011100" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VMFLT_VV         " -> BigInt("011011" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VMFLT_VF         " -> BigInt("011011" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VMFLE_VV         " -> BigInt("011001" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VMFLE_VF         " -> BigInt("011001" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VMFGT_VF         " -> BigInt("011101" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VMFGE_VF         " -> BigInt("011111" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2)
  )
  val fminmax_insts = Array(
    "VFMIN_VV         " -> BigInt("000100" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMIN_VF         " -> BigInt("000100" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFMAX_VV         " -> BigInt("000110" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFMAX_VF         " -> BigInt("000110" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2)
  )
  val fsgnj_insts = Array(
    "VFSGNJ_VV        " -> BigInt("001000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFSGNJ_VF        " -> BigInt("001000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFSGNJN_VV       " -> BigInt("001001" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFSGNJN_VF       " -> BigInt("001001" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFSGNJX_VV       " -> BigInt("001010" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFSGNJX_VF       " -> BigInt("001010" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2)
  )
  val fclass_insts = Array(
    "VFCLASS_V        " -> BigInt("010011" + vmField + src1Field + "10000"   + "001" + destField + "1010111", 2)
  )
  val fmerge_insts = Array(
    "VFMERGE_VFM      " -> BigInt("010111" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2)
  )
  val fdiv_insts = Array(
    "VFDIV_VV         " -> BigInt("100000" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFDIV_VF         " -> BigInt("100000" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFRDIV_VF        " -> BigInt("100001" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFSQRT_V         " -> BigInt("010011" + vmField + src1Field + "00000"   + "001" + destField + "1010111", 2)
  )
  val frsqrt_insts = Array(
    "VFRSQRTE7_V      " -> BigInt("010011" + vmField + src1Field + "00100"   + "001" + destField + "1010111", 2)
  )
  val frece_insts = Array(
    "VFRECE7_V        " -> BigInt("010011" + vmField + src1Field + "00101"   + "001" + destField + "1010111", 2)
  )
  val red_insts = Array(
    "VREDSUM_VS       " -> BigInt("000000" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDAND_VS       " -> BigInt("000001" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDOR_VS        " -> BigInt("000010" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDXOR_VS       " -> BigInt("000011" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDMINU_VS      " -> BigInt("000100" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDMIN_VS       " -> BigInt("000101" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDMAXU_VS      " -> BigInt("000110" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VREDMAX_VS       " -> BigInt("000111" + vmField + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VWREDSUMU_VS     " -> BigInt("110000" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VWREDSUM_VS      " -> BigInt("110001" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2)
  )
  val fred_insts = Array(
    "VFREDSUM_VS      " -> BigInt("000001" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFREDOSUM_VS     " -> BigInt("000011" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFREDMIN_VS      " -> BigInt("000101" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFREDMAX_VS      " -> BigInt("000111" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWREDSUM_VS     " -> BigInt("110001" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2),
    "VFWREDOSUM_VS    " -> BigInt("110011" + vmField + src1Field + src2Field + "001" + destField + "1010111", 2)
  )
  val popc_insts = Array(
    "VPOPC_M          " -> BigInt("010000" + vmField + src1Field + "10000"   + "010" + destField + "1010111", 2)
  )
  val first_insts = Array(
    "VFIRST_M         " -> BigInt("010000" + vmField + src1Field + "10001"   + "010" + destField + "1010111", 2)
  )
  val mindex_insts = Array(
    "VMSBF_M          " -> BigInt("010100" + vmField + src1Field + "00001"   + "010" + destField + "1010111", 2),
    "VMSOF_M          " -> BigInt("010100" + vmField + src1Field + "00010"   + "010" + destField + "1010111", 2),
    "VMSIF_M          " -> BigInt("010100" + vmField + src1Field + "00011"   + "010" + destField + "1010111", 2)
  )
  val iota_insts = Array(
    "VIOTA_M          " -> BigInt("010100" + vmField + src1Field + "10000"   + "010" + destField + "1010111", 2)
  )
  val index_insts = Array(
    "VID_V            " -> BigInt("010100" + vmField + "00000"   + "10001"   + "010" + destField + "1010111", 2)
  )
  val mbit_insts = Array(
    "VMANDNOT_MM      " -> BigInt("011000" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMAND_MM         " -> BigInt("011001" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMOR_MM          " -> BigInt("011010" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMXOR_MM         " -> BigInt("011011" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMORNOT_MM       " -> BigInt("011100" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMNAND_MM        " -> BigInt("011101" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMNOR_MM         " -> BigInt("011110" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2),
    "VMXNOR_MM        " -> BigInt("011111" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2)
  )
  val mv_insts = Array(
    "VMV_X_S          " -> BigInt("010000" + "1"     + src1Field + "00000"   + "010" + destField + "1010111", 2),
    "VMV_S_X          " -> BigInt("010000" + "1"     + "00000"   + src2Field + "110" + destField + "1010111", 2)
  )
  val fmv_insts = Array(
    "VFMV_F_S         " -> BigInt("010000" + "1"     + src1Field + "00000"   + "001" + destField + "1010111", 2),
    "VFMV_S_F         " -> BigInt("010000" + "1"     + "00000"   + src2Field + "101" + destField + "1010111", 2)
  )
  val slide_insts = Array(
    "VSLIDEUP_VX      " -> BigInt("001110" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSLIDEUP_VI      " -> BigInt("001110" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSLIDEDOWN_VX    " -> BigInt("001111" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VSLIDEDOWN_VI    " -> BigInt("001111" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2),
    "VSLIDE1UP_VX     " -> BigInt("001110" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VSLIDE1DOWN_VX   " -> BigInt("001111" + vmField + src1Field + src2Field + "110" + destField + "1010111", 2),
    "VFSLIDE1UP_VF    " -> BigInt("001110" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2),
    "VFSLIDE1DOWN_VF  " -> BigInt("001111" + vmField + src1Field + src2Field + "101" + destField + "1010111", 2)
  )
  val gather_insts = Array(
    "VRGATHER_VV      " -> BigInt("001100" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VRGATHEREI16_VV  " -> BigInt("001110" + vmField + src1Field + src2Field + "000" + destField + "1010111", 2),
    "VRGATHER_VX      " -> BigInt("001100" + vmField + src1Field + src2Field + "100" + destField + "1010111", 2),
    "VRGATHER_VI      " -> BigInt("001100" + vmField + src1Field + src2Field + "011" + destField + "1010111", 2)
  )
  val compress_insts = Array(
    "VCOMPRESS_VM     " -> BigInt("010111" + "1"     + src1Field + src2Field + "010" + destField + "1010111", 2)
  )
  val copy_insts = Array(
    "VMV1R_V          " -> BigInt("100111" + "1"     + src1Field + "00000"   + "011" + destField + "1010111", 2),
    "VMV2R_V          " -> BigInt("100111" + "1"     + src1Field + "00001"   + "011" + destField + "1010111", 2),
    "VMV4R_V          " -> BigInt("100111" + "1"     + src1Field + "00011"   + "011" + destField + "1010111", 2),
    "VMV8R_V          " -> BigInt("100111" + "1"     + src1Field + "00111"   + "011" + destField + "1010111", 2)
  )
}