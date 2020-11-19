// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

import chisel3.iotesters._
import VInsts._

class VDecodeTests(c: VDecode) extends VDecodeTable(c) {
  val insts = csr_insts ++ al_insts ++ 
              (if(c.MERGE)    merge_insts    else Nil) ++ 
              (if(c.MUL)      mul_insts      else Nil) ++ 
              (if(c.MULADD)   madd_insts     else Nil) ++ 
              (if(c.SATADD)   sadd_insts     else Nil) ++ 
              (if(c.AVERADD)  aadd_insts     else Nil) ++ 
              (if(c.SATMUL)   smul_insts     else Nil) ++ 
              (if(c.SCALESR)  ssr_insts      else Nil) ++ 
              (if(c.NCLIP)    nclip_insts    else Nil) ++ 
              (if(c.FMA)      fma_insts      else Nil) ++ 
              (if(c.FCVT)     fcvt_insts     else Nil) ++ 
              (if(c.FCMP)     fcmp_insts     else Nil) ++ 
              (if(c.FMINMAX)  fminmax_insts  else Nil) ++ 
              (if(c.FSGNJ)    fsgnj_insts    else Nil) ++ 
              (if(c.FCLASS)   fclass_insts   else Nil) ++ 
              (if(c.FMERGE)   fmerge_insts   else Nil) ++ 
              (if(c.FDIVSQRT) fdiv_insts     else Nil) ++ 
              (if(c.FRSQRT)   frsqrt_insts   else Nil) ++ 
              (if(c.FRECE)    frece_insts    else Nil) ++ 
              (if(c.RED)      red_insts      else Nil) ++ 
              (if(c.FRED)     fred_insts     else Nil) ++ 
              (if(c.POPC)     popc_insts     else Nil) ++ 
              (if(c.FIRST)    first_insts    else Nil) ++ 
              (if(c.MINDEX)   mindex_insts   else Nil) ++ 
              (if(c.IOTA)     iota_insts     else Nil) ++ 
              (if(c.INDEX)    index_insts    else Nil) ++ 
              (if(c.MBITWISE) mbit_insts     else Nil) ++ 
              (if(c.MV)       mv_insts       else Nil) ++ 
              (if(c.FMV)      fmv_insts      else Nil) ++ 
              (if(c.SLIDE)    slide_insts    else Nil) ++ 
              (if(c.GATHER)   gather_insts   else Nil) ++ 
              (if(c.COMPRESS) compress_insts else Nil) ++ 
              (if(c.COPY)     copy_insts     else Nil)

  val decode = csr_decode ++ al_decode ++
              (if(c.MERGE)    merge_decode    else Nil) ++ 
              (if(c.MUL)      mul_decode      else Nil) ++ 
              (if(c.MULADD)   madd_decode     else Nil) ++ 
              (if(c.SATADD)   sadd_decode     else Nil) ++ 
              (if(c.AVERADD)  aadd_decode     else Nil) ++ 
              (if(c.SATMUL)   smul_decode     else Nil) ++ 
              (if(c.SCALESR)  ssr_decode      else Nil) ++ 
              (if(c.NCLIP)    nclip_decode    else Nil) ++ 
              (if(c.FMA)      fma_decode      else Nil) ++ 
              (if(c.FCVT)     fcvt_decode     else Nil) ++ 
              (if(c.FCMP)     fcmp_decode     else Nil) ++ 
              (if(c.FMINMAX)  fminmax_decode  else Nil) ++ 
              (if(c.FSGNJ)    fsgnj_decode    else Nil) ++ 
              (if(c.FCLASS)   fclass_decode   else Nil) ++ 
              (if(c.FMERGE)   fmerge_decode   else Nil) ++ 
              (if(c.FDIVSQRT) fdiv_decode     else Nil) ++ 
              (if(c.FRSQRT)   frsqrt_decode   else Nil) ++ 
              (if(c.FRECE)    frece_decode    else Nil) ++ 
              (if(c.RED)      red_decode      else Nil) ++ 
              (if(c.FRED)     fred_decode     else Nil) ++ 
              (if(c.POPC)     popc_decode     else Nil) ++ 
              (if(c.FIRST)    first_decode    else Nil) ++ 
              (if(c.MINDEX)   mindex_decode   else Nil) ++ 
              (if(c.IOTA)     iota_decode     else Nil) ++ 
              (if(c.INDEX)    index_decode    else Nil) ++ 
              (if(c.MBITWISE) mbit_decode     else Nil) ++ 
              (if(c.MV)       mv_decode       else Nil) ++ 
              (if(c.FMV)      fmv_decode      else Nil) ++ 
              (if(c.SLIDE)    slide_decode    else Nil) ++ 
              (if(c.GATHER)   gather_decode   else Nil) ++ 
              (if(c.COMPRESS) compress_decode else Nil) ++ 
              (if(c.COPY)     copy_decode     else Nil)

  for(i <- insts) {
    println("Testing " + i._1)
    poke(c.io.inst, i._2)
    step(1)
    ExpectDecode(decode(i._1))
  }
}