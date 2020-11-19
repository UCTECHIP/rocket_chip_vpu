// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VPUParams.scala
*       Author          :       liangzh
*       Revision        :       2020/10/28
*       Company         :       UC TECH IP
*       Description     :       parameters for VPU
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

case class VMulDivParams(
  mulUnroll: Int = 1,           //directly multiply how many bits for reduce rolling times
  divUnroll: Int = 1,           //directly divide how many bits for reduce rolling times
  mulEarlyOut: Boolean = false, //reduce rolling times when multiply special values or not
  divEarlyOut: Boolean = false  //reduce rolling times when divide special values or not
)

case class VPUParams(
  VLEN:        Int = 32,        //vector register width in bits
  ELEN:        Int = 32,        //max element size in bits 
  XLEN:        Int = 32,        //integer scalar size in bits in rocket-chip
  FLEN:        Int = 32,        //floating-point scalar size in bits in in fpu of rocket-chip
  ZFINX:       Boolean = false, //floating-point scalar come from GPR or not
  FSEW16:      Boolean = false, //support half-float or not, for both vpu and fpu, default to false by now
  FSEWMAX:     Int = 32,        //max floating-point element size, must be less or equal to ELEN

  RMUL:        Int = 1,         //multiples of RLEN; RLEN: base length of data in one time calculation, equals to ELEN if ELEN<=VLEN, VLEN otherwise

//CSR, ADD, EXT BITWISE, SHIFT, CMP, MINMAX, LOAD, STORE and AMO inst subsets are default supported
  MERGE:       Boolean = false, //support integer merge inst subset or not
  MULDIV:      Boolean = false, //support integer multiply and divide inst subsets or not, including widening variant
  mulDiv:      Option[Array[VMulDivParams]] = Some(Array.fill(8)(VMulDivParams())),
  MULADD:      Boolean = false, //support integer multiply-add subset or not, including widening variant

  SATADD:      Boolean = false, //support saturting add and subtract insts subset or not
  AVERADD:     Boolean = false, //support averaging add and subtract insts subset or not
  SATMUL:      Boolean = false, //support fractional multiply with rounding and saturation inst subset or not
  SCALESR:     Boolean = false, //support scaling shift inst subset or not
  NCLIP:       Boolean = false, //narrowing fixed-point clip inst subset or not

  FMA:         Boolean = false, //support floating-point add, muliply, multiply-add inst subsets or not, including widening variant
  FCVT:        Boolean = false, //support floating-point/integer type-convert inst subsets or not, including widening variant and narrowing variant
  FCMP:        Boolean = false, //support floating-point compare inst subset or not
  FMINMAX:     Boolean = false, //support floating-point min/max inst subset or not
  FSGNJ:       Boolean = false, //support floating-point sign-injection subset or not
  FCLASS:      Boolean = false, //support floating-point classify inst or not
  FMERGE:      Boolean = false, //support floating-point merge inst or not
  FDIVSQRT:    Boolean = false, //support floating-point divide and square-root inst subset or not
  FRSQRT:      Boolean = false, //support floating-point reciprocal square-root estimate inst or not
  FRECE:       Boolean = false, //support floating-point reciprocal estimate inst or not

  RED:         Boolean = false, //support integer reduction inst subset or not, including widening variant
  FRED:        Boolean = false, //support floating-point reduction inst subset or not, including widening variant

  POPC:        Boolean = false, //support mask population count inst or not
  FIRST:       Boolean = false, //support find-first-set mask bit inst or not
  MINDEX:      Boolean = false, //support set-before-first, set-including-first, set-only-first insts or not
  IOTA:        Boolean = false, //support iota inst or not
  INDEX:       Boolean = false, //support element index inst or not
  MBITWISE:    Boolean = false, //support mask-register logical inst subset or not

  MV:          Boolean = false, //support integer scalar move inst subset or not
  FMV:         Boolean = false, //support floating-point scalar move inst subset or not
  SLIDE:       Boolean = false, //support slide inst subset or not
  GATHER:      Boolean = false, //support register gather inst subset or not
  COMPRESS:    Boolean = false, //support compress inst or not
  COPY:        Boolean = false, //support whole vector register move inst subset or not
)

