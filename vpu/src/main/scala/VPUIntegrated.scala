// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VPUIntegrated.scala
*       Author          :       liangzh
*       Revision        :       2020/01/07
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       parameters for VPU
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

case class VMulDivParams(
  mulUnroll: Int = 1,
  divUnroll: Int = 1,
  mulEarlyOut: Boolean = false,
  divEarlyOut: Boolean = false
)

case class VPUParams(
  VLEN:        Int = 32,
  ELEN:        Int = 32,
  SELEN:       Int = 8,
  XLEN:        Int = 32,
  FLEN:        Int = 32,
  ZFINX:       Boolean = true,
  FSEW16:      Boolean = false,
  FSEWMAX:     Int = 32,
  LMULMAX:     Int = 8,
  MERGE:       Boolean = false,
  MULDIV:      Boolean = false,
  MULADD:      Boolean = false,
  QMULADD:     Boolean = false,
  mulDiv:      Option[Array[VMulDivParams]] = Some(Array.fill(8)(VMulDivParams())),
  RED:         Boolean = false,
  MV:          Boolean = false,
  SATADD:      Boolean = false,
  AVERADD:     Boolean = false,
  SATMUL:      Boolean = false,
  SCALESR:     Boolean = false,
  NCLIP:       Boolean = false,
  SLIDE:       Boolean = false,
  GATHER:      Boolean = false,
  COMPRESS:    Boolean = false,
  COPY:        Boolean = false,
  FMA:         Boolean = false,
  FCVT:        Boolean = false,
  FCMP:        Boolean = false,
  FSGNJ:       Boolean = false,
  FCLASS:      Boolean = false,
  FMERGE:      Boolean = false,
  FMV:         Boolean = false,
  FDIVSQRT:    Boolean = false,
  FRED:        Boolean = false,
  SEGLS:       Boolean = false,
  AMO:         Boolean = false,
  EDIV:        Boolean = false,
  DOT:         Boolean = false,
  FDOT:        Boolean = false
)

