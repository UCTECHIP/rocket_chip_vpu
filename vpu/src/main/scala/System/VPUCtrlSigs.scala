// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename             :       VPUCtrlSigs.scala
*       Author               :       liangzh
*       Revision             :       2020/10/28
*       Company              :       UC TECH IP
*       Description          :       control signals from decoder
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

class VPUCtrlSigs extends Bundle with VConsts {
  val isUnmasked     = Bool()
  val swap12         = Bool()
  val swap23         = Bool()
  val src1EEW        = UInt(Src1EEW_SZ.W)
  val isSrc1t2SEW    = Bool()
  val src2EEW        = UInt(Src2EEW_SZ.W)
  val isSrc2t2SEW    = Bool()
  val destEEW        = UInt(DestEEW_SZ.W)
  val sign           = Bool()
  val majFun         = UInt(MajFun_SZ.W)
  val addFun         = UInt(AddFun_SZ.W)
  val shiftDir       = Bool()
  val mulFun         = UInt(MulFun_SZ.W)
  val isRND          = Bool()
  val isSAT          = Bool()
  val wflags         = Bool()
  val fmaFun         = UInt(FMAFun_SZ.W)

  val funct6         = UInt(6.W)
  val vm             = UInt(1.W)
  val src2Field      = UInt(5.W)
  val src1Field      = UInt(5.W)
  val widthField     = UInt(3.W)
  val dstField       = UInt(5.W)

  val isCSRInst      = Bool()
  val isALInst       = Bool()
  val isLdInst       = Bool()
  val isStInst       = Bool()
  val isAMOInst      = Bool()
}