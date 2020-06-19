// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.
// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.

package vpu

import Chisel._
import chisel3.util.ImplicitConversions._
import util._

trait HasFPUParameters {
  require(FLEN == 32 || FLEN == 64)
  val FLEN: Int
  val FSEW16: Boolean
  val floatTypes = if(FSEW16) FType.all.filter(_.ieeeWidth <= FLEN)
                   else       FType.all.filter(i => 16 < i.ieeeWidth && i.ieeeWidth <= FLEN)
  val minType = floatTypes.head
  val maxType = floatTypes.last
  def prevType(t: FType) = floatTypes(typeTag(t) - 1)
  val maxExpWidth = maxType.exp
  val maxSigWidth = maxType.sig
  def typeTag(t: FType) = floatTypes.indexOf(t)

  private def isBox(x: UInt, t: FType): Bool = x(t.sig + t.exp, t.sig + t.exp - 4).andR

  private def box(x: UInt, xt: FType, y: UInt, yt: FType): UInt = {
    require(xt.ieeeWidth == 2 * yt.ieeeWidth)
    val swizzledNaN = Cat(
      x(xt.sig + xt.exp, xt.sig + xt.exp - 3),
      x(xt.sig - 2, yt.recodedWidth - 1).andR,
      x(xt.sig + xt.exp - 5, xt.sig),
      y(yt.recodedWidth - 2),
      x(xt.sig - 2, yt.recodedWidth - 1),
      y(yt.recodedWidth - 1),
      y(yt.recodedWidth - 3, 0))
    Mux(xt.isNaN(x), swizzledNaN, x)
  }

  // implement NaN unboxing for FU inputs
  def unbox(x: UInt, tag: UInt, exactType: Option[FType]): UInt = {
    val outType = exactType.getOrElse(maxType)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == minType) {
          Seq()
        } else {
          val prevT = prevType(t)
          val unswizzled = Cat(
            x(prevT.sig + prevT.exp - 1),
            x(t.sig - 1),
            x(prevT.sig + prevT.exp - 2, 0))
          val prev = helper(unswizzled, prevT)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = helper(x, maxType).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux(oks(tag), floats(tag), maxType.qNaN)
    } else {
      val t = exactType.get
      floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
    }
  }

  // make sure that the redundant bits in the NaN-boxed encoding are consistent
  def consistent(x: UInt): Bool = {
    def helper(x: UInt, t: FType): Bool = if (typeTag(t) == 0) true.B else {
      val prevT = prevType(t)
      val unswizzled = Cat(
        x(prevT.sig + prevT.exp - 1),
        x(t.sig - 1),
        x(prevT.sig + prevT.exp - 2, 0))
      val prevOK = !isBox(x, t) || helper(unswizzled, prevT)
      val curOK = !t.isNaN(x) || x(t.sig + t.exp - 4) === x(t.sig - 2, prevT.recodedWidth - 1).andR
      prevOK && curOK
    }
    helper(x, maxType)
  }

  // generate a NaN box from an FU result
  def box(x: UInt, t: FType): UInt = {
    if (t == maxType) {
      x
    } else {
      val nt = floatTypes(typeTag(t) + 1)
      val bigger = box(UInt((BigInt(1) << nt.recodedWidth)-1), nt, x, t)
      bigger | UInt((BigInt(1) << maxType.recodedWidth) - (BigInt(1) << nt.recodedWidth))
    }
  }

  // generate a NaN box from an FU result
  def box(x: UInt, tag: UInt): UInt = {
    val opts = floatTypes.map(t => box(x, t))
    opts(tag)
  }

  // zap bits that hardfloat thinks are don't-cares, but we do care about
  def sanitizeNaN(x: UInt, t: FType): UInt = {
    if (typeTag(t) == 0) {
      x
    } else {
      val maskedNaN = x & ~UInt((BigInt(1) << (t.sig-1)) | (BigInt(1) << (t.sig+t.exp-4)), t.recodedWidth)
      Mux(t.isNaN(x), maskedNaN, x)
    }
  }

  // implement NaN boxing and recoding for FL*/fmv.*.x
  def recode(x: UInt, tag: UInt): UInt = {
    def helper(x: UInt, t: FType): UInt = {
      if (typeTag(t) == 0) {
        t.recode(x)
      } else {
        val prevT = prevType(t)
        box(t.recode(x), t, helper(x, prevT), prevT)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.map(t => UInt((BigInt(1) << maxType.ieeeWidth) - (BigInt(1) << t.ieeeWidth)))
    helper(boxes(tag) | x, maxType)
  }

  // implement NaN unboxing and un-recoding for FS*/fmv.x.*
  def ieee(x: UInt, t: FType = maxType): UInt = {
    if (typeTag(t) == 0) {
      t.ieee(x)
    } else {
      val unrecoded = t.ieee(x)
      val prevT = prevType(t)
      val prevRecoded = Cat(
        x(prevT.recodedWidth-2),
        x(t.sig-1),
        x(prevT.recodedWidth-3, 0))
      val prevUnrecoded = ieee(prevRecoded, prevT)
      Cat(unrecoded >> prevT.ieeeWidth, Mux(t.isNaN(x), prevUnrecoded, unrecoded(prevT.ieeeWidth-1, 0)))
    }
  }
}
