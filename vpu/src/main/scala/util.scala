// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.
// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.

package vpu

import chisel3._
import chisel3.util._
import scala.math._

object util {
  implicit def uintToBitPat(x: UInt): BitPat = BitPat(x)

  implicit class UIntIsOneOf(val x: UInt) extends AnyVal {
    def isOneOf(s: Seq[UInt]): Bool = s.map(x === _).reduce(_||_)
  
    def isOneOf(u1: UInt, u2: UInt*): Bool = isOneOf(u1 +: u2.toSeq)
  }

  implicit class IntToAugmentedInt(val x: Int) extends AnyVal {
    // exact log2
    def log2: Int = {
      require(isPow2(x))
      log2Ceil(x)
    }
  }

  implicit class UIntToAugmentedUInt(val x: UInt) extends AnyVal {
    def sextTo(n: Int): UInt = {
      require(x.getWidth <= n)
      if (x.getWidth == n) x
      else Cat(Fill(n - x.getWidth, x(x.getWidth-1)), x)
    }

    def extract(hi: Int, lo: Int): UInt = {
      if (hi == lo-1) 0.U
      else x(hi, lo)
    }
  }

  implicit class SeqToAugmentedSeq[T <: Data](val x: Seq[T]) extends AnyVal {
    def apply(idx: UInt): T = {
      if (x.size == 1) {
        x.head
      } else if (!isPow2(x.size)) {
        // For non-power-of-2 seqs, reflect elements to simplify decoder
        (x ++ x.takeRight(x.size & -x.size)).toSeq(idx)
      } else {
        // Ignore MSBs of idx
        val truncIdx =
          if (idx.isWidthKnown && idx.getWidth <= log2Ceil(x.size)) idx
          else (idx | 0.U(log2Ceil(x.size).W))(log2Ceil(x.size)-1, 0)
        (x.head /: x.zipWithIndex.tail) { case (prev, (cur, i)) => Mux(truncIdx === i.U, cur, prev) }
      }
    }

    def asUInt(): UInt = Cat(x.map(_.asUInt).reverse)
  }

  implicit class BooleanToAugmentedBoolean(val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0

    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
  }

}

object Packing {
  def splat_q(n: Bits) = Fill(128/128, n.asUInt)
  def splat_d(n: Bits) = Fill(64/64, n.asUInt)
  def splat_w(n: Bits) = Fill(64/32, n.asUInt)
  def splat_h(n: Bits) = Fill(64/16, n.asUInt)
  def splat_b(n: Bits) = Fill(64/8, n.asUInt)

  def expand(n: Bits, mlen: Int): UInt = { 
    val width = mlen-1
    Cat(Fill(width, 0.U(width.W)), n)
  }
}


object DataGating {
  def dgate(valid: Bool, b: UInt) = Fill(b.getWidth, valid) & b
}

object HardFloatHelper {
  def recode_qp(n: Bits) = hardfloat.recFNFromFN(15, 113, n.asUInt)
  def recode_dp(n: Bits) = hardfloat.recFNFromFN(11, 53, n.asUInt)
  def recode_sp(n: Bits) = hardfloat.recFNFromFN(8, 24, n.asUInt)
  def recode_hp(n: Bits) = hardfloat.recFNFromFN(5, 11, n.asUInt)
  def ieee_qp(n: Bits) = hardfloat.fNFromRecFN(15, 113, n.asUInt)
  def ieee_dp(n: Bits) = hardfloat.fNFromRecFN(11, 53, n.asUInt)
  def ieee_sp(n: Bits) = hardfloat.fNFromRecFN(8, 24, n.asUInt)
  def ieee_hp(n: Bits) = hardfloat.fNFromRecFN(5, 11, n.asUInt)
}


object DataExtend {
  def SignExtend(x: UInt, xlength: Int, ylength: Int): UInt = {
    require(ylength > xlength)
    val ldiff = ylength - xlength
    val y = Cat(Fill(ldiff, x.head(1)), x)
    y
  }

  def UnsignExtend(x: UInt, xlength: Int, ylength: Int): UInt = {
    require(ylength > xlength)
    val ldiff = ylength - xlength
    val y = Cat(0.U(ldiff.W), x)
    y
  }
}

