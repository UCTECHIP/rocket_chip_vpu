// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

import chisel3._
import chisel3.util._

trait AsyncResetTrait extends Module {
  def AsyncResetRegInit[T <: Data](init: T): T = {
    val x = withReset(reset.asAsyncReset) { RegInit(init) }
    x
  }

  def AsyncResetRegNext[T <: Data](next: T, init: T): T = {
    val x = withReset(reset.asAsyncReset) { RegNext(next, init) }
    x
  }
}