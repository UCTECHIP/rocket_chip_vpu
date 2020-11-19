// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

import chisel3.iotesters.{Driver, TesterOptionsManager}
import utils.ProjectRunner

object TestLauncher extends CustomConfigParams {
  val circuits = Map(
    "VDecode" -> { (manager: TesterOptionsManager) => Driver.execute(() => new VDecode(customConfig), manager){(c) => new VDecodeTests(c)} }
  )

  def main(args: Array[String]): Unit = {
    ProjectRunner(circuits, args)
  }
}
