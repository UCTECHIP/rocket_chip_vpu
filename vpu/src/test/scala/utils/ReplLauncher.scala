// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

import chisel3.iotesters.{Driver, ReplOptionsManager}
import utils.ProjectRepl

object ReplLauncher extends CustomConfigParams {
  val circuits = Map(
    "VDecode" -> { (manager: ReplOptionsManager) => Driver.executeFirrtlRepl(() => new VDecode(customConfig), manager) }
  )

  def main(args: Array[String]): Unit = {
    ProjectRepl(circuits, args)
  }
}
