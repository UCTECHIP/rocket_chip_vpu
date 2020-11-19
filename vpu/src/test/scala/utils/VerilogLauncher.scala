// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package vpu

import firrtl._
import chisel3.stage._

object VerilogLauncher extends CustomConfigParams {
  val circuits = Map(
    "VDecode" -> { (args: Array[String]) => (new chisel3.stage.ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new VDecode(customConfig)))) }
  )

  def main(args: Array[String]): Unit = {
    if(args.nonEmpty) {
      for(circuitName <- args) {
        circuits.get(circuitName) match {
          case Some(v) => {
            val generateArgs = Array("-X", "verilog", "-td", s"test_run_dir/$circuitName")
            v(generateArgs)
          }
          case _ => println(s"Project name not found: $circuitName")
        }
      }
    } else {
      println("\nAvailable projects:")
      for(x <- circuits.keys) {
        println("  " + x)
      }
      System.exit(0)
    }
  }
}