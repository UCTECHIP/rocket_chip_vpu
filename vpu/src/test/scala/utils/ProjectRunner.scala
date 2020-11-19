// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
package utils

import scala.collection.mutable.ArrayBuffer
import chisel3.iotesters._

object OptionsCopy {
  def apply(t: TesterOptionsManager): TesterOptionsManager = {
    new TesterOptionsManager {
      testerOptions = t.testerOptions.copy()
      interpreterOptions = t.interpreterOptions.copy()
      chiselOptions = t.chiselOptions.copy()
      firrtlOptions = t.firrtlOptions.copy()
      treadleOptions = t.treadleOptions.copy()
    }
  }

  def apply(t: ReplOptionsManager): ReplOptionsManager = {
    new ReplOptionsManager {
      interpreterOptions = t.interpreterOptions.copy()
      chiselOptions = t.chiselOptions.copy()
      firrtlOptions = t.firrtlOptions.copy()
    }
  }
}

object ProjectRunner {
  def apply(projectMap: Map[String, TesterOptionsManager => Boolean], args: Array[String]): Unit = {
    var successful = 0
    val errors = new ArrayBuffer[String]

    val optionsManager = new TesterOptionsManager()
    optionsManager.doNotExitOnHelp()

    optionsManager.parse(args)

    val programArgs = optionsManager.commonOptions.programArgs

    if(programArgs.isEmpty) {
      println("\nAvailable projects:")
      for(x <- projectMap.keys) {
        println("  " + x)
      }
      println("  all")
      System.exit(0)
    }

    val problemsToRun = if(programArgs.exists(x => x.toLowerCase() == "all")) {
      projectMap.keys
    }
    else {
      programArgs
    }

    for(testName <- problemsToRun) {
      projectMap.get(testName) match {
        case Some(test) =>
          println(s"Starting project $testName")
          try {
            // Start with a (relatively) clean set of options.
            val testOptionsManager = OptionsCopy(optionsManager)
            testOptionsManager.setTopName(testName)
            testOptionsManager.setTargetDirName(s"test_run_dir/$testName")
            if(test(testOptionsManager)) {
              successful += 1
            }
            else {
              errors += s"Project $testName: test error occurred"
            }
          }
          catch {
            case exception: Exception =>
              exception.printStackTrace()
              errors += s"Project $testName: ${exception.getMessage}"
            case t : Throwable =>
              errors += s"Project $testName: throwable ${t.getMessage}"
          }
        case _ =>
          errors += s"Project name not found: $testName"
      }

    }
    if(successful > 0) {
      println(s"Projects passing: $successful")
    }
    if(errors.nonEmpty) {
      println("=" * 80)
      println(s"Errors: ${errors.length}: in the following projects")
      println(errors.mkString("\n"))
      println("=" * 80)
      System.exit(1)
    }
  }
}


object ProjectRepl {
  def apply(projectMap: Map[String, ReplOptionsManager => Boolean], args: Array[String]): Unit = {
    var successful = 0
    val errors = new ArrayBuffer[String]

    val optionsManager = new ReplOptionsManager()
    optionsManager.doNotExitOnHelp()

    optionsManager.parse(args)

    val programArgs = optionsManager.commonOptions.programArgs

    if(programArgs.isEmpty) {
      println("\nAvailable projects:")
      for(x <- projectMap.keys) {
        println("  " + x)
      }
      System.exit(0)
    }

    val problemsToRun = programArgs


    for(testName <- problemsToRun) {
      projectMap.get(testName) match {
        case Some(test) =>
          println(s"Starting project $testName")
          try {
            // Start with a (relatively) clean set of options.
            val testOptionsManager = OptionsCopy(optionsManager)
            testOptionsManager.setTopName(testName)
            testOptionsManager.setTargetDirName(s"test_run_dir/$testName")
            if(test(testOptionsManager)) {
              successful += 1
            }
            else {
              errors += s"Project $testName: test error occurred"
            }
          }
          catch {
            case exception: Exception =>
              exception.printStackTrace()
              errors += s"Project $testName: ${exception.getMessage}"
            case t : Throwable =>
              errors += s"Project $testName: throwable ${t.getMessage}"
          }
        case _ =>
          errors += s"Project name not found: $testName"
      }

    }
    if(successful > 0) {
      println(s"Projects passing: $successful")
    }
    if(errors.nonEmpty) {
      println("=" * 80)
      println(s"Errors: ${errors.length}: in the following projects")
      println(errors.mkString("\n"))
      println("=" * 80)
      System.exit(1)
    }
  }
}
