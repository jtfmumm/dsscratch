package dsscratch.modules.test

import dsscratch.modules._
import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.topology._

"""
ModuleTest is used to construct and run tests. At the minimum,
you must specify a test module that will return a TestCode via
its testCode method indicating whether the test passed or failed.
ModuleTest considers a test to have passed if the test module on
every node returns TestCodes.SUCCESS.

You can also specify one or more modules that will be used to check
for termination (if there are more than one, ModuleTest will wait
until they all terminate). If you provide zero termination modules,
then the test will run until a provided timeout is reached.
"""


object ModuleTest {
  def apply(name: String): ModuleTest = new ModuleTest(name)
}

class ModuleTest(val name: String)  {
  private var maxRuns = 1
  private var remaining = 1
  private var testModule: ModuleCode = ModuleCodes.NONE
  private var terminationModules: Seq[ModuleCode] = Seq()
  private var topologyBuilder = TopologyBuilder(10, 0.125)
  private var systemModel = SystemModel()
  private var isVerbose = false
  private var timeout = 0

  // Specify the module that will be used to determine if
  // the test passed or failed.
  def testFor(testM: ModuleCode): ModuleTest = {
    testModule = testM
    this
  }

  // Specify zero or more modules to determine if the test is
  // terminated (zero means the test runs until a provided
  // timeout is reached).
  def terminateFor(terminationMs: Seq[ModuleCode]): ModuleTest = {
    terminationModules = terminationMs
    this
  }

  // Specify the number of nodes in the topology.
  def withNodeCount(count: Int): ModuleTest = {
    topologyBuilder = topologyBuilder.withNodeCount(count)
    this
  }

  // Specify a value between 0 and 1 that indicates how interconnected
  // the nodes in the topology are. Every topology has at least one
  // connected path, so a value of 0 means that there is only this path.
  // A value of 1 means every node is connected to every other node.
  def withEdgeDensity(density: Double): ModuleTest = {
    topologyBuilder = topologyBuilder.withEdgeDensity(density)
    this
  }

  // You can construct your own TopologyBuilder and pass it in here.
  // You probably would not use this in conjunction with .withNodeCount()
  // and .withEdgeDensity().
  def withTopologyBuilder(topologyB: TopologyBuilder): ModuleTest = {
    topologyBuilder = topologyB
    this
  }

  // Specify the system model for running the test. The default is
  // a no failure model without network partitions and with no message
  // delays.
  def withSystemModel(sModel: SystemModel): ModuleTest = {
    systemModel = sModel
    this
  }

  // Specify a timeout (in steps) for ending the test in case the
  // termination condition is not reached (or not specified).
  def withTimeout(t: Int): ModuleTest = {
    timeout = t
    this
  }

  // On a failing test, print out detailed information about what happened
  // during the run.
  def verbose(): ModuleTest = {
    isVerbose = true
    this
  }

  // Run the test n times.
  def run(n: Int = 1): Unit = {
    maxRuns = n
    remaining = maxRuns

    printTestHeading()

    if (testModule == ModuleCodes.NONE) {
      println("You must specify a test module via .testFor().")
      println("Skipped " + name)
    } else if (terminationModules.isEmpty && (timeout == 0)) {
      println("If you specify no termination condition then you must provide a timeout greater than 0.")
      println("Skipped " + name)
    } else {
      // We're good to run the tests
      if (isVerbose)
        runTests(true)
      else
        runTests()
    }
  }

  private def runTests(verbose: Boolean = false) = {
    var passed = false
    if (terminationModules.isEmpty) {
      println("Running with no explicit termination condition. Will run until timeout...")
    }
    var continuing = true
    while (remaining > 0 && continuing) {
      remaining -= 1
      val t = topologyBuilder.build()
      passed = runTest(t)
      if (!passed) {
        if (verbose) {
          printVerboseDetails(t)
        } else {
          printGraph(t)
          printNodeDetails(t)
        }
        continuing = false
      }
    }
    printTestResult(passed, maxRuns - remaining)
  }

  private def printTestHeading(): Unit = {
    val boundary = (1 to (name.size + 21)).foldLeft("")((x, y) => x + "|")
    println("\n" + boundary)
    println("  " + name + " Test starting...")
    println(boundary + "\n")
  }

  private def printTestResult(passed: Boolean, count: Int = 1) = {
    val boundary = (1 to (name.size + 18)).foldLeft("")((x, y) => x + "-")
    println(boundary)
    print("  " + name + ":  ")
    if (passed) {
      print(Console.GREEN + s"TEST PASSED\n" + Console.RESET)
      val suffix = if (count == 1) "" else "s"
      println("  Ran " + count + " time" + suffix)
    } else {
      print(Console.RED + s"TEST FAILED\n" + Console.RESET)
      println("  Failed on " + intToAdjective(count) + " run")
    }

    println(boundary)
  }

  private def printVerboseDetails(t: Topology) = {
    printTrace(t)
    printGraph(t)
    printNodeDetails(t)
  }

  private def printTrace(t: Topology) = {
    println("//TRACE")
    print(Topology.trace(t))
  }

  private def printGraph(t: Topology) = {
    println("//NETWORK")
    println(Topology.dotgraph(t))
  }

  private def printNodeDetails(t: Topology) = {
    println("DETAILED TEST RESULTS")
    println(Draw.asStrings(t.nodes.map(_.resultFor(testModule))))
  }

  private def runTest(t: Topology): Boolean = {
    def endCondition: Boolean = {
      if (terminationModules.isEmpty) return false
      // Check that every node is terminated for every termination
      // module specified.
      t.nodes.forall(nd => {
        terminationModules.forall(nd.terminatedFor(_))
      })
    }
    TopologyRunner(t, endCondition _, systemModel, timeout).run()
    // Return true if test passes, false otherwise
    t.nodes.forall(pass(_, testModule))
  }

  private def pass(nd: Node, algoModule: ModuleCode): Boolean =
    nd.testCodeFor(algoModule) == TestCodes.SUCCESS

  private def intToAdjective(i: Int): String = {
    var suffix = (i % 10) match {
      case 1 if (i % 100 != 11) => "st"
      case 2 if (i % 100 != 12) => "nd"
      case 3 if (i % 100 != 13) => "rd"
      case _ => "th"
    }
    i + suffix
  }
}
