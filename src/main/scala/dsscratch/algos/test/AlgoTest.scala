package dsscratch.algos.test

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.runners.TopologyRunner


object AlgoTest {
  def verboseTest(name: String, t: Topology, endCondition: () => Boolean,
    algo: AlgoCode, systemModel: SystemModel, timeout: Int = 0) = {
    runTest(name, t, endCondition, algo, systemModel, timeout, true)
  }

  def test(name: String, t: Topology, endCondition: () => Boolean,
    algo: AlgoCode, systemModel: SystemModel, timeout: Int = 0) = {
    runTest(name, t, endCondition, algo, systemModel, timeout)
  }

  private def printTestHeading(name: String): Unit = {
    val boundary = (1 to (name.size + 21)).foldLeft("")((x, y) => x + "|")
    println("\n" + boundary)
    println("  " + name + " Test starting...")
    println(boundary + "\n")
  }

  private def runTest(name: String, t: Topology, endCondition: () => Boolean,
    algo: AlgoCode, systemModel: SystemModel, timeout: Int,
    verbose: Boolean = false) = {

    printTestHeading(name)

    // Run the algorithm
    TopologyRunner(t, endCondition, systemModel, timeout).run()

    if (verbose) {
      println("//TRACE")
      print(Topology.trace(t))

      println("//NETWORK")
      println(Topology.dotgraph(t))

      println("DETAILED TEST RESULTS")
      println(Draw.asStrings(t.nodes.map(_.resultFor(algo))))
    }

    val passed = t.nodes.forall(pass(_, algo))

    val boundary = (1 to (name.size + 18)).foldLeft("")((x, y) => x + "-")
    println(boundary)
    print("  " + name + ":  ")
    if (passed)
      print(Console.GREEN + s"TEST PASSED\n" + Console.RESET)
    else
      print(Console.RED + s"TEST FAILED\n" + Console.RESET)
    println(boundary)
  }

  private def pass(nd: Node, algoCode: AlgoCode): Boolean =
    nd.testCodeFor(algoCode) == TestCodes.SUCCESS
}
