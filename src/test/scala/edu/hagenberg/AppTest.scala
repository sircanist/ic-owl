package edu.hagenberg


import org.scalatest.funsuite.AnyFunSuite
import edu.hagenberg.CLI
import edu.hagenberg.hst.{BFS, SearchIterator}

class Szenario1 extends AnyFunSuite   {
  val baseOntologyPath = "owls/cti2.owl"
  val aboxSharePath = "owls/cti2-scenario1.owl"
  val attackerBkPath = "owls/cti2-scenario1-bk.owl"
  val attackerPolicyPath = "owls/cti2-scenario1-policy.owl"
  val searchMethod: SearchIterator = BFS
  val iriMappersPath = "owls/cti2-iri-mapper.txt"
  val weaken = true
  test("test-example") {
    print("test-example")
    Main.main(baseOntologyPath,
      aboxSharePath,
      attackerBkPath,
      attackerPolicyPath,
      searchMethod,
      iriMappersPath,
      weaken,
      stopAfter = 1)
  }
  test("test-all-no-weaken") {
    print("test-all-no-weaken")
    Main.main(baseOntologyPath,
      aboxSharePath,
      attackerBkPath,
      attackerPolicyPath,
      searchMethod,
      iriMappersPath,
      weaken=false)
  }
  test("test-all-weaken") {
    print("test-all-weaken")
    Main.main(baseOntologyPath,
      aboxSharePath,
      attackerBkPath,
      attackerPolicyPath,
      searchMethod,
      iriMappersPath,
      weaken)
  }
}


class Szenario2 extends AnyFunSuite   {
  val baseOntologyPath = "owls/cti2.owl"
  val aboxSharePath = "owls/cti2-scenario2.owl"
  val attackerBkPath = "owls/cti2-scenario2-bk.owl"
  val attackerPolicyPath = "owls/cti2-scenario2-policy.owl"
  val searchMethod: SearchIterator = BFS
  val iriMappersPath = "owls/cti2-iri-mapper.txt"
  val weaken = true
  test("test-example") {
    print("test-example")
    Main.main(baseOntologyPath,
      aboxSharePath,
      attackerBkPath,
      attackerPolicyPath,
      searchMethod,
      iriMappersPath,
      weaken,
      stopAfter = 1)
  }
  test("test-all-no-weaken") {
    print("test-all-no-weaken")
    Main.main(baseOntologyPath,
      aboxSharePath,
      attackerBkPath,
      attackerPolicyPath,
      searchMethod,
      iriMappersPath,
      weaken=false)
  }
  test("test-all-weaken") {
    print("test-all-weaken")
    Main.main(baseOntologyPath,
      aboxSharePath,
      attackerBkPath,
      attackerPolicyPath,
      searchMethod,
      iriMappersPath,
      weaken)
  }
}