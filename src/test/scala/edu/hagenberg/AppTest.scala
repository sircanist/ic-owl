package edu.hagenberg


import org.scalatest.funsuite.AnyFunSuite
import edu.hagenberg.hst.{BFS, SearchIterator}


class Szenario1 extends AnyFunSuite   {
  var n = 96
  def time[R](block: => R): List[Double] = {
    val times =  scala.collection.mutable.ListBuffer[Double]()
    1 to n foreach {
      _ =>
        val t0 = System.nanoTime()
        block // call-by-name
        val t1 = System.nanoTime()
        times += (t1 - t0) / 1e9d
    }
    times.toList
  }
  val baseOntologyPath = "owls/cti2.owl"
  val aboxSharePath = "owls/cti2-scenario1.owl"
  val attackerBkPath = "owls/cti2-scenario1-bk.owl"
  val attackerPolicyPath = "owls/cti2-scenario1-policy.owl"
  val searchMethod: SearchIterator = BFS
  val iriMappersPath = "owls/cti2-iri-mapper.txt"
  test("weaken-all") {
    val times = time {
      Main.main(baseOntologyPath,
        aboxSharePath,
        attackerBkPath,
        attackerPolicyPath,
        searchMethod,
        iriMappersPath,
        weaken=true,
        stopAfter = -1,
        create_files = false)
    }
    println(times)
  }
  test("noweaken-all") {
    val times = time {
      Main.main(baseOntologyPath,
        aboxSharePath,
        attackerBkPath,
        attackerPolicyPath,
        searchMethod,
        iriMappersPath,
        weaken = false,
        stopAfter = -1,
        create_files = false)
    }
    println(times)
  }
  test("weaken-one") {
    val times = time {
      Main.main(baseOntologyPath,
        aboxSharePath,
        attackerBkPath,
        attackerPolicyPath,
        searchMethod,
        iriMappersPath,
        weaken=true,
        stopAfter = 1,
        create_files = false)
    }
    println(times)
  }
  test("noweaken-one") {
    val times = time {
      Main.main(baseOntologyPath,
        aboxSharePath,
        attackerBkPath,
        attackerPolicyPath,
        searchMethod,
        iriMappersPath,
        weaken = false,
        stopAfter = 1,
        create_files = false)
    }
    println(times)
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