package edu.hagenberg

import edu.hagenberg.Util.getAxiomsFromFile
import openllet.owlapi.OpenlletReasonerFactory
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLDeclarationAxiom, OWLOntologyIRIMapper}
import org.semanticweb.owlapi.util.SimpleIRIMapper

import java.io.{File, FileOutputStream}
import scala.collection.JavaConverters._
import scala.io.Source

/**
 * Hello world!
 *
 */

object CLI extends App {

  def createIriMappers(path: String): List[OWLOntologyIRIMapper]  ={
    println("Fetching iris from file: " + path)
    val lines = Source.fromFile(path).getLines()
    lines.map{
      line =>
        val split_line = line.split(" ")
        if (split_line.length != 2) {
          println("iri-mapping has wrong format, must be \"iri \"file-path")
          System.exit(-1)
        }
        val iri = split_line(0)
        var path = split_line(1)
        if (!path.startsWith("/"))
          path = System.getProperty("user.dir").concat("/").concat(path)
        else if (!path.startsWith("."))
          path = System.getProperty("user.dir").concat("/").concat(path.substring(1))
        new SimpleIRIMapper(IRI.create(iri), IRI.create("file:///" + path))
    }.toList
  }

  Console.println("Commandline-Arguments: " + (args mkString ", "))
  if (args.length != 7 && args.length != 6)
  {
    println("Supply path for 4 ontologies in form: base-ontology[*.owl], abox-share[*.owl], attacker-bk[*.owl] attacker-policy[*.owl]")
    println("then: SearchMethod[BFS||DFS] number-of-/ic-owl/repairs[1-n]  ")
    println("Additionally provide an argument for an iri-mapping to ontology with lines in form \"iri filepath\"")
    println("spaces are not allowed atm for iri-mapping")
    System.exit(-1)
  }
  val baseOntologyPath = args(0)
  val aboxSharePath = args(1)
  val attackerBkPath = args(2)
  val attackerPolicyPath = args(3)
  val searchMethodString = args(4)
  val stopAfter = args(5).toInt
  val searchMethod = {
    if (searchMethodString.equals("BFS"))
      edu.hagenberg.hst.BFS
    else edu.hagenberg.hst.DFS
  }
  val iriMappers =
    if (args.length == 7)
      createIriMappers(args(6))
    else
      null

  val base_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(baseOntologyPath), iriMappers)
  val attacker_knowledge_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(attackerBkPath), iriMappers)
  val cti_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(aboxSharePath), iriMappers)
  val unwanted_ontology_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(attackerPolicyPath), iriMappers)

  val static_without_cti: Set[OWLAxiom] = base_axioms ++ attacker_knowledge_axioms
  val input: Set[OWLAxiom] = static_without_cti ++ cti_axioms
  //val static: Set[OWLAxiom] = static_without_cti ++ cti_axioms.intersect(static_without_cti)
  val static: Set[OWLAxiom] = static_without_cti // removing intersections leads to also possible delete instances

  val entailment = (unwanted_ontology_axioms -- static).filter(axiom => !axiom.isInstanceOf[OWLDeclarationAxiom]).asJava

  val generator: BlackBoxGenerator[java.util.Set[OWLAxiom], OWLAxiom] = new BlackBoxGenerator(
    input,
    static,
    checkerFactory = CheckerFactory.AnyAxiomCheckerCEStrategy(new OpenlletReasonerFactory),
    reasonerFactory = new OpenlletReasonerFactory,
    expansionStrategy = ExpansionStrategy.structuralExpansionStrategy,
    contractionStrategy = ContractionStrategy.newSlidingContractionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
    algorithm = Algorithm.hittingSet(true, searchMethod, stop_after = stopAfter)
    //algorithm = Algorithm.simple
    //algorithm = Algorithm.simpleWeakening
  )
  val remove_axioms = generator.executeAlgorithm(entailment)
  remove_axioms match {
    case Left(s) => println(s"ERROR: ${s.getMessage}")
    case Right(paths) => {
      val distinct_paths = paths.map(path => path.flatMap { pe =>
        val pathSet = Set(pe.selected)
        if (pe.weakened.isDefined)
          pathSet + pe.weakened.get
        else
          pathSet
      }).distinct

//       val axioms_removed = paths.flatMap(path => path.map { pe =>
//         pe.selected
//       }).distinct
//       val axioms_added = paths.flatMap(path => path.map { pe =>
//         pe.weakened
//       }.filter(_.isDefined).map(_.get)).distinct
//       val tmp_ont: Set[OWLAxiom] =
//         cti_axioms -- axioms_removed ++ axioms_added
//       val outputStream = new FileOutputStream(new File("/tmp/test.tttl"))
//       Util.createManager.createOntology((tmp_ont.asJava)).saveOntology(outputStream)

      paths.zipWithIndex.foreach{
       case (pathelements, idx) =>  {
          println("\n\nNew path")
          pathelements.foreach {
            path => {
              println("{")
              println("Justifications:\n " + path.justifications)
              println("selected:\n " + path.selected)
              println("weakened:\n " + path.weakened)
              println("}")
            }
          }



          val removed_axioms: Set[OWLAxiom] = pathelements.map(e => e.selected).toSet
          val added_axioms: Set[OWLAxiom] = pathelements.map(_.weakened).filter(_.isDefined).map(_.get).toSet
          val file = new File("repair/" + idx + ".owl")
          val outputStream = new FileOutputStream(file)
          val tmp_ont: Set[OWLAxiom] =
            cti_axioms -- removed_axioms ++ added_axioms
          Util.createManager.createOntology((tmp_ont.asJava)).saveOntology(outputStream)
          outputStream.close()
       }
      }
    }
  }
  println("Finished")
  System.exit(0)
}
