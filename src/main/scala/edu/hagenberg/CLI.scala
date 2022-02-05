package edu.hagenberg

import edu.hagenberg.Util.getAxiomsFromFile
import edu.hagenberg.hst.{BFS, DFS}
import openllet.owlapi.OpenlletReasonerFactory
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLDeclarationAxiom, OWLOntologyIRIMapper}
import org.semanticweb.owlapi.util.SimpleIRIMapper

import java.io.File
import scala.io.Source
import scala.collection.JavaConverters._

/**
 * Hello world!
 *
 */

object CLI extends App {

  def createIriMappers(path: String): List[OWLOntologyIRIMapper]  ={
    val lines = Source.fromFile(path).getLines()
    lines.map{
      line =>
        val split_line = line.split(" ")
        if (split_line.length != 2) {
          println("iri-mapping has wrong format, must be \"iri \"file-path")
          System.exit(-1)
        }
        val iri = split_line(0)
        val path = split_line(1)
        new SimpleIRIMapper(IRI.create(iri), IRI.create("file:///" + path))
    }.toList
  }

  Console.println("Commandline-Arguments: " + (args mkString ", "))
  if (args.length != 4 && args.length != 5)
  {
    println("Supply path for 4 ontologies in form: base-ontology, abox-share, attacker-bk attacker-policy")
    println("Additionally provide an argument for an iri-mapping to ontology with lines in form \"iri filepath\"")
    println("spaces are not allowed atm for iri-mapping")
    System.exit(-1)
  }
  val baseOntologyPath = args(0)
  val aboxSharePath = args(1)
  val attackerBkPath = args(2)
  val attackerPolicyPath = args(3)
  val iriMappers =
    if (args.length == 5)
      createIriMappers(args(4))
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
    algorithm = Algorithm.hittingSet(true, BFS)
    //algorithm = Algorithm.simple
    //algorithm = Algorithm.simpleWeakening
  )
  val remove_axioms = generator.executeAlgorithm(entailment)
  remove_axioms match {
    case Left(s) => println(s"ERROR: ${s.getMessage}")
    case Right(paths) => {
      paths.foreach(
        pathelements => {
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
        }
      )
    }
  }
  System.exit(0)
}
