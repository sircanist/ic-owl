package edu.hagenberg

import edu.hagenberg.Util.{createManager, getManager}
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.{OWLFunctionalSyntaxFactory, OWLManager}
import org.semanticweb.elk.reasoner.ReasonerFactory
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner._
import uk.ac.manchester.cs.owlapi.modularity.{ModuleType, SyntacticLocalityModuleExtractor}

import java.util
import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}
import scala.collection.mutable

trait Checker[E, I]{
  def getStatic: Set[I]
  def isEntailed(input: Set[I]): Boolean
  def isTautology:Boolean
  def getModule(input: Set[I]): Set[I]
}

trait CheckerFactory[E, I]{
  def createChecker(entailment: E, static: Set[I]): Checker[E, I]
}


abstract class OWLSetChecker(reasonerFactory: OWLReasonerFactory,
                    entailment: java.util.Set[OWLAxiom],
                    static: Set[OWLAxiom],
                    timeOutMS: Long = Long.MaxValue) extends Checker[java.util.Set[OWLAxiom],OWLAxiom] {
  var m: OWLOntologyManager = getManager
  val module_manager: OWLOntologyManager = createManager
  val iriOntology: IRI = OWLFunctionalSyntaxFactory.IRI("reasoner")
  val reasoner_ontology: OWLOntology = {
    if (m.getOntology(iriOntology) == null)
      m.createOntology()
    else
      m.getOntology(iriOntology)
  }

  val signatures: java.util.Set[OWLEntity]  = entailment.asScala.flatMap(_.getSignature.asScala).asJava
  val reasoner_config = new SimpleConfiguration(
    new NullReasonerProgressMonitor, FreshEntityPolicy.ALLOW, timeOutMS, IndividualNodeSetPolicy.BY_SAME_AS)


  override def isTautology: Boolean = {
    isEntailed(Set.empty)
  }

  override def getModule(input: Set[OWLAxiom]): Set[OWLAxiom] = {
    val moduleType = ModuleType.STAR
    var jInput: java.util.Set[OWLAxiom] = input.asJava
    var extractor = new SyntacticLocalityModuleExtractor(module_manager,jInput.stream() , moduleType)
    val moduleSet = extractor.extract(signatures).asScala.toSet
    extractor = null
    jInput = null
    module_manager.clearOntologies()
    moduleSet
  }

  override def getStatic: Set[OWLAxiom] = static
}



class AxiomsAtOnceChecker(reasonerFactory: OWLReasonerFactory,
                             entailment: java.util.Set[OWLAxiom],
                             static: Set[OWLAxiom],
                             timeOutMS: Long = Long.MaxValue) extends OWLSetChecker(reasonerFactory, entailment, static, timeOutMS){

  val filtered_entailment: util.Set[OWLAxiom] = entailment.asScala
    .filter(axiom => axiom.isInstanceOf[OWLIndividualAxiom]).asJava

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    val ont: OWLOntology = m.createOntology(input.asJava)
    val check_entailed = (axiom: java.util.Set[OWLAxiom]) => {
      val reasoner = reasonerFactory.createNonBufferingReasoner(ont,reasoner_config)
      val is_entailed = reasoner.isEntailed(axiom)
      reasoner.dispose()
      is_entailed
    }

    val entailed = check_entailed(entailment)
    m.removeOntology(ont)
    entailed
  }
}

class AnyAxiomChecker(reasonerFactory: OWLReasonerFactory,
                    entailment: java.util.Set[OWLAxiom],
                    static: Set[OWLAxiom],
                    timeOutMS: Long = Long.MaxValue) extends OWLSetChecker(reasonerFactory, entailment, static, timeOutMS){

  val filtered_entailment: util.Set[OWLAxiom] = entailment.asScala
    .filter(axiom => axiom.isInstanceOf[OWLIndividualAxiom]).asJava

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    val ont: OWLOntology = m.createOntology(input.asJava)
    val check_entailed = (axiom: OWLAxiom) => {
      val reasoner = reasonerFactory.createReasoner(ont,reasoner_config)
      val is_entailed = reasoner.isEntailed(axiom)
      reasoner.dispose()
      is_entailed
    }

    val entailed = filtered_entailment.parallelStream.anyMatch((axiom: OWLAxiom) =>  check_entailed(axiom))
    m.removeOntology(ont)
    entailed
  }
}


class AnyAxiomCEChecker(reasonerFactory: OWLReasonerFactory,
                      entailment: java.util.Set[OWLAxiom],
                      static: Set[OWLAxiom],
                      timeOutMS: Long = Long.MaxValue) extends OWLSetChecker(reasonerFactory, entailment, static, timeOutMS){

  val ces: mutable.Set[OWLClassExpression] = entailment.asScala
    .filter(axiom => axiom.isInstanceOf[OWLClassAssertionAxiom])
    .map( axiom => axiom.asInstanceOf[OWLClassAssertionAxiom].getClassExpression)
  val factory2: OWLReasonerFactory = new ElkReasonerFactory

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    //println("checking if is entailed")
    //val t1 = System.nanoTime
    val inputJava = input.asJava
    reasoner_ontology.addAxioms(inputJava)
    var reasoner = reasonerFactory.createNonBufferingReasoner(reasoner_ontology, reasoner_config)
    //var reasoner2 = factory2.createNonBufferingReasoner(reasoner_ontology, reasoner_config)
    //val entailed = ces.par.exists(ce => !reasoner2.getInstances(ce).isEmpty || !reasoner.getInstances(ce).isEmpty)
    val entailed = ces.exists{
      ce => !reasoner.getInstances(ce).isEmpty}
    reasoner_ontology.removeAxioms(inputJava)
    reasoner.dispose()
    //reasoner2.dispose()
    reasoner = null
    //reasoner2 = null
    //println("checking if is entailed finished")
    //val duration = (System.nanoTime - t1) / 1e9d
    //println("entailment search took " + duration +"  input-size: "+ input.size + "  entailed = " + entailed)
    entailed
  }
}

//class SimpleCheckerFactory(reasonerFactory: OWLReasonerFactory, timeOutMS: Long = Long.MaxValue) extends CheckerFactory[java.util.Set[OWLAxiom], OWLAxiom] {
//  override def createChecker(entailment: util.Set[OWLAxiom], refutable: Set[OWLAxiom]): Checker[util.Set[OWLAxiom], OWLAxiom] = {
//    new SimpleChecker(reasonerFactory, entailment, refutable, timeOutMS)
//  }
//}

object CheckerFactory{
  def AxiomsAtOnceStrategy(reasonerFactory: OWLReasonerFactory, timeOutMS: Long = Long.MaxValue): CheckerFactory[java.util.Set[OWLAxiom], OWLAxiom] = {
    (entailment, refutable) =>
      new AxiomsAtOnceChecker(reasonerFactory, entailment, refutable, timeOutMS)
  }
  def AnyAxiomCheckerStrategy(reasonerFactory: OWLReasonerFactory, timeOutMS: Long = Long.MaxValue): CheckerFactory[java.util.Set[OWLAxiom], OWLAxiom] = {
    (entailment, refutable) =>
      new AnyAxiomChecker(reasonerFactory, entailment, refutable, timeOutMS)
  }
  def AnyAxiomCheckerCEStrategy(reasonerFactory: OWLReasonerFactory, timeOutMS: Long = Long.MaxValue): CheckerFactory[java.util.Set[OWLAxiom], OWLAxiom] = {
    (entailment, refutable) =>
      new AnyAxiomCEChecker(reasonerFactory, entailment, refutable, timeOutMS)
  }
}