package edu.hagenberg

import edu.hagenberg.Util.createManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner._
import uk.ac.manchester.cs.owlapi.modularity.{ModuleType, SyntacticLocalityModuleExtractor}

import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}

trait Checker[E, I]{
  def getEntailment: E
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
  var m: OWLOntologyManager = createManager
  val signatures: java.util.Set[OWLEntity]  = entailment.asScala.flatMap(_.getSignature.asScala).asJava
  val reasoner_config = new SimpleConfiguration(
    new NullReasonerProgressMonitor, FreshEntityPolicy.ALLOW, timeOutMS, IndividualNodeSetPolicy.BY_SAME_AS)



  override def isTautology: Boolean = {
    isEntailed(Set.empty)
  }

  override def getModule(input: Set[OWLAxiom]): Set[OWLAxiom] = {
    val moduleType = ModuleType.STAR
    val jInput: java.util.Set[OWLAxiom] = input.asJava
    val extractor = new SyntacticLocalityModuleExtractor(createManager,jInput.stream() , moduleType)
    extractor.extract(signatures).asScala.toSet
  }

  override def getEntailment: java.util.Set[OWLAxiom] = entailment

  override def getStatic: Set[OWLAxiom] = static
}



class AxiomsAtOnceChecker(reasonerFactory: OWLReasonerFactory,
                             entailment: java.util.Set[OWLAxiom],
                             static: Set[OWLAxiom],
                             timeOutMS: Long = Long.MaxValue) extends OWLSetChecker(reasonerFactory, entailment, static, timeOutMS){

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    m = createManager
    val ont: OWLOntology = m.createOntology(input.asJava)
    val check_entailed = (axiom: java.util.Set[OWLAxiom]) => reasonerFactory.createReasoner(ont,reasoner_config)
      .isEntailed(axiom)

    val entailed = check_entailed(entailment)
    m.removeOntology(ont)
    entailed
  }
}

class AnyAxiomChecker(reasonerFactory: OWLReasonerFactory,
                    entailment: java.util.Set[OWLAxiom],
                    static: Set[OWLAxiom],
                    timeOutMS: Long = Long.MaxValue) extends OWLSetChecker(reasonerFactory, entailment, static, timeOutMS){

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    m = createManager
    val ont: OWLOntology = m.createOntology(input.asJava)
    val check_entailed = (axiom: OWLAxiom) => reasonerFactory.createReasoner(ont,reasoner_config)
      .isEntailed(axiom)

    val entailed = entailment.parallelStream.anyMatch((axiom: OWLAxiom) =>  check_entailed(axiom))
    m.removeOntology(ont)
    entailed
  }
}


class AnyAxiomCEChecker(reasonerFactory: OWLReasonerFactory,
                      entailment: java.util.Set[OWLAxiom],
                      static: Set[OWLAxiom],
                      timeOutMS: Long = Long.MaxValue) extends OWLSetChecker(reasonerFactory, entailment, static, timeOutMS){

  val ces = entailment.asScala.map( axiom => axiom.asInstanceOf[OWLClassAssertionAxiom].getClassExpression)

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    //println("checking if is entailed")
    //val t1 = System.nanoTime
    m = createManager
    val ont: OWLOntology = m.createOntology(input.asJava)
    val factory = reasonerFactory.createReasoner(ont, reasoner_config)
    val entailed = ces.exists(ce => !factory.getInstances(ce).isEmpty)

    /*val check_entailed = (axiom: OWLAxiom) => {

      // this silently skips non class expressions, risky
      val ce =
        if (axiom.getAxiomType.equals(AxiomType.CLASS_ASSERTION))
         axiom.asInstanceOf[OWLClassAssertionAxiom].getClassExpression
        else null
      //ont.saveOntology(new FunctionalSyntaxDocumentFormat, System.out)
      if (ce != null) {
        val instances = reasonerFactory.createReasoner(ont,reasoner_config).getInstances(ce, false)
        return !instances.isEmpty
      }
      else {
        print("OMG here we go")
        return false
      }
    }
    val entailed = entailment.parallelStream.anyMatch((axiom: OWLAxiom) =>  check_entailed(axiom))*/

    m.removeOntology(ont)
    factory.dispose()
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