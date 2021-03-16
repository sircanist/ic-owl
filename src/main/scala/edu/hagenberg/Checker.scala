package edu.hagenberg

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLAxiom, OWLDataFactory, OWLEntity, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.{FreshEntityPolicy, IndividualNodeSetPolicy, NullReasonerProgressMonitor, OWLReasonerFactory, SimpleConfiguration}
import uk.ac.manchester.cs.owlapi.modularity.{ModuleType, SyntacticLocalityModuleExtractor}

import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}


class SimpleChecker(reasonerFactory: OWLReasonerFactory, entailment: java.util.Set[OWLAxiom], timeOutMS: Long = Long.MaxValue) extends Checker[java.util.Set[OWLAxiom],OWLAxiom] {
  var m: OWLOntologyManager = createManager();
  val signatures: java.util.Set[OWLEntity]  = entailment.asScala.flatMap(_.getSignature.asScala).asJava
  val reasoner_config = new SimpleConfiguration(new NullReasonerProgressMonitor, FreshEntityPolicy.ALLOW, timeOutMS, IndividualNodeSetPolicy.BY_SAME_AS)


  def createManager(): OWLOntologyManager = {
    OWLManager.createOWLOntologyManager()
  }

  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    var entailed: Boolean = false
    val ont: OWLOntology = m.createOntology(input.asJava)
    val df: OWLDataFactory = m.getOWLDataFactory
    signatures.forEach{
      ent =>
        if (!ent.isBuiltIn){
          if (!ont.containsEntityInSignature(ent)){
            ont.addAxiom(df.getOWLDeclarationAxiom(ent))
          }
        }
    }
    val check_entailed = (axiom: OWLAxiom) => reasonerFactory.createReasoner(ont,reasoner_config)
      .isEntailed(axiom)

    entailed = input.asJava.parallelStream.anyMatch((axiom: OWLAxiom) =>  check_entailed(axiom))
    m.removeOntology(ont)
    entailed
  }

  override def isTautology: Boolean = {
    isEntailed(Set.empty)
  }

  override def getModule(input: Set[OWLAxiom]): Set[OWLAxiom] = {
    val moduleType = ModuleType.STAR
    val jInput: java.util.Set[OWLAxiom] = input.asJava
    val extractor = new SyntacticLocalityModuleExtractor(createManager(),jInput.stream() , moduleType)
    extractor.extract(signatures).asScala.toSet
  }

  override def getEntailment: java.util.Set[OWLAxiom] = entailment
}



class SimpleCheckerFactory(reasonerFactory: OWLReasonerFactory, timeOutMS: Long = Long.MaxValue) extends CheckerFactory[java.util.Set[OWLAxiom], OWLAxiom] {

  override def createChecker(entailment: java.util.Set[OWLAxiom]): Checker[java.util.Set[OWLAxiom], OWLAxiom] = {
    new SimpleChecker(reasonerFactory, entailment, timeOutMS)
  }
}