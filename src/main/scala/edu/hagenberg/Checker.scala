package edu.hagenberg

import edu.hagenberg.Util.createManager
import org.semanticweb.owlapi.model.{OWLAxiom, OWLDataFactory, OWLEntity, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.{FreshEntityPolicy, IndividualNodeSetPolicy, NullReasonerProgressMonitor, OWLReasonerFactory, SimpleConfiguration}
import uk.ac.manchester.cs.owlapi.modularity.{ModuleType, SyntacticLocalityModuleExtractor}

import java.util
import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}


class SimpleChecker(reasonerFactory: OWLReasonerFactory,
                    entailment: java.util.Set[OWLAxiom],
                    static: Set[OWLAxiom],
                    timeOutMS: Long = Long.MaxValue) extends Checker[java.util.Set[OWLAxiom],OWLAxiom] {
  var m: OWLOntologyManager = createManager();
  val signatures: java.util.Set[OWLEntity]  = entailment.asScala.flatMap(_.getSignature.asScala).asJava
  val reasoner_config = new SimpleConfiguration(
    new NullReasonerProgressMonitor, FreshEntityPolicy.ALLOW, timeOutMS, IndividualNodeSetPolicy.BY_SAME_AS)


  override def isEntailed(input: Set[OWLAxiom]): Boolean = {
    m = createManager();
    var entailed: Boolean = false
    val ont: OWLOntology = m.createOntology(input.asJava)
    val df: OWLDataFactory = m.getOWLDataFactory
//    signatures.forEach{
//      ent =>
//        if (!ent.isBuiltIn){
//          if (!ont.containsEntityInSignature(ent)){
//            ont.addAxiom(df.getOWLDeclarationAxiom(ent))
//          }
//        }
//    }
    val check_entailed = (axiom: OWLAxiom) => reasonerFactory.createReasoner(ont,reasoner_config)
      .isEntailed(axiom)

    entailed = entailment.parallelStream.anyMatch((axiom: OWLAxiom) =>  check_entailed(axiom))
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

  override def getStatic: Set[OWLAxiom] = static
}



class SimpleCheckerFactory(reasonerFactory: OWLReasonerFactory, timeOutMS: Long = Long.MaxValue) extends CheckerFactory[java.util.Set[OWLAxiom], OWLAxiom] {
  override def createChecker(entailment: util.Set[OWLAxiom], refutable: Set[OWLAxiom]): Checker[util.Set[OWLAxiom], OWLAxiom] = {
    new SimpleChecker(reasonerFactory, entailment, refutable, timeOutMS)
  }
}