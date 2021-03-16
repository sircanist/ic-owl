package edu.hagenberg

import org.semanticweb.owlapi.model.{OWLAxiom, OWLDataFactory, OWLEntity, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.{FreshEntityPolicy, IndividualNodeSetPolicy, NullReasonerProgressMonitor, OWLReasonerFactory, SimpleConfiguration}


trait Algorithm[E, I]{

  def findRemoveSet(input: Set[I], finder: JustificationFinder[E, I]): Option[Set[I]]
}


class JustificationFinder[E, I](checker: Checker[E,I],
                                expansionStrategy: ExpansionStrategy[E, I],
                                contractionStrategy: ContractionStrategy[E, I]){
  private[this] def expand(workingAxioms: Set[I]): Option[Set[I]] = {
    val module = checker.getModule(workingAxioms)
    expansionStrategy.doExpansion(module, checker)
  }

  private[this] def contract(expandedAxioms: Set[I]): Set[I] = {
    contractionStrategy.doPruning(expandedAxioms, checker)
  }

  def searchOneJustification(input: Set[I]): Option[Set[I]] = {
    expand(input) match {
      case Some(axioms) => Some(contract(axioms))
      case None => None
    }
  }
}

trait Checker[E, I]{
  def getEntailment: E
  def isEntailed(input: Set[I]): Boolean
  def isTautology:Boolean
  def getModule(input: Set[I]): Set[I]
}

trait CheckerFactory[E, I]{
  def createChecker(entailment: E): Checker[E, I]
}


trait ExpansionStrategy[E, I]{
  def doExpansion(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}


trait ContractionStrategy[E, I]{
  def doPruning(axioms: Set[I], checker: Checker[E, I] ): Set[I]
}



class BlackBoxGenerator[E, I](input: Set[I],
                              checkerFactory: CheckerFactory[E, I],
                              expansionStrategy: ExpansionStrategy[E,I],
                              contractionStrategy: ContractionStrategy[E, I],
                              algorithm: Algorithm[E, I],
                              useModularisation: Boolean = true){
  def executeAlgorithm(entailment: E): Option[Set[I]] ={
    val checker: Checker[E, I] = checkerFactory.createChecker(entailment)
    val finder: JustificationFinder[E, I] = new JustificationFinder(checker, expansionStrategy, contractionStrategy)
    val algorithmInput = if (useModularisation) checker.getModule(input) else input
    val removes: Option[Set[I]] = algorithm.findRemoveSet(algorithmInput, finder)
    removes
  }
}


