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
    val expanded = expansionStrategy.doExpansion(workingAxioms, checker)
    expanded
  }

  private[this] def contract(expandedAxioms: Set[I]): Option[Set[I]] = {
    val removeCandidates = expandedAxioms
    contractionStrategy.doPruning(removeCandidates, checker)
  }

  def searchOneJustification(input: Set[I]): Option[Set[I]] = {
    expand(input) match {
      case Some(axioms) => contract(axioms)
      case None => None
    }
  }
}

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


trait ExpansionStrategy[E, I]{
  def doExpansion(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}


trait ContractionStrategy[E, I]{
  def doPruning(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}



class BlackBoxGenerator[E, I](input: Set[I],
                              static: Set[I],
                              checkerFactory: CheckerFactory[E, I],
                              expansionStrategy: ExpansionStrategy[E,I],
                              contractionStrategy: ContractionStrategy[E, I],
                              algorithm: Algorithm[E, I],
                              useModularisation: Boolean = true){
  def executeAlgorithm(entailment: E): Option[Set[I]] ={
    val checker: Checker[E, I] = checkerFactory.createChecker(entailment, static)
    val algorithmInput = if (useModularisation) checker.getModule(input) else input
    val finder: JustificationFinder[E, I] = new JustificationFinder(checker, expansionStrategy, contractionStrategy)
    if (checker.isTautology)
      return None
    if (!checker.isEntailed(algorithmInput))
      return None
    val removes: Option[Set[I]] = algorithm.findRemoveSet(algorithmInput, finder)
    removes
  }
}


