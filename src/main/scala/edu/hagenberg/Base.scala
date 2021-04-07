package edu.hagenberg

import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory


case class PathElement(justifications: Set[OWLAxiom], selected: OWLAxiom, weakened: Option[OWLAxiom])

class JustificationFinder[E, I](val checker: Checker[E,I],
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

class BlackBoxGenerator[E, I](input: Set[I],
                              static: Set[I],
                              checkerFactory: CheckerFactory[E, I],
                              reasonerFactory: OWLReasonerFactory,
                              expansionStrategy: ExpansionStrategy[E,I],
                              contractionStrategy: ContractionStrategy[E, I],
                              algorithm: Algorithm[E, I],
                              useModularisation: Boolean = true){
  def executeAlgorithm(entailment: E): Either[Error, List[List[PathElement]]] ={
    val checker: Checker[E, I] = checkerFactory.createChecker(entailment, static)
    val algorithmInput = if (useModularisation) checker.getModule(input) else input
    val finder: JustificationFinder[E, I] = new JustificationFinder(checker, expansionStrategy, contractionStrategy)
    if (checker.isTautology)
      Left(new Error("Tautology"))
    else if (!checker.isEntailed(algorithmInput))
      Left(new Error("Not Entailed"))
    else
      Right(algorithm.findRemoveSet(algorithmInput, finder, reasonerFactory))
  }
}


