package edu.hagenberg

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
      None
    else if (!checker.isEntailed(algorithmInput))
      None
    else
      algorithm.findRemoveSet(algorithmInput, finder)
  }
}


