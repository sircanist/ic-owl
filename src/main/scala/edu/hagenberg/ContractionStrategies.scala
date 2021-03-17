package edu.hagenberg


class SimpleContractionStrategy[E, I] extends ContractionStrategy[E, I] {
  override def doPruning(axioms:Set[I], checker: Checker[E, I]): Option[Set[I]] = {
    var contraction: Set[I] = axioms
    // TODO remove Option on return value and remove None return here, otherwise this silently fails
    if (!checker.isEntailed(contraction)){
      return None
    }
    for (ex: I <- axioms) {
      contraction -= ex
      if (!checker.isEntailed(contraction)){
        contraction += ex
      }
    }
    Some(contraction)
  }
}