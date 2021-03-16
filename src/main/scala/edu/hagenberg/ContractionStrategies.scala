package edu.hagenberg


class SimpleContractionStrategy[E, I] extends ContractionStrategy[E, I] {
  override def doPruning(axioms: Set[I], checker: Checker[E, I]): Set[I] = {
    var contraction: Set[I] = Set()
    for (ex: I <- axioms) {
      contraction -= ex
      if (!checker.isEntailed(contraction)){
        contraction += ex
      }
    }
    contraction
  }
}