package edu.hagenberg


class SimpleExpansionStrategy[E, I] extends ExpansionStrategy[E, I] {
  override def doExpansion(axioms: Set[I], checker: Checker[E, I]): Option[Set[I]] = {
    var expansion: Set[I] = Set()
    for (el <- axioms ){
      expansion = expansion + el
      if(checker.isEntailed(expansion))
        return Some(expansion)
    }
    None
  }
}