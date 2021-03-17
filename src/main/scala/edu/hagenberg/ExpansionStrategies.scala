package edu.hagenberg

import scala.collection.mutable


class SimpleExpansionStrategy[E, I] extends ExpansionStrategy[E, I] {
  override def doExpansion(axioms: Set[I], checker: Checker[E, I]): Option[Set[I]] = {
    var expansion: mutable.Set[I] = mutable.Set()
    for (el <- axioms ){
      expansion = expansion + el
      if(checker.isEntailed(expansion.toSet))
        return Some(expansion.toSet)
    }
    None
  }
}