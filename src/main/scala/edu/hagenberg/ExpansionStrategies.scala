package edu.hagenberg



trait ExpansionStrategy[E, I]{
  def doExpansion(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}


object ExpansionStrategies {
  def simpleExpansionStrategy[E, I](): ExpansionStrategy[E, I] = {
    (axioms, checker) => {
      def addWhile(axioms: Seq[I], newelems: Set[I]): Option[Set[I]] ={
        axioms match{
          case x +: Seq() =>
            Some(newelems + x).filter(checker.isEntailed)
          case x +: xs =>
            Some(newelems + x).filter(checker.isEntailed).orElse(addWhile(xs, newelems + x))
          case _ =>
            None
        }
      }
      val (static, expander) = axioms.partition(checker.getStatic)
      addWhile(expander.toSeq, static)
    }
  }
}
