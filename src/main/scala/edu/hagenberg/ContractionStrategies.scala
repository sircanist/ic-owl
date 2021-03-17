package edu.hagenberg

import scala.collection.mutable



trait ContractionStrategy[E, I]{
  def doPruning(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}

object ContractionStrategy {
  def simpleContractionStrategy[E, I](): ContractionStrategy[E, I] = {
    (axioms, checker) => {
      def removeWhile(refutable: Seq[I], contraction: Set[I], static: Set[I]): Option[Set[I]] ={
        refutable match{
          case x +: Seq() =>
            val new_contraction = contraction - x
            if (!checker.isEntailed(new_contraction ++ static))
              Some(contraction)
            else
              None
          case x +: xs =>
            val new_contraction = contraction - x
            if (checker.isEntailed(new_contraction ++ static))
              removeWhile(xs, new_contraction, static)
            else
              removeWhile(xs, contraction, static)
          case _ =>
            None
        }
      }
      val (static, refutable) = axioms.partition(checker.getStatic)
      removeWhile(refutable.toSeq, refutable, static)
    }
  }
}

//
//class SimpleContractionStrategy[E, I] extends ContractionStrategy[E, I] {
//  override def doPruning(axioms:Set[I], checker: Checker[E, I]): Option[Set[I]] = {
//    val (static, refutable) = axioms.partition(checker.getStatic)
//    var contraction: Set[I] = refutable
//    if (!checker.isEntailed(axioms)){
//      return None
//    }
//    for (ex: I <- refutable) {
//      contraction -= ex
//      if (!checker.isEntailed(contraction ++ static)){
//        contraction += ex
//      }
//    }
//    Some(contraction)
//  }
//}