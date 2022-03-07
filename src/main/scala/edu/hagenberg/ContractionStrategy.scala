package edu.hagenberg

import scala.annotation.tailrec

trait ContractionStrategy[E, I]{
  def doPruning(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}

object ContractionStrategy {
  def simpleContractionStrategy[E, I]: ContractionStrategy[E, I] = {
    (axioms, checker) => {
      @tailrec def removeWhile(refutable: Seq[I], contraction: Set[I], static: Set[I]): Option[Set[I]] ={
        refutable match{
          case x +: Seq() =>
            val new_contraction = contraction - x
            if (checker.isEntailed(new_contraction ++ static))
              Some(new_contraction)
            else
              Some(contraction)
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

  def newSlidingContractionStrategy[E, I]: ContractionStrategy[E, I] = {
    (axioms, checker) => {

      val WINDOW_SIZE = 20

      @tailrec def removeWhile(refutable: Seq[I], contraction: Set[I], static: Set[I], remove_count: Int): Option[Set[I]] ={
        refutable match{
          case x +: Seq() =>
            val new_contraction = contraction - x
            if (checker.isEntailed(new_contraction ++ static))
              Some(new_contraction)
            else
              Some(contraction)
          case x +: xs =>
            if (remove_count > 5) {
              val take_size = Math.min(refutable.length-1, remove_count)
              val (remove_contraction, new_list) = refutable.splitAt(take_size)
              val new_contraction_set = contraction -- remove_contraction.toSet
              if (checker.isEntailed(new_contraction_set ++ static)) {
                // increase speed for removements if nothing is in the Window
                val min_remove_count = Math.min(WINDOW_SIZE, remove_count*2)
                removeWhile(new_list, new_contraction_set, static, min_remove_count)
              } else
              // reduce speed for removements if culprit was found in Window
                removeWhile(refutable, contraction, static, remove_count/2)
            }
            else {
              val new_contraction = contraction - x
              if (checker.isEntailed(new_contraction ++ static)) {
                // if entailment is true after removing some item from contraction
                // it was not the culprit which lead to slow down, so cannot increase
                // speed
                removeWhile(xs, new_contraction, static, remove_count)
              } else {
                // increase speed because culprit was found
                //println(x)
                removeWhile(xs, contraction, static, remove_count*2)
              }
            }

        }
      }
      val (static, refutable) = axioms.partition(checker.getStatic)
      removeWhile(refutable.toSeq, refutable, static, WINDOW_SIZE)
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
