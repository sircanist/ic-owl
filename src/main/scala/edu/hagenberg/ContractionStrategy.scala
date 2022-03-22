package edu.hagenberg

import edu.hagenberg.ExpansionStrategy.debug
import wvlet.log.LogFormatter.AppLogFormatter

import scala.annotation.tailrec
import wvlet.log.{FileHandler, LogLevel, LogSupport, Logger}

trait ContractionStrategy[E, I]{
  def doPruning(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}

object ContractionStrategy extends LogSupport{
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
            if (checker.isEntailed(new_contraction ++ static)) {
              removeWhile(xs, new_contraction, static)
            } else
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

      val WINDOW_SIZE = 16

      val (static, refutable) = axioms.partition(checker.getStatic)
      @tailrec def removeWhile(refutable: Seq[I], contraction: Set[I], remove_count: Int): Option[Set[I]] ={
        refutable match{
          case x +: Seq() =>
            val new_contraction = contraction - x
            if (checker.isEntailed(new_contraction ++ static))
              Some(new_contraction)
            else
              Some(contraction)
          case x +: xs =>
            if (remove_count > 4) {
              val take_size = Math.min(refutable.length-1, remove_count)
              val (remove_contraction, new_list) = refutable.splitAt(take_size)
              val new_contraction_set = contraction -- remove_contraction.toSet
              if (checker.isEntailed(new_contraction_set ++ static)) {
                // increase speed for removements if nothing is in the Window
                val remove_window =
                  if (remove_count != WINDOW_SIZE) remove_count/2
                  else WINDOW_SIZE
                debug(s"executed contraction, entailment found, new remove_window = $remove_window ")
                removeWhile(new_list, new_contraction_set, remove_window)
              } else{
              // reduce speed for removements if culprit was found in Window
                val remove_window = remove_count / 2
                debug(s"executed contraction, entailment NOT found, new remove_window = $remove_window ")
                removeWhile(refutable, contraction, remove_window)
              }
            }
            else {
              val new_contraction = contraction - x
              if (checker.isEntailed(new_contraction ++ static)) {
                // if entailment is true after removing some item from contraction
                // it was not the culprit which lead to slow down, so cannot increase
                // speed
                debug(s"executed contraction, stepwise")
                removeWhile(xs, new_contraction, remove_count)
              } else {
                // increase speed because culprit was found
                //println(x)
                debug(s"executed contraction, stepwise culprit found, new remove_window $WINDOW_SIZE")
                removeWhile(xs, contraction, WINDOW_SIZE)
              }
            }

        }
      }
      val removed_set = removeWhile(refutable.toSeq, refutable, WINDOW_SIZE)
      removed_set
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
