package edu.hagenberg

import org.semanticweb.owlapi.model.{AxiomType, OWLAxiom}
import edu.hagenberg.weaken.WeakeningRelation

import java.util

trait Algorithm[E, I]{

  def findRemoveSet(input: Set[I], finder: JustificationFinder[E, I], checkerFactory: CheckerFactory[E, I]): Option[Set[I]]
}

object Algorithms {
  def simple(): Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
    (input, finder, _) => {
      def findAll(input: Set[OWLAxiom]): Set[OWLAxiom] = {
        val justification = finder.searchOneJustification(input)
        justification match {
          case Some(just) => just ++ findAll(input -- just)
          case None => Set.empty
        }
      }

      val all = findAll(input)
      Some(all)
    }

  def simpleWeakening(): Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
    (input, finder, checkerFactory) => {


      def getWeakenedSet(input: Set[OWLAxiom], just: Set[OWLAxiom]): (OWLAxiom, Option[OWLAxiom]) ={
        // TODO improve handling, maybe remove runtime
        val selected: OWLAxiom = Util.getRandomElement(just).get
        val atype: AxiomType[_] = selected.getAxiomType
        val currentWeakeningRelation =
          atype match {
            case AxiomType.CLASS_ASSERTION | AxiomType.SUBCLASS_OF =>
              WeakeningRelation.classicalWeakeningRelation
            case AxiomType.OBJECT_PROPERTY_ASSERTION => WeakeningRelation.elPropertyWeakeningRelation
            case _ => WeakeningRelation.classicalWeakeningRelation
          }
        val weakened: Set[OWLAxiom] = currentWeakeningRelation.getWeakened(input, finder, just, selected, checkerFactory)
        val chosen_weakened = Util.getRandomElement(weakened)
        chosen_weakened match {
          case Some(found) => (selected, Some(found))
          case _ => (selected, None)
        }
      }

      def findAll(input: Set[OWLAxiom]): Set[(OWLAxiom, OWLAxiom)] = {
        val justification = finder.searchOneJustification(input)
        justification match {
          case Some(just) => {
            val weakenSet = getWeakenedSet(input, just)
            val (selected, weakened) = weakenSet
//            weakened match {
//              case Some(found) =>
//            }
            weakenSet ++ findAll(input - selected + weakened)
          }
          case None => Set.empty
        }
      }

      val all = findAll(input)
      Some(all)
    }

//  def simpleWeakening(): Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
//    (input, finder) => {
//
//      def findAll(input: Set[OWLAxiom]): Set[OWLAxiom] = {
//        val justification = finder.searchOneJustification(input)
//        justification match {
//          case Some(just) => just ++ findAll(input -- just)
//          case None => Set.empty
//        }
//      }
//
//      val all = findAll(input)
//      Some(all)
//      }
}