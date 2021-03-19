package edu.hagenberg

import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory


trait Algorithm[E, I]{

  def findRemoveSet(input: Set[I], finder: JustificationFinder[E, I], reasonerFactory: OWLReasonerFactory): List[List[PathElement]]
}


object Algorithm {
  def simple: Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
    (input, finder, _) => {
      def findAll(input: Set[OWLAxiom]): List[PathElement] = {
        val justification = finder.searchOneJustification(input)
        justification match {
          case Some(just) =>
            val selected = Util.getRandomElement(just).get
            PathElement(just, selected, None) :: findAll(input -- just)
          case None => List.empty
        }
      }

      val all = findAll(input)
      List(all)
    }

  def simpleWeakening: Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
    (input, finder, reasonerFactory) => {


      def findAll(input: Set[OWLAxiom]): List[PathElement] = {
        val justification = finder.searchOneJustification(input)
        justification match {
          case Some(just) =>
            val weakenSet = Util.getWeakenedSet(input, just, finder, reasonerFactory)
            val (selected, weakened) = weakenSet
            weakened match {
              case Some(found) => PathElement(just, selected, weakened) :: findAll(input - selected + found)
              case None => PathElement(just, selected, weakened) :: findAll(input - selected)
            }
          case None => List.empty
        }
      }

      val all = findAll(input)
      List(all)
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