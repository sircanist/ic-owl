package edu.hagenberg

import edu.hagenberg.hst.{HittingSetTree, HittingSetTreeNode, SearchIterator}
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory


trait Algorithm[E, I]{

  def findRemoveSet(input: Set[I], finder: JustificationFinder[E, I], reashittingSetNoWeakeningonerFactory: OWLReasonerFactory): List[List[PathElement]]
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

  def hittingSet(weaken: Boolean, searchIterator: SearchIterator, stop_after: Int):Algorithm[java.util.Set[OWLAxiom], OWLAxiom] = {
    (input, finder, reasonerFactory) => {
      val rootNode = new HittingSetTreeNode(edges_set = Set())
      val tree = new HittingSetTree(input, rootNode, finder, reasonerFactory, weaken, searchIterator, stop_after)
      tree.search().toList.map(el => el.edges_set.toList.map(edge => new PathElement(Set(), edge.selected, edge.weakened)))
    }
  }
//  def hittingSetWeakening: Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
//    (input, finder, reasonerFactory) => {
//      val rootNode = new HittingSetTreeNode(root = None)
//      val tree = new HittingSetTree(input, rootNode, finder, reasonerFactory)
//      tree.bfs().toList.map(el => el.getPathElementsToRoot)
//    }
//
//
//
//  def hittingSetNoWeakening: Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
//    (input, finder, reasonerFactory) => {
//      val rootNode = new HittingSetTreeNode(root = None)
//      val tree = new HittingSetTree(input, rootNode, finder, reasonerFactory, weaken = false)
//      tree.bfs().toList.map(el => el.getPathElementsToRoot)
//    }
}
