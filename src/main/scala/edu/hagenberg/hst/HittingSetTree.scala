package edu.hagenberg.hst

import edu.hagenberg.{JustificationFinder, Util}
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import scala.collection.immutable.Queue

class HittingSetTree[E, I](input: Set[OWLAxiom],
                           root: HittingSetTreeNode,
                           finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                           reasonerFactory: OWLReasonerFactory,
                           weaken:Boolean =true) {

  def getJustification(axioms:Set[OWLAxiom]): Option[Set[OWLAxiom]] ={
    finder.searchOneJustification(axioms)
  }
  def getEdge(axioms:Set[OWLAxiom], justification: Set[OWLAxiom], selected: OWLAxiom): Edge ={
    // get the new input = axioms - justification + selected
    val weakened: Option[OWLAxiom] = {
      if (weaken)
        Util.getWeakened(input, justification, selected, finder, reasonerFactory)
      else
        None
    }
    Edge(selected, weakened)
  }

  def getChildren(root: HittingSetTreeNode, axioms:Set[OWLAxiom], justifications: Set[OWLAxiom]): Set[HittingSetTreeNode]  = {
    def createChildForEdge(root: HittingSetTreeNode, edge: Edge) = {
      new HittingSetTreeNode(Some(RootConnection(root, edge)))
    }
    justifications.map {
      just => createChildForEdge(root, getEdge(axioms, justifications, just))
    }
  }

  def getAxioms(edges: List[Edge]): Set[OWLAxiom] = {
    val selected: List[OWLAxiom] = edges.map(edge => edge.selected)
    val weakened: List[OWLAxiom] = edges.flatMap(edge => edge.weakened)
    (input ++ weakened.toSet) -- selected.toSet
  }


  def processNode(node:HittingSetTreeNode, discovered: Set[HittingSetTreeNode]): (HittingSetTreeNode, Set[OWLAxiom]) = {
    val edges = node.edges

    def toSet(selected: OWLAxiom, weakened: Option[OWLAxiom]) = {
      weakened match {
        case Some(w) => Set(selected, w)
        case None => Set(selected)
      }
    }
    // this probably does not work for non laconic axioms
    def edges_flatten(edges: List[Edge]) = {
      edges.flatMap(e => toSet(e.selected, e.weakened)).toSet
    }

    def check_intersection(just1: Set[OWLAxiom], justNode: Option[Set[OWLAxiom]]) = {
      justNode match {
        case Some(just) => just1.intersect(just).isEmpty
        case None => false // no intersection possible because one element is None
      }
    }

    val axioms: Set[OWLAxiom] = getAxioms(edges)// input -- edges
    val discover: Option[Option[Set[OWLAxiom]]] = discovered.collectFirst{
      case i
        if check_intersection(edges_flatten(edges), i.justification) => i.justification
    }
    val just = discover match {
      case Some(justification) => justification
      case None => getJustification(axioms)
    }
    (new HittingSetTreeNode(node.root, just), axioms)
  }

  def bfs(): Set[HittingSetTreeNode] = {

    def bfs_(q: Queue[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode]): Set[HittingSetTreeNode] = {
      val (head, nq) = q.dequeue
      val (newNode, axioms) = processNode(head, discovered)
      val dn = discovered + head
      val children = {
        newNode.justification match {
          case Some(just) => getChildren(newNode, axioms, just)
          case None =>  List.empty
        }
      }

      // if no justifications were found for the newNode it cannot have children
      // if a justification was found it must have a mutation were the justification is not to be found
      // so it must have children
      assert ( newNode.justification.nonEmpty == children.nonEmpty )
      val cp = if (newNode.justification.isEmpty) closedPaths + newNode  else closedPaths
      val n = children.filter(child => !closedPaths.exists(closed => child.edges.toSet.subsetOf(closed.edges.toSet)))

      // TODO if explanation enclosed, reuse

      val nq2 = nq.enqueue(n)

      if (nq2.nonEmpty)
        bfs_(nq2, dn, cp)
      else
        cp
    }
    bfs_( Queue( root ), Set( ), Set(  ))
  }
}
