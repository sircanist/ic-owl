package edu.hagenberg.hst

import edu.hagenberg.JustificationFinder
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Queue

class HittingSetTree[E, I](input: Set[OWLAxiom], root: HittingSetTreeNode, finder: JustificationFinder[E, I], reasonerFactory: OWLReasonerFactory) {

  var visited: ListBuffer[HittingSetTreeNode] = new ListBuffer[HittingSetTreeNode]
  var no_childs: ListBuffer[HittingSetTreeNode] = new ListBuffer[HittingSetTreeNode]
  visited += root
  def getJustification(axioms:Set[OWLAxiom]): Set[OWLAxiom] ={
    // get the justification for the axioms
       // - if None => return None
    // if found return child edge paths
  }
  def getEdge(axioms:Set[OWLAxiom], justification: Set[OWLAxiom], selected: OWLAxiom): Set[Edge] ={
    // get the new input = axioms - justification + axiom
    // create weakening
    // return edge
  }

  def getChildren(root: HittingSetTreeNode, axioms:Set[OWLAxiom], just: Set[OWLAxiom]): List[HittingSetTreeNode]  = {
    // select all axioms randomly from just
    // getEdge(axioms, justification, selected)
    // create Node with root
    // return the Edges
  }

  def getAxioms(edges: List[Edge]): Set[OWLAxiom] = {
    // return input - edges
  }

//  def processNode(node: HittingSetTreeNode): List[HittingSetTreeNode] = {
//    val edges = node.edges
//    val axioms: Set[OWLAxiom] = getAxioms(edges)// input -- edges
//    val just = getJustification(axioms)
//    node.updateJustification(just)
//    visited += node
//    val children = getChildren(node, axioms, just)
//    if (children.isEmpty){
//      no_childs += node
//    }
//    children
//  }


  def processNode(node:HittingSetTreeNode, discovered: Set[HittingSetTreeNode]): (HittingSetTreeNode, Set[OWLAxiom]) = {
    val edges = node.edges

    // this probably does not work for non laconic axioms
    def edges_flatten(edges: List[Edge]) = {
      edges.flatMap(e => {
        val axs: Set[OWLAxiom] = Set(e.selected, e.weakened.getOrElse(Set.empty[OWLAxiom]))
        axs
      }).toSet
    }
    val axioms: Set[OWLAxiom] = getAxioms(edges)// input -- edges
    val discover: Option[Set[OWLAxiom]] = discovered.collectFirst{
      case i if edges_flatten(edges).intersect(i.justification).isEmpty => i.justification
    }
    val just = discover match {
      case Some(justification) => justification
      case None => getJustification(axioms)
    }
    (new HittingSetTreeNode(node.root, just), axioms)
  }

  def bfs(root: HittingSetTreeNode): Set[HittingSetTreeNode] = {

    def bfs_(q: Queue[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode]): Set[HittingSetTreeNode] = {
      val (head, nq) = q.dequeue
      val (newNode, axioms) = processNode(head, discovered)
      val dn = discovered + head
      val children = {
        if (newNode.justification.nonEmpty)
          getChildren(newNode, axioms, newNode.justification)
        else
          List.empty
      }

      val cp = if (children.isEmpty) closedPaths + newNode  else closedPaths
      val n = children.filter(child => closedPaths.exists(closed => child.edges.toSet.subsetOf(closed.edges.toSet)))

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
