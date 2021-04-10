package edu.hagenberg.hst

import edu.hagenberg.{JustificationFinder, Util}
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import scala.collection.immutable
import scala.collection.immutable.Queue
sealed trait SearchIterator
object BFS extends  SearchIterator
object DFS extends  SearchIterator
class HittingSetTree[E, I](input: Set[OWLAxiom],
                           root: HittingSetTreeNode,
                           finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                           reasonerFactory: OWLReasonerFactory,
                           weaken:Boolean =true,
                           searchIterator: SearchIterator) {

  def getJustification(axioms:Set[OWLAxiom]): Option[Set[OWLAxiom]] ={
    finder.searchOneJustification(axioms)
  }
  def getEdges(axioms:Set[OWLAxiom], justification: Set[OWLAxiom], selected: OWLAxiom): Set[Edge] ={
    // get the new input = axioms - justification + selected
    if (weaken) {
      val weakenedSet: Set[OWLAxiom] = Util.getWeakened(axioms, justification, selected, finder, reasonerFactory)
      // if no weakening was found, we weaken with None
      if (weakenedSet.nonEmpty)
        weakenedSet.map(weakened => Edge(selected, Some(weakened)))
      else
        Set(Edge(selected, None))
    }
    else
      Set(Edge(selected, None))
  }

  def getChildren(root: HittingSetTreeNode, axioms:Set[OWLAxiom], justifications: Set[OWLAxiom]): Set[HittingSetTreeNode]  = {
    def createChildForEdge(root: HittingSetTreeNode, edge: Edge) = {
      new HittingSetTreeNode(Some(RootConnection(root, edge)))
    }
    justifications.flatMap {
      just =>{
        val edges = getEdges(axioms, justifications, just)
        edges.map(edge => createChildForEdge(root, edge))
      }
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

  def edges_subsetOf(edges1: Set[Edge],edges2: Set[Edge]): Boolean = {
    edges1.subsetOf(edges2)
  }

  def search(): Set[HittingSetTreeNode] = {

    def common(head: HittingSetTreeNode, discovered: Set[HittingSetTreeNode],
               closedPaths: Set[HittingSetTreeNode]): (Set[HittingSetTreeNode], Set[HittingSetTreeNode], immutable.Iterable[HittingSetTreeNode]) = {
      val (newNode, axioms) = processNode(head, discovered)
      val dn = discovered + newNode
      val children = {
        newNode.justification match {
          case Some(just) => getChildren(newNode, axioms, just)
          case None =>  List.empty
        }
      }

      // if no justifications were found for the newNode it cannot have children
      // if a justification was found it must have a mutation were the justification is not to be found
      // so it must have children
      assert(newNode.justification.nonEmpty == children.nonEmpty)
      val cp = if (newNode.justification.isEmpty) closedPaths + newNode  else closedPaths
      val n = children.filter(child => !closedPaths.exists(closed => edges_subsetOf(child.edges.toSet,closed.edges.toSet)))
      (dn, cp, n)

      // TODO if explanation enclosed, reuse

    }

    def bfs_(q: Queue[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode]): Set[HittingSetTreeNode] = {
      val (head, nq) = q.dequeue
      val (dn, cp, n) = common(head, discovered, closedPaths)
//      // TODO if explanation enclosed, reuse
      val nq2 = nq.enqueue(n)
      if (nq2.nonEmpty)
        bfs_(nq2, dn, cp)
      else
        cp
    }

    def dfs_(l: List[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode]): Set[HittingSetTreeNode] = {
      val head::nq = l
      val (dn, cp, n) = common(head, discovered, closedPaths)
      //      // TODO if explanation enclosed, reuse
      val nq2 = List.concat(n,nq)
      if (nq2.nonEmpty)
        dfs_(nq2, dn, cp)
      else
        cp
    }
    searchIterator match {
      case BFS => bfs_( Queue( root ), Set( ), Set(  ))
      case DFS => dfs_( List( root ), Set( ), Set(  ))
    }
  }
}
