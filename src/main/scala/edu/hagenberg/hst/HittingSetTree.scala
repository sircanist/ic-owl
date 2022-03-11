package edu.hagenberg.hst

import edu.hagenberg.{JustificationFinder, Util}
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import scala.annotation.tailrec
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
                           searchIterator: SearchIterator,
                           stop_after: Int) {
  val dont_stop: Boolean = stop_after == -1
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

  def getChildren(root: HittingSetTreeNode,
                  axioms:Set[OWLAxiom],
                  justifications: Set[OWLAxiom]): Set[HittingSetTreeNode] = {
    def createChildForEdge(root: HittingSetTreeNode, edge: Edge) = {
      new HittingSetTreeNode(Some(RootConnection(root, edge)))
    }
    val children = justifications.flatMap {
      just =>{
        val edges = getEdges(axioms, justifications, just)
        edges.map(edge => createChildForEdge(root, edge))
      }
    }
    children
  }

  def getAxioms(edges: List[Edge]): Set[OWLAxiom] = {
    val selected: List[OWLAxiom] = edges.map(edge => edge.selected)
    val weakened: List[OWLAxiom] = edges.flatMap(edge => edge.weakened)
    (input ++ weakened.toSet) -- selected.toSet
  }


  def processNode(node:HittingSetTreeNode,
                  discovered: Set[HittingSetTreeNode],
                  closedPaths: Set[HittingSetTreeNode]): (HittingSetTreeNode, Set[OWLAxiom]) = {
    val edges = node.edges
    val edges_set = node.edges_set
    val axioms: Set[OWLAxiom] = getAxioms(edges)// input -- edges

    def toSet(selected: OWLAxiom, weakened: Option[OWLAxiom]) = {
      weakened match {
        case Some(w) => Set(selected, w)
        case None => Set(selected)
      }
    }
    // this probably does not work for non laconic axioms
    def edges_flatten(edges: Set[Edge]) = {
      edges.flatMap(e => toSet(e.selected, e.weakened))
    }

    def check_intersection(just1: Set[OWLAxiom], justNode: Option[Set[OWLAxiom]]) = {
      justNode match {
        case Some(just) =>
          val intersection = just1.intersect(just)
          if (intersection.isEmpty)
            true
          else
            false
        case None => false // no intersection possible because one element is None
      }
    }


    var node_stat = {
      if (edges_set.nonEmpty  &&
        discovered.exists(d => d.edges_set.equals(edges_set)))
        {
          NodeStatus.Removed
        }
      else if (closedPaths.exists(cp => cp.edges_set == cp.edges_set.intersect(edges_set)))
        NodeStatus.Cancelled
      else
        NodeStatus.Open
    }



    val just = {
      if (node_stat != NodeStatus.Open)
        None
      else {
        val discover: Option[Option[Set[OWLAxiom]]] = discovered.collectFirst{
          case i: HittingSetTreeNode
            if check_intersection(edges_flatten(edges_set), i.justification) &&
                i.justification.get.forall(ijust => axioms.contains(ijust))
                => i.justification
        }
        discover match {
          case Some(justification) => justification
          case None => getJustification(axioms)
        }

      }
    }
    if (just.isEmpty && node_stat == NodeStatus.Open){
      node_stat = NodeStatus.Closed
    }
    val newNode = new HittingSetTreeNode(node.root, just)
    newNode.status = node_stat
    (newNode, axioms)
  }


  def search(): Set[HittingSetTreeNode] = {

    def common(head: HittingSetTreeNode, discovered: Set[HittingSetTreeNode],
               closedPaths: Set[HittingSetTreeNode]): (Set[HittingSetTreeNode], Set[HittingSetTreeNode], immutable.Iterable[HittingSetTreeNode]) = {
      val (newNode, axioms) = processNode(head, discovered, closedPaths)
      val dn =
        if (! newNode.status.equals(NodeStatus.Removed))
          discovered + newNode
        else discovered
      val children = {
        if (newNode.status == NodeStatus.Open) {
          newNode.justification match {
            case Some(just) => getChildren(newNode, axioms, just)
            case None => List.empty
          }
        }
        else
          List.empty
      }
      val cp = if (newNode.status == NodeStatus.Closed) closedPaths + newNode else closedPaths
//      val n = children.filter{
//        child =>
//          !closedPaths.exists(closed => child.edges.toSet.subsetOf(closed.edges.toSet))}
//      if (!n.eq(children)){
//        println("Children were removed by filter!")
//      }
      (dn, cp, children)


    }

    @tailrec def bfs(q: Queue[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode]): Set[HittingSetTreeNode] = {
      val (head, nq) = q.dequeue
      val (dn, cp, children) = common(head, discovered, closedPaths)

      var nq2 = nq
      if (children.nonEmpty) {
        nq2 = nq.enqueue(children)
        println("queue size: " + nq2.size)
        println("edges length: " +head.edges.length)
        println("processed: " + discovered.size)
        println("cp: " + cp.size)
      }
      if (nq2.nonEmpty && (dont_stop || cp.size <= stop_after))
        bfs(nq2, dn, cp)
      else
        cp
    }

    @tailrec def dfs(l: List[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode]): Set[HittingSetTreeNode] = {
      val head::nq = l
      val (dn, cp, children) = common(head, discovered, closedPaths)
      var nq2 = nq
      if (children.nonEmpty) {
        nq2 = List.concat(children,nq)
        println("queue size: " + nq2.size)
        println("edges length: " +head.edges.length)
        println("processed: " + discovered.size)
        println("cp: " + cp.size)
      }
      if (nq2.nonEmpty && (dont_stop || cp.size <= stop_after))
        dfs(nq2, dn, cp)
      else
        cp
    }
    searchIterator match {
      case BFS => bfs( Queue( root ), Set( ), Set(  ))
      case DFS => dfs( List( root ), Set( ), Set(  ))
    }
  }
}
