package edu.hagenberg.hst

import edu.hagenberg.{JustificationFinder, Util}
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory
import wvlet.log.LogFormatter.AppLogFormatter
import wvlet.log.LogSupport

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.JavaConverters._
import wvlet.log._

import java.io.FileOutputStream
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
                           stop_after: Int) extends LogSupport{
  val dont_stop: Boolean = stop_after == -1
  var iterations = 0
  logger.resetHandler(new FileHandler(fileName = "/tmp/app.log", formatter = AppLogFormatter))
  def getJustification(axioms:Set[OWLAxiom]): Option[Set[OWLAxiom]] ={
    finder.searchOneJustification(axioms)
  }

  def getEdges(axioms:Set[OWLAxiom], justification: Set[OWLAxiom], selected: OWLAxiom): List[Edge] ={
    // get the new input = axioms - justification + selected
    if (weaken) {
      val weakenedSet: Set[OWLAxiom] = Util.getWeakened(axioms, justification, selected, finder, reasonerFactory)
      // if no weakening was found, we weaken with None
      if (weakenedSet.nonEmpty)
        weakenedSet.toList.map(weakened => Edge(selected, Some(weakened)))
      else
        List(Edge(selected, None))
    }
    else
      List(Edge(selected, None))
  }

  def getChildren(root: HittingSetTreeNode,
                  axioms:Set[OWLAxiom],
                  justifications: Set[OWLAxiom],
                  closedPaths: Set[HittingSetTreeNode]): List[HittingSetTreeNode] = {
    def createChildForEdge(root: HittingSetTreeNode, edge: Edge) = {
      new HittingSetTreeNode(Some(RootConnection(root, edge)))
    }
    val children = justifications.flatMap {
      just =>{
        val edges = getEdges(axioms, justifications, just)
        edges.map(edge => createChildForEdge(root, edge))
      }
    }
    val remaining =   children
      .filterNot(child => closedPaths.exists(cp => cp.edges_set == cp.edges_set.intersect(child.edges_set)))
    // Filter childs so memory footprint is smaller
    //val removed_amount = children.size - remaining.size
    //if (removed_amount > 0)
    //  println("removed "+ removed_amount +" children.")
    remaining.toList
  }

  def getAxioms(edges_set: Set[Edge]): Set[OWLAxiom] = {
    val selected: Set[OWLAxiom] = edges_set.map(edge => edge.selected)
    val weakened: Set[OWLAxiom] = edges_set.flatMap(edge => edge.weakened)
    (input ++ weakened) -- selected
  }


  def processNode(node:HittingSetTreeNode,
                  discovered: mutable.HashSet[Set[Edge]],
                  closedPaths: Set[HittingSetTreeNode],
                  justifications: mutable.Set[Set[OWLAxiom]]): (HittingSetTreeNode, Set[OWLAxiom]) = {
    val edges_set = node.edges_set
    val axioms: Set[OWLAxiom] = getAxioms(edges_set)// input -- edges

    def toSet(selected: OWLAxiom, weakened: Option[OWLAxiom]) = {
      weakened match {
        case Some(w) => Set(selected, w)
        case None => Set(selected)
      }
    }
    // this probably does not work for non laconic axioms
    def edges_flatten(edges: Stream[Edge]) = {
      edges.flatMap(e => toSet(e.selected, e.weakened))
    }

    def check_intersection(just1: Set[OWLAxiom], justNode: Set[OWLAxiom]) = {
      just1.intersect(justNode).isEmpty
    }


    var node_stat = {
      if (edges_set.nonEmpty  &&
        discovered(edges_set))
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
        val discover: Option[Set[OWLAxiom]] = justifications.toStream.collectFirst{
            case j if j.intersect(edges_set.flatMap(e => toSet(e.selected, e.weakened)) ).isEmpty &&
                j.toStream.forall(axioms.contains) => j
        }
            //if check_intersection(edges_flatten(edges_set.toStream), i) &&
              //  i.forall(ijust => axioms.contains(ijust)) => i

        discover match {
          case Some(_) => discover
          case None => getJustification(axioms)
        }

      }
    }
    if (just.isEmpty && node_stat == NodeStatus.Open){
      node_stat = NodeStatus.Closed
      // TODO, manual debug only
//      if (edges_set.size > 8) {
//        val o = Util.createManager.createOntology()
//        o.addAxioms(axioms.asJava)
//        o.saveOntology(new FileOutputStream("/tmp/ont.owl"))
//      }
    }
    if (just.isDefined)
      justifications += just.get
    val newNode = new HittingSetTreeNode(node.root, just)
    newNode.status = node_stat
    (newNode, axioms)
  }


  def search(): Set[HittingSetTreeNode] = {

    def common(head: HittingSetTreeNode,
               discovered: mutable.HashSet[Set[Edge]],
               closedPaths: Set[HittingSetTreeNode],
               justifications: mutable.Set[Set[OWLAxiom]]): (Set[HittingSetTreeNode], List[HittingSetTreeNode]) = {
      val (newNode, axioms) = processNode(head, discovered, closedPaths, justifications)
      if (! newNode.status.equals(NodeStatus.Removed))
        discovered += newNode.edges_set
      val children = {
        if (newNode.status == NodeStatus.Open) {
          newNode.justification match {
            case Some(just) => getChildren(newNode, axioms, just, closedPaths)
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
      (cp, children)


    }

    @tailrec def bfs(q: Queue[HittingSetTreeNode],
             discovered: mutable.HashSet[Set[Edge]],
             closedPaths: Set[HittingSetTreeNode],
             justifications: mutable.Set[Set[OWLAxiom]],
             depth: Integer): Set[HittingSetTreeNode] = {
      val (head, nq) = q.dequeue
      val (cp, children) = common(head, discovered, closedPaths, justifications)
      iterations += 1
      if (iterations % 1000 == 0) {
        warn(s"iteration count: $iterations")
        info(s"cp: ${cp.size}" )
      }
      val new_depth = head.edges_set.size
      val dn_new =
          if (new_depth != depth ) {
            info(s"queue size: ${nq.size}")
            info(s"edges length: ${head.edges_set.size}")
            info(s"processed: ${discovered.size}")
            info(s"cp: ${cp.size}" )
            discovered
              .filter(node => node.size == new_depth)
              .filterNot(node => cp.exists(c => c.edges_set == c.edges_set.intersect(node)))
          } else discovered
      if (dn_new != discovered)
        discovered.clear()
      var nq2 = nq
      if (children.nonEmpty) {
        nq2 = nq.enqueue(children)
      }
      if (nq2.nonEmpty && (dont_stop || cp.size < stop_after))
        bfs(nq2, dn_new, cp, justifications, new_depth)
      else {
        warn(s"iteration count: $iterations")
        cp
      }
    }

    /*@tailrec def dfs(l: List[HittingSetTreeNode],
             discovered: Set[HittingSetTreeNode],
             closedPaths: Set[HittingSetTreeNode],
             justifications: mutable.Set[Set[OWLAxiom]]): Set[HittingSetTreeNode] = {
      val head::nq = l
      val (dn, cp, children) = common(head, discovered, closedPaths, justifications)
      var nq2 = nq
      if (children.nonEmpty) {
        nq2 = List.concat(children,nq)
        println("queue size: " + nq2.size)
        println("edges length: " +head.edges.length)
        println("processed: " + discovered.size)
        println("cp: " + cp.size)
      }
      if (nq2.nonEmpty && (dont_stop || cp.size < stop_after))
        dfs(nq2, dn, cp, justifications)
      else
        cp
    }*/

    var justifications =  scala.collection.mutable.Set[Set[OWLAxiom]]()
    var discovered = scala.collection.mutable.HashSet[Set[Edge]]()
    searchIterator match {
      case BFS => bfs( Queue( root ), discovered, Set( ), justifications, 0)
      //case DFS => dfs( List( root ), Set( ), Set(  ), justifications)
    }
  }
}
