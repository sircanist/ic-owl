package edu.hagenberg.hst

import edu.hagenberg.PathElement
import org.semanticweb.owlapi.model.OWLAxiom

import scala.annotation.tailrec

case class RootConnection(root: HittingSetTreeNode, edge: Edge)
case class Edge(selected: OWLAxiom, weakened: Option[OWLAxiom])

class HittingSetTreeNode(val root: Option[RootConnection],
                         var justification: Option[Set[OWLAxiom]] = None) {

  var processed: Boolean =  justification.nonEmpty

  val edges: List[Edge] = getEdgesToRoot

  def getNodesToRoot: List[HittingSetTreeNode] = {
    HittingSetTreeNode.getNodesToRoot(this, List.empty)
  }

  def getEdgesToRoot: List[Edge] = {
    HittingSetTreeNode.getEdgesToRoot(this, List.empty)
  }

  def getPathElementsToRoot: List[PathElement] = {
    HittingSetTreeNode.getPathElementsToRoot(this, List.empty)
  }


}

object HittingSetTreeNode{
  @tailrec
  def getNodesToRoot(node: HittingSetTreeNode, path: List[HittingSetTreeNode]): List[HittingSetTreeNode] = {
    node.root match {
      case Some(RootConnection(root, _)) => getNodesToRoot(root, root :: path)
      case None => path
    }
  }

  @tailrec
  def getEdgesToRoot(node: HittingSetTreeNode, path: List[Edge]): List[Edge] = {
    node.root match {
      case Some(RootConnection(root, edge)) => getEdgesToRoot(root, edge :: path)
      case None => path
    }
  }

  @tailrec
  def getPathElementsToRoot(node: HittingSetTreeNode, path: List[PathElement]): List[PathElement] = {
    node.root match {
      case Some(RootConnection(root, edge)) =>
        root.justification match {
          case Some(justification) =>
              getPathElementsToRoot(
              root,
              PathElement(justifications = justification,
              selected = edge.selected,
              weakened = edge.weakened) :: path)
          case None => path
        }
      case None => path
    }
  }
}
