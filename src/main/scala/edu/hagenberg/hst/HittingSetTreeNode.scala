package edu.hagenberg.hst

import edu.hagenberg.PathElement
import org.semanticweb.owlapi.model.OWLAxiom

import scala.annotation.tailrec

case class RootConnection(root: HittingSetTreeNode, edge: Edge)
case class Edge(selected: OWLAxiom, weakened: Option[OWLAxiom])

class HittingSetTreeNode(val root: Option[RootConnection],
                         var justification: Set[OWLAxiom] = Set.empty) {

  var processed: Boolean =   if (justification.nonEmpty) true else false

  val edges: List[Edge] = getEdgesToRoot

  def updateJustification(just: Set[OWLAxiom]): Unit = {
    justification = just
    processed = true
  }
//
//  var children: List[HittingSetTreeNode] = List.empty
//
//  def addChild(node: HittingSetTreeNode): Unit = {
//    children = node :: children
//  }

//  def getOutgoingEdges: List[Edge] = {
//
//    def getEdgeIfExists(root: Option[RootConnection]): Option[Edge] = root match {
//      case Some(RootConnection(_, edge)) => Some(edge)
//      case _ => None
//    }
//    children.flatMap(
//      child => getEdgeIfExists(child.root))
//  }

  def getNodesToRoot: List[HittingSetTreeNode] = {
    HittingSetTreeNode.getNodesToRoot(this, List.empty)
  }

  def getEdgesToRoot: List[Edge] = {
    HittingSetTreeNode.getEdgesToRoot(this, List.empty)
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
        getPathElementsToRoot(
          root,
          PathElement(justifications = root.justification,
                      selected = edge.selected,
                      weakened = edge.weakened) :: path)
      case None => path
    }
  }
}
