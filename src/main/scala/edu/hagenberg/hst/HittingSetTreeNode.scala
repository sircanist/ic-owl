package edu.hagenberg.hst

import edu.hagenberg.PathElement
import org.apache.tinkerpop.gremlin.structure.Vertex
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerVertex
import org.semanticweb.owlapi.model.OWLAxiom

import scala.annotation.tailrec

case class RootConnection(root: HittingSetTreeNode, edge: Edge)
case class Edge(selected: OWLAxiom, weakened: Option[OWLAxiom])

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val Open, Closed, Cancelled, Removed = Value
}

class HittingSetTreeNode(var edges_set: Set[Edge],
	var status: NodeStatus.Value = NodeStatus.Open,
	var justification: Option[Set[OWLAxiom]] = None) {}
