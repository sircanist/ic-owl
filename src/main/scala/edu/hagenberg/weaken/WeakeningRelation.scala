package edu.hagenberg.weaken

import org.semanticweb.owlapi.model.{OWLAxiom, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import java.util

trait WeakeningRelation {

  // get the weakening for an axiom
  def getWeakenings(ontologyManager: OWLOntologyManager,
                    reasonerFactory: OWLReasonerFactory,
                    staticOntology: OWLOntology,
                    justification: java.util.Set[OWLAxiom],
                    axiom: OWLAxiom,
                    unwantedConsequence: util.Set[OWLAxiom]): java.util.Set[OWLAxiom]
}

object WeakeningRelation {

  def classicalWeakeningRelation: WeakeningRelation =
    (_1, _2, _3, _4, _5, _6) ⇒ java.util.Collections.emptySet()
}
