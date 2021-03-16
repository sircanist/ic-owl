package edu.hagenberg

import org.semanticweb.owlapi.model.OWLAxiom

import java.util


class SimpleAlgorithm extends Algorithm[java.util.Set[OWLAxiom], OWLAxiom] {
  override def findRemoveSet(input: Set[OWLAxiom], finder: JustificationFinder[util.Set[OWLAxiom], OWLAxiom]): Option[Set[OWLAxiom]] = {
    finder.searchOneJustification(input)
  }

}