package edu.hagenberg


import org.scalatest.funsuite.AnyFunSuite
import edu.hagenberg.BlackBoxGenerator
import openllet.owlapi.OpenlletReasonerFactory
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLDataFactory

import java.util
import java.util.Collections

class Test1 extends AnyFunSuite {
    test("Hello World"){
        val df = OWLManager.getOWLDataFactory
        val A = df.getOWLClass(IRI.create("http://example.com/A"))
        val B = df.getOWLClass(IRI.create("http://example.com/B"))
        val C = df.getOWLClass(IRI.create("http://example.com/C"))
        val ASubClassOfB = df.getOWLSubClassOfAxiom(A, B)
        val BSubClassOfC = df.getOWLSubClassOfAxiom(B, C)
        val entailment:java.util.Set[OWLAxiom] = new util.HashSet[OWLAxiom]()
        entailment.add(BSubClassOfC)
        val input: Set[OWLAxiom] = Set(ASubClassOfB, BSubClassOfC)
        val generator: BlackBoxGenerator[java.util.Set[OWLAxiom], OWLAxiom] = new BlackBoxGenerator(
            input,
            checkerFactory = new SimpleCheckerFactory(new OpenlletReasonerFactory),
            expansionStrategy = new SimpleExpansionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            contractionStrategy = new SimpleContractionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            algorithm = new SimpleAlgorithm
        )
        generator.executeAlgorithm(entailment)
    }
}

