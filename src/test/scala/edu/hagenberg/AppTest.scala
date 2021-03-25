package edu.hagenberg


import edu.hagenberg.Util.getAxiomsFromFile
import openllet.owlapi.OpenlletReasonerFactory
import org.scalatest.funsuite.AnyFunSuite
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLDeclarationAxiom}

import java.io.File
import java.util
import scala.collection.JavaConverters._

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
        val static: Set[OWLAxiom] = Set(ASubClassOfB)

        val generator: BlackBoxGenerator[java.util.Set[OWLAxiom], OWLAxiom] = new BlackBoxGenerator(
            input,
            static,
            checkerFactory = new SimpleCheckerFactory(new OpenlletReasonerFactory),
            reasonerFactory = new OpenlletReasonerFactory,
            expansionStrategy = ExpansionStrategy.simpleExpansionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            contractionStrategy = ContractionStrategy.simpleContractionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            algorithm = Algorithm.simple
        )
        val remove_axioms = generator.executeAlgorithm(entailment)
        print(remove_axioms)
    }


    test("real"){
        val PATH_MASTERARBEIT = "/home/chris/Desktop/VPRO/Mastererarbeit/"

        val base_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/cti.owl"))
        val attacker_knowledge_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/scenario1-att.owl"))
        val cti_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/scenario1.owl"))
        val unwanted_ontology_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File("/tmp/unwanted.owl"))

        val static_without_cti: Set[OWLAxiom] = base_axioms ++ attacker_knowledge_axioms
        val input: Set[OWLAxiom] = static_without_cti ++ cti_axioms
        val static: Set[OWLAxiom] = input -- cti_axioms.diff(static_without_cti)

        val entailment = (unwanted_ontology_axioms -- static).filter(axiom => !axiom.isInstanceOf[OWLDeclarationAxiom]).asJava


        val generator: BlackBoxGenerator[java.util.Set[OWLAxiom], OWLAxiom] = new BlackBoxGenerator(
            input,
            static,
            checkerFactory = new SimpleCheckerFactory(new OpenlletReasonerFactory),
            reasonerFactory = new OpenlletReasonerFactory,
            expansionStrategy = ExpansionStrategy.simpleExpansionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            contractionStrategy = ContractionStrategy.simpleContractionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            algorithm = Algorithm.hittingSetWeakening
        )
        val remove_axioms = generator.executeAlgorithm(entailment)
        remove_axioms match {
            case Left(s) => println(s"ERROR: ${s.getMessage}")
            case Right(paths) => println(paths)
        }
//        val manager = OWLManager.createOWLOntologyManager
//        val merged = manager.loadOntologyFromOntologyDocument(new File("/tmp/merged_complex.owl"))
//        val unwanted = manager.loadOntologyFromOntologyDocument(new File("/tmp/unwanted.owl"))
//        val input = merged.axioms()
//        val staticAxioms = merged.getAxioms().asScala -- AxiomSplitter(unwanted).getAboxAxioms

    }


}

