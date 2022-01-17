package edu.hagenberg


import edu.hagenberg.Util.getAxiomsFromFile
import edu.hagenberg.hst.DFS
import openllet.owlapi.OpenlletReasonerFactory
import org.scalatest.funsuite.AnyFunSuite
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLDeclarationAxiom}
import org.semanticweb.owlapi.util.SimpleIRIMapper

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
            checkerFactory = CheckerFactory.AxiomsAtOnceStrategy(new OpenlletReasonerFactory),
            reasonerFactory = new OpenlletReasonerFactory,
            expansionStrategy = ExpansionStrategy.simpleExpansionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            contractionStrategy = ContractionStrategy.simpleContractionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            algorithm = Algorithm.simple,
            useModularisation = false
        )
        val remove_axioms = generator.executeAlgorithm(entailment)
        print(remove_axioms)
    }


    test("real"){
        //val PATH_MASTERARBEIT = "/home/chris/Desktop/masterarbeit/"
        val PATH_MASTERARBEIT = "/home/chris/Desktop/VPRO/Mastererarbeit/"
        val PATH_CORE = PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/cti.owl"
        val PATH_ATT_KNOWLEDGE = PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/scenario1-att.owl"
        val PATH_SCENARIO1 = PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/scenario1.owl"
        val PATH_UNWANTED = PATH_MASTERARBEIT + "org/ontologien/tawny_ctidev/scenario1-unwanted.owl"

        val iriMapper = List(
            new SimpleIRIMapper
                (IRI.create("http://purl.org/cti-core"), IRI.create("file:///" + PATH_CORE)),
            new SimpleIRIMapper
                (IRI.create("http://purl.org/cti-scenario1"), IRI.create("file:///" + PATH_SCENARIO1)))


        val base_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_CORE), iriMapper)
        val attacker_knowledge_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_ATT_KNOWLEDGE), iriMapper)
        val cti_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_SCENARIO1), iriMapper)
        val unwanted_ontology_axioms: Set[OWLAxiom] = getAxiomsFromFile(new File(PATH_UNWANTED), iriMapper)

        val static_without_cti: Set[OWLAxiom] = base_axioms ++ attacker_knowledge_axioms
        val input: Set[OWLAxiom] = static_without_cti ++ cti_axioms
        //val static: Set[OWLAxiom] = static_without_cti ++ cti_axioms.intersect(static_without_cti)
        val static: Set[OWLAxiom] = static_without_cti // removing intersections leads to also possible delete instances

        val entailment = (unwanted_ontology_axioms -- static).filter(axiom => !axiom.isInstanceOf[OWLDeclarationAxiom]).asJava


        val generator: BlackBoxGenerator[java.util.Set[OWLAxiom], OWLAxiom] = new BlackBoxGenerator(
            input,
            static,
            checkerFactory = CheckerFactory.AnyAxiomCheckerStrategy(new OpenlletReasonerFactory),
            reasonerFactory = new OpenlletReasonerFactory,
            expansionStrategy = ExpansionStrategy.structuralExpansionStrategy,
            contractionStrategy = ContractionStrategy.newSlidingContractionStrategy[java.util.Set[OWLAxiom], OWLAxiom],
            algorithm = Algorithm.hittingSet(true, DFS)
            //algorithm = Algorithm.simpleWeakening
        )
        val remove_axioms = generator.executeAlgorithm(entailment)
        remove_axioms match {
            case Left(s) => println(s"ERROR: ${s.getMessage}")
            case Right(paths) => {
                paths.foreach(
                    pathelements => {
                        println("\n\nNew path")
                        pathelements.foreach {
                            path => {
                                println("{")
                                println("Justifications:\n " + path.justifications)
                                println("selected:\n " + path.selected)
                                println("weakened:\n " + path.weakened)
                                println("}")
                            }
                        }
                    }
                )
            }
        }
//        val manager = OWLManager.createOWLOntologyManager
//        val merged = manager.loadOntologyFromOntologyDocument(new File("/tmp/merged_complex.owl"))
//        val unwanted = manager.loadOntologyFromOntologyDocument(new File("/tmp/unwanted.owl"))
//        val input = merged.axioms()
//        val staticAxioms = merged.getAxioms().asScala -- AxiomSplitter(unwanted).getAboxAxioms

    }


}

