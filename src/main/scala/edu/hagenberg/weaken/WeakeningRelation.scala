package edu.hagenberg.weaken

import com.google.common.collect.Sets
import conexp.fx.core.collections.relation.MatrixRelation
import conexp.fx.core.dl.ELConceptDescription
import org.semanticweb.owlapi.model.{AxiomType, IRI, OWLAnonymousIndividual, OWLAxiom, OWLClass, OWLClassAssertionAxiom, OWLDataFactory, OWLIndividual, OWLObjectPropertyAssertionAxiom, OWLOntology, OWLOntologyCreationException, OWLOntologyManager, OWLSubClassOfAxiom}
import org.semanticweb.owlapi.reasoner.{Node, OWLReasoner, OWLReasonerFactory}

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

  def semanticELConceptInclusionWeakeningRelation(dataFactory: OWLDataFactory): WeakeningRelation =
    nthELConceptInclusionWeakeningRelation3(java.lang.Integer.MAX_VALUE)
  //semanticELConceptInclusionWeakeningRelation(dataFactory)

  def syntacticELConceptInclusionWeakeningRelation(dataFactory: OWLDataFactory): WeakeningRelation =
    nthELConceptInclusionWeakeningRelation3(1)

  def classicalWeakeningRelation: WeakeningRelation =
    (_1, _2, _3, _4, _5, _6) ⇒ java.util.Collections.emptySet()

  def elPropertyWeakeningRelation(refutableOntology: OWLOntology): WeakeningRelation =
    (ontologyManager, reasonerFactory, staticOntology, justification, axiom, unwantedConsequence) ⇒ {

      val factory: OWLDataFactory = ontologyManager.getOWLDataFactory()
      val weakenings: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      val baseAxioms: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      baseAxioms.addAll(staticOntology.getAxioms())
      baseAxioms.addAll(refutableOntology.getAxioms())

      val weakenedOntology: OWLOntology = ontologyManager.createOntology()


      ontologyManager.addAxioms(weakenedOntology, baseAxioms)
      val reasoner = reasonerFactory.createReasoner(weakenedOntology)

      def createAnonymousAssertion(classAssertion: OWLObjectPropertyAssertionAxiom): OWLObjectPropertyAssertionAxiom ={

        val anonymous: OWLAnonymousIndividual = factory.getOWLAnonymousIndividual()
        val opAssertion: OWLObjectPropertyAssertionAxiom = factory.getOWLObjectPropertyAssertionAxiom(
          classAssertion.getProperty, classAssertion.getSubject, anonymous
        )
        opAssertion
      }

      def addClassAssertionForAnonymous(clazz: Node[OWLClass], individual:OWLIndividual): Unit ={
        weakenings add factory.getOWLClassAssertionAxiom(clazz.getRepresentativeElement(), individual)
      }
      if (axiom.getAxiomType().equals(AxiomType.OBJECT_PROPERTY_ASSERTION)){
        val origopAssertion: OWLObjectPropertyAssertionAxiom = axiom.asInstanceOf[OWLObjectPropertyAssertionAxiom]
        val obj = origopAssertion.getObject
        if (obj.isNamed) {
          val opAssertion = createAnonymousAssertion(origopAssertion)
          weakenings add opAssertion
          // does not change anything, because anonymous individuals have no classes in owl file
          //          val classes : NodeSet[OWLClass] = reasoner.getTypes(obj.asOWLNamedIndividual(), true)
          //          classes.forEach(
          //            addClassAssertionForAnonymous(_, opAssertion.getObject())
          //
          //          )
        }
        //
        //        val topClass = dataFactory.getOWLClass(IRI.create("owl:Thing"))
        //        return dataFactory.getOWLSubClassOfAxiom(topClass, classExpression.get)
      }
      weakenings
    }

  def getSubClassToWeaken(axiom: OWLAxiom, dataFactory: OWLDataFactory): OWLSubClassOfAxiom ={
    if (axiom.getAxiomType().equals(AxiomType.CLASS_ASSERTION)){
      val classAssertion: OWLClassAssertionAxiom = axiom.asInstanceOf[OWLClassAssertionAxiom]
      val classExpression = classAssertion.getClassExpression
      val topClass = dataFactory.getOWLClass(IRI.create("owl:Thing"))
      return dataFactory.getOWLSubClassOfAxiom(topClass, classExpression)
    }
    //    if (axiom.getAxiomType().equals(AxiomType.OBJECT_PROPERTY_ASSERTION)){
    //      val classAssertion: OWLObjectPropertyAssertionAxiom = axiom.asInstanceOf[OWLObjectPropertyAssertionAxiom]
    //      val classExpression = classAssertion.getObject.asOWLAnonymousIndividual()
    //      val topClass = dataFactory.getOWLClass(IRI.create("owl:Thing"))
    //      return dataFactory.getOWLSubClassOfAxiom(topClass, classExpression.get)
    //    }
    else if (axiom.getAxiomType().equals(AxiomType.SUBCLASS_OF))
      return axiom.asInstanceOf[OWLSubClassOfAxiom]
    else
      throw new IllegalArgumentException("Currently, only concept inclusions are supported.")
  }

  def createWeakenedAxiom(originalAxiom: OWLAxiom, candidate: ELConceptDescription, dataFactory: OWLDataFactory): OWLAxiom ={

    if (originalAxiom.getAxiomType().equals(AxiomType.CLASS_ASSERTION)){
      val classAssertion: OWLClassAssertionAxiom = originalAxiom.asInstanceOf[OWLClassAssertionAxiom]
      return dataFactory.getOWLClassAssertionAxiom(candidate.toOWLClassExpression(), classAssertion.getIndividual)
    }
    else if (originalAxiom.getAxiomType().equals(AxiomType.SUBCLASS_OF)){
      val subClassOfAssertion: OWLSubClassOfAxiom = originalAxiom.asInstanceOf[OWLSubClassOfAxiom]
      return dataFactory.getOWLSubClassOfAxiom(subClassOfAssertion.getSubClass(), candidate.toOWLClassExpression())
    }
    else
      throw new IllegalArgumentException("Currently, only concept inclusions are supported.")
  }

  def nthELConceptInclusionWeakeningRelation3(n: Integer): WeakeningRelation =
    (ontologyManager, reasonerFactory, staticOntology, justification, axiom, unwantedConsequence) ⇒ {


      // TODO: add method for subclass of and class assert abox to get the subClassOfAxiom for weaking
      // TODO: Add subistituion for compound Axiom or Parent Axiom

      //      val timer: Meter[java.lang.Long] = Meter.newNanoStopWatch

      val dataFactory: OWLDataFactory = ontologyManager.getOWLDataFactory
      val baseAxioms: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      baseAxioms.addAll(staticOntology.getAxioms())
      baseAxioms.addAll(justification)
      baseAxioms.remove(axiom)
      val subClassOfAxiom: OWLSubClassOfAxiom = getSubClassToWeaken(axiom, dataFactory)
      val conclusion: ELConceptDescription = ELConceptDescription.of(subClassOfAxiom.getSuperClass())
      val weakenedRHS: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet()
      val nextCandidates: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet(conclusion.upperNNeighborsReduced(n))
      //      val additionalnextCondidates: java.util.Set[ELConceptDescription]  = getOtherCandidates(conclusion)

      while (!nextCandidates.isEmpty()) {
        val processedCandidates: java.util.Set[ELConceptDescription] = new java.util.HashSet(nextCandidates)
        nextCandidates.parallelStream().forEach(candidate ⇒ {
          try {
            // TODO create weakenedAxiom for bot the classassertion and the owlsubclassofaxiom
            val weakenedAxiom: OWLAxiom =
              createWeakenedAxiom(axiom, candidate, dataFactory)
            val weakenedOntology: OWLOntology = ontologyManager.createOntology()
            ontologyManager.addAxioms(weakenedOntology, baseAxioms)
            ontologyManager.addAxiom(weakenedOntology, weakenedAxiom)
            val reasoner: OWLReasoner = reasonerFactory.createReasoner(weakenedOntology)
            if (reasoner.isEntailed(unwantedConsequence))
              nextCandidates.addAll(candidate.upperNNeighborsReduced(n))
            else
              weakenedRHS add candidate
            reasoner.dispose()
            ontologyManager.removeOntology(weakenedOntology)
          } catch {
            case e: OWLOntologyCreationException ⇒ throw new RuntimeException(e)
          }
        })
        nextCandidates.removeAll(processedCandidates)
      }

      def isStrictlyMoreSpecific(c: ELConceptDescription, d: ELConceptDescription): Boolean = (c compareTo d) == -1
      val nonMinimalRHS: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet()
      weakenedRHS.parallelStream().forEach(c ⇒ {
        weakenedRHS.parallelStream().forEach(d ⇒ {
          if (isStrictlyMoreSpecific(c, d))
            nonMinimalRHS add d
        })
      })
      weakenedRHS removeAll nonMinimalRHS

      val weakenings: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      weakenedRHS.parallelStream().forEach(c ⇒ {
        weakenings add createWeakenedAxiom(axiom, c, dataFactory)
      })

      val nonMinimalWeakenings: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      val order: MatrixRelation[OWLAxiom, OWLAxiom] = new MatrixRelation(true)
      order.rowHeads().addAll(weakenings)
      weakenings.stream().parallel().forEach(weakening1 ⇒ {
        val ontology: OWLOntology = ontologyManager.createOntology(java.util.Collections.singleton(weakening1))
        val reasoner: OWLReasoner = reasonerFactory.createReasoner(ontology)
        weakenings.stream().sequential().forEach(weakening2 ⇒ {
          if (!(weakening1 equals weakening2))
            if (reasoner isEntailed weakening2)
              order.add(weakening1, weakening2)
        })
        reasoner.dispose()
        ontologyManager.removeOntology(ontology)
        System.gc()
      })
      weakenings.stream().parallel().forEach(weakening2 ⇒ {
        val ontology: OWLOntology = ontologyManager.createOntology(java.util.Collections.singleton(weakening2))
        val reasoner: OWLReasoner = reasonerFactory.createReasoner(ontology)
        order.col(weakening2).stream().sequential().forEach(weakening1 ⇒ {
          if (!(weakening1 equals weakening2))
            if (!(reasoner isEntailed weakening1))
              nonMinimalWeakenings add weakening2
        })
        reasoner.dispose()
        ontologyManager.removeOntology(ontology)
        System.gc()
      })
      weakenings.removeAll(nonMinimalWeakenings)

      //      println("computation time of method 3: " + timer.measureAndFormat())
      //
      //      val weakenings4: java.util.Set[OWLAxiom] = semanticELConceptInclusionWeakeningRelation4(dataFactory).getWeakenings(ontologyManager, reasonerFactory, staticOntology, justification, axiom, unwantedConsequence)
      //      println()
      //      println("same weakenings found: " + ((weakenings containsAll weakenings4) && (weakenings4 containsAll weakenings)))
      //      println("\r\nweakenings only found by method number 3:")
      //      Sets.difference(weakenings, weakenings4).forEach(ax ⇒ println("\r\n" + ax))
      //      println("\r\nweakenings only found by method number 4:")
      //      Sets.difference(weakenings4, weakenings).forEach(ax ⇒ println("\r\n" + ax))

      weakenings
    }
}
