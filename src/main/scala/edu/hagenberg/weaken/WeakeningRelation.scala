package edu.hagenberg.weaken

import com.google.common.collect.Sets
import conexp.fx.core.collections.relation.MatrixRelation
import conexp.fx.core.dl.ELConceptDescription
import edu.hagenberg.{JustificationFinder, Util}
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.{OWLReasoner, OWLReasonerFactory}

import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}

trait WeakeningRelation {

  // get the weakening for an axiom
  def getWeakened(input: Set[OWLAxiom],
                  finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                  justification: Set[OWLAxiom],
                  axiom: OWLAxiom,
                  reasonerFactory: OWLReasonerFactory ): Set[OWLAxiom]
}

object WeakeningRelation {

  def semanticELConceptInclusionWeakeningRelation: WeakeningRelation =
    nthELConceptInclusionWeakeningRelation3(java.lang.Integer.MAX_VALUE)
  //semanticELConceptInclusionWeakeningRelation(dataFactory)

  def syntacticELConceptInclusionWeakeningRelation: WeakeningRelation =
    nthELConceptInclusionWeakeningRelation3(1)

  def classicalWeakeningRelation: WeakeningRelation =
    (_, _, _, _, _) ⇒ Set.empty

  def elPropertyWeakeningRelation: WeakeningRelation =
    (_, _, _, axiom, _) ⇒ {
      val factory = Util.createManager.getOWLDataFactory

      def createAnonymousAssertion(classAssertion: OWLObjectPropertyAssertionAxiom): OWLObjectPropertyAssertionAxiom ={

        val anonymous: OWLAnonymousIndividual = factory.getOWLAnonymousIndividual()
        val opAssertion: OWLObjectPropertyAssertionAxiom = factory.getOWLObjectPropertyAssertionAxiom(
          classAssertion.getProperty, classAssertion.getSubject, anonymous
        )
        opAssertion
      }

      val origAssertion: OWLObjectPropertyAssertionAxiom = axiom.asInstanceOf[OWLObjectPropertyAssertionAxiom]
      val obj = origAssertion.getObject
      if (obj.isNamed) {
        Set(createAnonymousAssertion(origAssertion))
      }
      else
        Set.empty
    }

//
  def getSubClassToWeaken(axiom: OWLAxiom, dataFactory: OWLDataFactory): OWLSubClassOfAxiom ={
    if (axiom.getAxiomType.equals(AxiomType.CLASS_ASSERTION)){
      val classAssertion: OWLClassAssertionAxiom = axiom.asInstanceOf[OWLClassAssertionAxiom]
      val classExpression = classAssertion.getClassExpression
      val topClass = dataFactory.getOWLClass(IRI.create("owl:Thing"))
      dataFactory.getOWLSubClassOfAxiom(topClass, classExpression)
    }
    //    if (axiom.getAxiomType().equals(AxiomType.OBJECT_PROPERTY_ASSERTION)){
    //      val classAssertion: OWLObjectPropertyAssertionAxiom = axiom.asInstanceOf[OWLObjectPropertyAssertionAxiom]
    //      val classExpression = classAssertion.getObject.asOWLAnonymousIndividual()
    //      val topClass = dataFactory.getOWLClass(IRI.create("owl:Thing"))
    //      return dataFactory.getOWLSubClassOfAxiom(topClass, classExpression.get)
    //    }
    else if (axiom.getAxiomType.equals(AxiomType.SUBCLASS_OF))
      axiom.asInstanceOf[OWLSubClassOfAxiom]
    else
      throw new IllegalArgumentException("Currently, only concept inclusions are supported.")
  }

  def createWeakenedAxiom(originalAxiom: OWLAxiom, candidate: ELConceptDescription, dataFactory: OWLDataFactory): OWLAxiom ={

    if (originalAxiom.getAxiomType.equals(AxiomType.CLASS_ASSERTION)){
      val classAssertion: OWLClassAssertionAxiom = originalAxiom.asInstanceOf[OWLClassAssertionAxiom]
      dataFactory.getOWLClassAssertionAxiom(candidate.toOWLClassExpression, classAssertion.getIndividual)
    }
    else if (originalAxiom.getAxiomType.equals(AxiomType.SUBCLASS_OF)){
      val subClassOfAssertion: OWLSubClassOfAxiom = originalAxiom.asInstanceOf[OWLSubClassOfAxiom]
      dataFactory.getOWLSubClassOfAxiom(subClassOfAssertion.getSubClass, candidate.toOWLClassExpression)
    }
    else
      throw new IllegalArgumentException("Currently, only concept inclusions are supported.")
  }
//
  def nthELConceptInclusionWeakeningRelation3(n: Integer): WeakeningRelation =
    (input, finder, justification, axiom, reasonerFactory) ⇒ {
      val ontologyManager = Util.createManager
      val dataFactory = ontologyManager.getOWLDataFactory
      val checker = finder.checker
      val subClassOfAxiom: OWLSubClassOfAxiom = getSubClassToWeaken(axiom, dataFactory)
      val conclusion: ELConceptDescription = ELConceptDescription.of(subClassOfAxiom.getSuperClass)
      val weakenedRHS: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet()
      val nextCandidates: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet(conclusion.upperNNeighborsReduced(n))

      val (static, _) = input.partition(checker.getStatic)

      val baseAxioms = static ++ justification - axiom

      while (!nextCandidates.isEmpty) {
        val processedCandidates: java.util.Set[ELConceptDescription] = new java.util.HashSet(nextCandidates)
        nextCandidates.parallelStream().forEach(candidate ⇒ {
        val weakenedAxiom: OWLAxiom =
          createWeakenedAxiom(axiom, candidate, dataFactory)
        if (checker.isEntailed(baseAxioms + weakenedAxiom)) {
          nextCandidates.addAll(candidate.upperNNeighborsReduced(n))
          // TODO add superclasses with reasoner
        } else
          weakenedRHS add candidate

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

      val weakening: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      weakenedRHS.parallelStream().forEach(c ⇒ {
        weakening add createWeakenedAxiom(axiom, c, dataFactory)
      })


      val nonMinimalWeakening: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
      val order: MatrixRelation[OWLAxiom, OWLAxiom] = new MatrixRelation(true)
      order.rowHeads().addAll(weakening)
      weakening.stream().parallel().forEach(weakening1 ⇒ {
        val reasoner: OWLReasoner =
          reasonerFactory.createReasoner(Util.createManager.createOntology((baseAxioms + weakening1).asJava))
        weakening.stream().sequential().forEach(weakening2 ⇒ {
          if (!(weakening1 equals weakening2))
            if (reasoner isEntailed weakening2)
              order.add(weakening1, weakening2)
        })
        System.gc()
      })
      weakening.removeAll(nonMinimalWeakening)
      weakening.asScala.toSet
    }
}
