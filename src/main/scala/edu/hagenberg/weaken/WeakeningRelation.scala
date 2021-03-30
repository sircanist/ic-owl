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
  def getClassExpression(axiom: OWLAxiom): OWLClassExpression ={
    if (axiom.getAxiomType.equals(AxiomType.CLASS_ASSERTION)){
      axiom.asInstanceOf[OWLClassAssertionAxiom].getClassExpression
    }
    //    if (axiom.getAxiomType().equals(AxiomType.OBJECT_PROPERTY_ASSERTION)){
    //      val classAssertion: OWLObjectPropertyAssertionAxiom = axiom.asInstanceOf[OWLObjectPropertyAssertionAxiom]
    //      val classExpression = classAssertion.getObject.asOWLAnonymousIndividual()
    //      val topClass = dataFactory.getOWLClass(IRI.create("owl:Thing"))
    //      return dataFactory.getOWLSubClassOfAxiom(topClass, classExpression.get)
    //    }
    else if (axiom.getAxiomType.equals(AxiomType.SUBCLASS_OF))
      axiom.asInstanceOf[OWLSubClassOfAxiom].getSuperClass
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
      val classExpression: OWLClassExpression = getClassExpression(axiom)
      val conclusion: ELConceptDescription = ELConceptDescription.of(classExpression)
      val weakenedRHS: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet()

      val (static, _) = input.partition(checker.getStatic)

      val baseAxioms = static ++ justification - axiom

      val testOntology = Util.createManager.createOntology((baseAxioms).asJava)
//      val reasoner = reasonerFactory.createReasoner(testOntology)
      val nextCandidates: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet(
        Util.upperNNeighborsOntology(testOntology,
          conclusion: ELConceptDescription,
          reasonerFactory: OWLReasonerFactory,
          dataFactory: OWLDataFactory))
//      val conceptnames_1 = conclusion.getConceptNames
//      val existentialrestricions_1 = conclusion.getExistentialRestrictions
//      dataFactory.getowlobjectin
//      reasoner.isSatisfiable(classExpression.getClassesInSignature.toArray()(0).asInstanceOf[OWLClassExpression])

      while (!nextCandidates.isEmpty) {
        val processedCandidates: java.util.Set[ELConceptDescription] = new java.util.HashSet(nextCandidates)
        nextCandidates.parallelStream().forEach(candidate ⇒ {
        val weakenedAxiom: OWLAxiom =
          createWeakenedAxiom(axiom, candidate, dataFactory)
        if (checker.isEntailed(baseAxioms + weakenedAxiom)) {

          val new_next_candidates: java.util.Set[ELConceptDescription] = Sets.newConcurrentHashSet(
            Util.upperNNeighborsOntology(testOntology,
              candidate: ELConceptDescription,
              reasonerFactory: OWLReasonerFactory,
              dataFactory: OWLDataFactory))
          nextCandidates.addAll(new_next_candidates)
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


      // TODO evaluate by expanding reasoner of eldescription
      val nonMinimalWeakenings: java.util.Set[OWLAxiom] = Sets.newConcurrentHashSet()
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
      weakening.stream().parallel().forEach(weakening2 ⇒ {
        val reasoner: OWLReasoner =
          reasonerFactory.createReasoner(Util.createManager.createOntology((baseAxioms + weakening2).asJava))
        order.col(weakening2).stream().sequential().forEach(weakening1 ⇒ {
          if (!(weakening1 equals weakening2))
            if (!(reasoner isEntailed weakening1))
              nonMinimalWeakenings add weakening2
        })
        System.gc()
      })
      weakening.removeAll(nonMinimalWeakenings)
      weakening.asScala.toSet
    }
}