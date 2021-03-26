package edu.hagenberg

import com.google.common.collect.Sets
import conexp.fx.core.dl.ELConceptDescription
import edu.hagenberg.weaken.WeakeningRelation
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import java.io.File
import java.util.stream.Collectors
import java.util.{Optional, Random}
import scala.collection.JavaConverters.asScalaSetConverter

object Util {

  private val random: Random = new Random()

  def getRandomElement[E](c: java.util.Collection[E]): Optional[E] = {
    if (c.isEmpty)
      Optional.empty()
    else
      c.stream().skip(random.nextInt(c.size())).findFirst()
  }
  def getRandomElement[E](c: Set[E]): Option[E] = {
    if (c.size <= 0)
      None
    else {
      val n = util.Random.nextInt(c.size)
      Some(c.iterator.drop(n).next)
    }
  }

  def getAxiomsFromFile(file: File): Set[OWLAxiom] = {
    createManager.loadOntologyFromOntologyDocument(file).getAxioms().asScala.toSet
  }


  def createManager: OWLOntologyManager = {
    OWLManager.createOWLOntologyManager()
  }

  def getWeakened(input: Set[OWLAxiom],
                  just: Set[OWLAxiom],
                  selected: OWLAxiom,
                  finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                  reasoner: OWLReasonerFactory): Option[OWLAxiom] = {
    val currentWeakeningRelation =
      selected.getAxiomType match {
        case AxiomType.CLASS_ASSERTION | AxiomType.SUBCLASS_OF =>
          WeakeningRelation.semanticELConceptInclusionWeakeningRelation
        case AxiomType.OBJECT_PROPERTY_ASSERTION => WeakeningRelation.elPropertyWeakeningRelation
        case _ => WeakeningRelation.classicalWeakeningRelation
      }
    val weakened: Set[OWLAxiom] = currentWeakeningRelation.getWeakened(input, finder, just, selected, reasoner)
    val chosen_weakened = Util.getRandomElement(weakened)
    chosen_weakened match {
      case Some(found) => Some(found)
      case _ => None
    }
  }


  def getWeakened2(input: Set[OWLAxiom],
                  just: Set[OWLAxiom],
                  selected: OWLAxiom,
                  finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                  reasoner: OWLReasonerFactory): Option[OWLAxiom] = {
    val currentWeakeningRelation =
    selected.getAxiomType match {
      case AxiomType.CLASS_ASSERTION | AxiomType.SUBCLASS_OF =>
        WeakeningRelation.semanticELConceptInclusionWeakeningRelation
      case AxiomType.OBJECT_PROPERTY_ASSERTION => WeakeningRelation.elPropertyWeakeningRelation
      case _ => WeakeningRelation.classicalWeakeningRelation
    }
    val weakened: Set[OWLAxiom] = currentWeakeningRelation.getWeakened(input, finder, just, selected, reasoner)
    val chosen_weakened = Util.getRandomElement(weakened)
    chosen_weakened match {
      case Some(found) => Some(found)
      case _ => None
    }
  }

  def getWeakenedSet(input: Set[OWLAxiom],
                     just: Set[OWLAxiom],
                     finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                     reasoner: OWLReasonerFactory):
  (OWLAxiom, Option[OWLAxiom]) ={
    val selected: OWLAxiom = Util.getRandomElement(just).get
    (selected, getWeakened(input, just, selected, finder, reasoner))
  }

  def upperNNeighborsOntology(ontology: OWLOntology,
                                concept: ELConceptDescription,
                                reasonerFactory: OWLReasonerFactory,
                                factory: OWLDataFactory,
                              n:Integer = Integer.MAX_VALUE) = {
    val concepts_reduced: java.util.Set[ELConceptDescription] =
      m_upperNNeighborsOntology(ontology, concept, reasonerFactory, factory, n)
    val searched_upper_concepts: java.util.Set[ELConceptDescription] =
      concept.upperNNeighborsReduced(n)
    concepts_reduced.addAll(searched_upper_concepts)
    concepts_reduced

  }

  def m_upperNNeighborsOntology(ontology: OWLOntology,
                                concept: ELConceptDescription,
                                reasonerFactory: OWLReasonerFactory,
                                factory: OWLDataFactory,
                                n:Integer) = {

    val reasoner = reasonerFactory.createReasoner(ontology)

    def get_super_classes(iri: IRI) = {
      reasoner.getSuperClasses(factory.getOWLClass(iri),true)
    }

    def m_upperNNeighborsReduced(concept: ELConceptDescription, n: Integer): java.util.Set[ELConceptDescription] = {
      val reducedForm: ELConceptDescription = concept.reduce()

      val upperNNeighborsFromConceptNames: java.util.stream.Stream[ELConceptDescription] = {
        reducedForm.getConceptNames.stream().flatMap(
          A => {
            val superClasses = get_super_classes(A)
            superClasses.entities().map[ELConceptDescription](
              superclass => {
                val upperNeighbor = reducedForm.clone()
                upperNeighbor.getConceptNames.remove(A)
                upperNeighbor.getConceptNames.add(superclass.getIRI)
                upperNeighbor
              }
            )
          }
        )
      }
      val _upperNNeighborsFromExistentialRestrictions: java.util.stream.Stream[ELConceptDescription] = {
        reducedForm.getExistentialRestrictions.entries().stream().flatMap(
          ER => {
            val all_uERs = m_upperNNeighborsReduced(ER.getValue, n)
            val combinations: java.util.stream.Stream[ELConceptDescription] =
              Sets.combinations(all_uERs, Math.min(n, all_uERs.size())).stream().map[ELConceptDescription](uErs => {
                val upperNeighbor = reducedForm.clone()
                upperNeighbor.getExistentialRestrictions.remove(ER.getKey, ER.getValue)
                uErs
                  .parallelStream()
                  .filter(
                    uER => reducedForm.getExistentialRestrictions.entries
                      .parallelStream()
                      .filter(otherER => !otherER.equals(ER))
                      .filter(otherER => ER.getKey.equals(otherER.getKey))
                      .map[ELConceptDescription](entry => entry.getValue)
                      .noneMatch(uER.subsumes(_)))
                  .sequential()
                  .forEach(uER => upperNeighbor.getExistentialRestrictions.put(ER.getKey, uER))
                upperNeighbor
              }
            )
            combinations
          }
        )
      }
      val upperNNeighborsFromExistentialRestriction: java.util.stream.Stream[ELConceptDescription] = {
        if (n < Integer.MAX_VALUE)
          _upperNNeighborsFromExistentialRestrictions.filter(other => concept.subsumes(other))
        else
          _upperNNeighborsFromExistentialRestrictions
      }
      val stream: java.util.stream.Stream[ELConceptDescription] = java.util.stream.Stream.concat(
          upperNNeighborsFromConceptNames,
          upperNNeighborsFromExistentialRestriction)
      //upperNNeighborsFromConceptNames.collect(Collectors.toSet[ELConceptDescription])
      stream.collect(Collectors.toSet[ELConceptDescription])
      }
    m_upperNNeighborsReduced(concept, n)
  }
}





