package edu.hagenberg

import com.google.common.collect.Sets
import conexp.fx.core.collections.Collections3
import conexp.fx.core.dl.ELConceptDescription
import edu.hagenberg.weaken.WeakeningRelation
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import java.io.File
import java.util.function.BiPredicate
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

  def getAxiomsFromFile(file: File, irimapper: List[OWLOntologyIRIMapper] = null): Set[OWLAxiom] = {
    val manager = createManager
    if (irimapper != null){
      irimapper.foreach{
        manager.getIRIMappers.add(_)
      }
    }
    manager.loadOntologyFromOntologyDocument(file).getAxioms().asScala.toSet
  }

  def createManager: OWLOntologyManager = {
    OWLManager.createOWLOntologyManager()
  }

  def getWeakened(input: Set[OWLAxiom],
                  just: Set[OWLAxiom],
                  selected: OWLAxiom,
                  finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                  reasoner: OWLReasonerFactory): Set[OWLAxiom] = {
    val currentWeakeningRelation =
      selected.getAxiomType match {
        case AxiomType.CLASS_ASSERTION | AxiomType.SUBCLASS_OF =>
          WeakeningRelation.semanticELConceptInclusionWeakeningRelation
        case AxiomType.OBJECT_PROPERTY_ASSERTION => WeakeningRelation.elPropertyWeakeningRelation
        case _ => WeakeningRelation.classicalWeakeningRelation
      }
    val weakenedSet: Set[OWLAxiom] = currentWeakeningRelation.getWeakened(input, finder, just, selected, reasoner)
    weakenedSet
//
//    val chosen_weakened = Util.getRandomElement(weakened)
//    chosen_weakened match {
//      case Some(found) => Some(found)
//      case _ => None
//    }
  }


  def getWeakenedSet(input: Set[OWLAxiom],
                     just: Set[OWLAxiom],
                     finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                     reasoner: OWLReasonerFactory):
  (OWLAxiom, Option[OWLAxiom]) ={
    val selected: OWLAxiom = Util.getRandomElement(just).get
    val weakendSet = getWeakened(input, just, selected, finder, reasoner)
    val chosen_weakened = Util.getRandomElement(weakendSet)
    (selected, chosen_weakened)
  }

  def upperNNeighborsOntology(ontology: OWLOntology,
                                concept: ELConceptDescription,
                                reasonerFactory: OWLReasonerFactory,
                                factory: OWLDataFactory,
                              n:Integer = Integer.MAX_VALUE) = {
    val concepts_reduced: java.util.Set[ELConceptDescription] =
      m_upperNNeighborsOntology(ontology, concept, reasonerFactory, factory, n)
    concepts_reduced
  }

//  def subsumedByELCD(ontology: OWLOntology,
//                     concept1: ELConceptDescription,
//                     concept2: ELConceptDescription,
//                     reasonerFactory: OWLReasonerFactory,
//                     factory: OWLDataFactory,
//                     n:Integer = Integer.MAX_VALUE) = {
//    reasoner = reasonerFactory.createReasoner()
//
//  }

  def m_upperNNeighborsOntology(ontology: OWLOntology,
                                concept: ELConceptDescription,
                                reasonerFactory: OWLReasonerFactory,
                                factory: OWLDataFactory,
                                n:Integer) = {

    val reasoner = reasonerFactory.createReasoner(ontology)


    val predicate = new BiPredicate[ELConceptDescription, ELConceptDescription] {
      def test(x: ELConceptDescription, y: ELConceptDescription) =
        (x.subsumes((y)) && y.subsumes(x)) //  || subsumedBy(x.toOWLClassExpression, y.toOWLClassExpression)
    }

    def get_super_classes(iri: IRI) = {
      reasoner.getSuperClasses(factory.getOWLClass(iri),true)
    }

    def subsumedBy(ce1: OWLClassExpression, ce2: OWLClassExpression) = {
      val subClassOfAxiom = factory.getOWLSubClassOfAxiom(ce1, ce2)
      reasoner.isEntailed(subClassOfAxiom)
    }

    def subsumedByIRI(iri1: IRI, iri2: IRI) = {
      val subClassOfAxiom = factory.getOWLSubClassOfAxiom(factory.getOWLClass(iri1), factory.getOWLClass(iri2))
      reasoner.isEntailed(subClassOfAxiom)
    }

    def m_upperNNeighborsReduced(concept: ELConceptDescription, n: Integer): java.util.Set[ELConceptDescription] = {
      val reducedForm: ELConceptDescription = concept.reduce()

      val upperNNeighborsFromConceptNames: java.util.stream.Stream[ELConceptDescription] = {
        reducedForm.getConceptNames.stream().map(
          A => {
            val superClasses = get_super_classes(A)
            val upperNeighbor = reducedForm.clone()
            upperNeighbor.getConceptNames.remove(A)
            upperNeighbor.getConceptNames.addAll((superClasses.entities().map[IRI](_.getIRI)).collect(Collectors.toSet[IRI]))

            val orig_concepts: java.util.Set[IRI] = new java.util.HashSet(upperNeighbor.getConceptNames)
            upperNeighbor.getConceptNames.removeIf(
              parent =>
                orig_concepts.stream()
                .filter(other => !other.equals(parent))
                .anyMatch(child => subsumedByIRI(child, parent))
            )
            upperNeighbor
//            superClasses.entities().map[ELConceptDescription](
//              superclass => {
//                val upperNeighbor = reducedForm.clone()
//                upperNeighbor.getConceptNames.remove(A)
//                upperNeighbor.getConceptNames.add(superclass.getIRI)
//                upperNeighbor
//              }
//            )
          }
        )
      }
      val upperNNeighborsFromDataValues: java.util.stream.Stream[ELConceptDescription] = {
        reducedForm.getDataValues.entries().stream().map(
          V => {
            val upperNeighbor = reducedForm.clone()
            upperNeighbor.getDataValues.remove(V.getKey, V.getValue)
            upperNeighbor
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
                  .stream()
                  .filter(
                    uER => reducedForm.getExistentialRestrictions.entries
                      .parallelStream()
                      .filter(otherER => !otherER.equals(ER))
                      .filter(otherER => ER.getKey.equals(otherER.getKey))
                      .map[ELConceptDescription](entry => entry.getValue)
                      .noneMatch(child => uER.subsumes(child) || subsumedBy(child.toOWLClassExpression, uER.toOWLClassExpression)))
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
          java.util.stream.Stream.concat(
            upperNNeighborsFromConceptNames,
            upperNNeighborsFromDataValues),
          upperNNeighborsFromExistentialRestriction)
      //upperNNeighborsFromConceptNames.collect(Collectors.toSet[ELConceptDescription])
      //stream.collect(Collectors.toSet[ELConceptDescription])
      Collections3
        .representatives(
          stream
            .collect(Collectors.toSet()),
          predicate);
      }
    m_upperNNeighborsReduced(concept, n)
  }
}





