package edu.hagenberg

import org.semanticweb.owlapi.model.{AxiomType, OWLAxiom}
import edu.hagenberg.weaken.WeakeningRelation

import java.util


object Algorithms {
  def simple(): Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
    (input, finder) => {
      def findAll(input: Set[OWLAxiom]): Set[OWLAxiom] = {
        val justification = finder.searchOneJustification(input)
        justification match {
          case Some(just) => just ++ findAll(input -- just)
          case None => Set.empty
        }
      }

      val all = findAll(input)
      Some(all)
    }

  def simpleWeakening(): Algorithm[java.util.Set[OWLAxiom], OWLAxiom] =
    (input, finder) => {
      def getWeakenedSet(input: Set[OWLAxiom], just: Set[OWLAxiom]): Set[OWLAxiom] ={
        // TODO improve handling, maybe remove runtime
        val selected: OWLAxiom = Util.getRandomElement(just) match {
          case Some(el) => el
          case None => throw new RuntimeException("No random element can be fetched")
        }
        val atype: AxiomType[_] = selected.getAxiomType()
        val currentWeakeningRelation =
          atype match {
            case AxiomType.CLASS_ASSERTION | AxiomType.SUBCLASS_OF =>
              WeakeningRelation.semanticELConceptInclusionWeakeningRelation(Util.createManager().getOWLDataFactory)
            case AxiomType.OBJECT_PROPERTY_ASSERTION => WeakeningRelation.elPropertyWeakeningRelation(refutableOntology)
            case _ => WeakeningRelation.classicalWeakeningRelation
          }
        val weakened = currentWeakeningRelation.getWeakenings()
        input - selected + weakened

      }

      def findAll(input: Set[OWLAxiom]): Set[OWLAxiom] = {
        val justification = finder.searchOneJustification(input)
        justification match {
          case Some(just) => just ++ findAll(input -- just)
          case None => Set.empty
        }
      }

      val all = findAll(input)
      Some(all)
      }
}