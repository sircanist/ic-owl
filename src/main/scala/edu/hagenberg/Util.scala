package edu.hagenberg

import edu.hagenberg.weaken.WeakeningRelation
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AxiomType, OWLAxiom, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

import java.io.File
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


  def getWeakenedSet(input: Set[OWLAxiom],
                     just: Set[OWLAxiom],
                     finder: JustificationFinder[java.util.Set[OWLAxiom], OWLAxiom],
                     reasoner: OWLReasonerFactory):
  (OWLAxiom, Option[OWLAxiom]) ={
    val selected: OWLAxiom = Util.getRandomElement(just).get
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
      case Some(found) => (selected, Some(found))
      case _ => (selected, None)
    }
  }
}
