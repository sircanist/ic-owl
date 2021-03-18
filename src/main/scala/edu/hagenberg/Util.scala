package edu.hagenberg

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLOntology, OWLOntologyManager}

import java.io.File
import java.nio.file.Path
import java.util.{Collection, Optional, Random}
import scala.collection.JavaConverters.asScalaSetConverter

object Util {

  private val random: Random = new Random()

  def getRandomElement[E](c: Collection[E]): Optional[E] = {
    if (c.isEmpty())
      return Optional.empty()
    return c.stream().skip(random.nextInt(c.size())).findFirst()
  }
  def getRandomElement[E](c: Set[E]): Option[E] = {
    if (c.size <= 0)
      None
    else {
      val n = util.Random.nextInt(c.size)
      Some(c.iterator.drop(n).next)
    }
  }

  def getAxiomsFromFile(file: File) = {
    createManager().loadOntologyFromOntologyDocument(file).getAxioms().asScala.toSet
  }


  def createManager(): OWLOntologyManager = {
    OWLManager.createOWLOntologyManager()
  }
}
