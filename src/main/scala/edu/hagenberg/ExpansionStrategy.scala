package edu.hagenberg

import edu.hagenberg.Util.createManager
import org.semanticweb.owlapi.model.{OWLAxiom, OWLOntology}

import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}


trait ExpansionStrategy[E, I]{
  def doExpansion(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}


object ExpansionStrategy {
  def simpleExpansionStrategy[E, I]: ExpansionStrategy[E, I] = {
    (axioms, checker) => {
      def addWhile(axioms: Seq[I], newelems: Set[I]): Option[Set[I]] ={
        axioms match{
          case x +: Seq() =>
            Some(newelems + x).filter(checker.isEntailed)
          case x +: xs =>
            Some(newelems + x).filter(checker.isEntailed).orElse(addWhile(xs, newelems + x))
          case _ =>
            None
        }
      }
      val (static, expander) = axioms.partition(checker.getStatic)
      addWhile(expander.toSeq, static)
    }
  }

  def structuralExpansionStrategy: ExpansionStrategy[java.util.Set[OWLAxiom], OWLAxiom]  = {
    (axioms, checker) => {
      val (static, refutable) = axioms.partition(checker.getStatic)
      def addWhile(ont: OWLOntology, expansion: Set[OWLAxiom], static: Set[OWLAxiom]): Option[Set[OWLAxiom]] ={
        if (checker.isEntailed(expansion))
          Some(expansion)
        else if (expansion.equals(axioms))
          None
        else {
          var new_expansion = expansion
          expansion.foreach(
            axiom =>  axiom.getSignature.forEach(
              ent => {
                val referencingAxioms = ont.getReferencingAxioms(ent).asScala
                new_expansion = new_expansion ++ referencingAxioms
              }
            )
          )
          // cannot expand further, take all axioms to be sure that we can go on
          if (expansion.size == new_expansion.size){
            new_expansion = axioms
          }
          addWhile(ont, new_expansion, static)
          }
        }
      val manager = createManager
      val ont: OWLOntology = manager.createOntology(axioms.asJava)
      addWhile(ont, checker.getModule(refutable), static)
    }
  }
}
