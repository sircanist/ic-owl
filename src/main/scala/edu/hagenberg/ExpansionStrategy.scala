package edu.hagenberg

import edu.hagenberg.Util.{createManager, getManager}
import org.semanticweb.owlapi.apibinding.OWLFunctionalSyntaxFactory
import org.semanticweb.owlapi.model.{OWLAxiom, OWLOntology}

import scala.annotation.tailrec
import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}


trait ExpansionStrategy[E, I]{
  def doExpansion(axioms: Set[I], checker: Checker[E, I] ): Option[Set[I]]
}


object ExpansionStrategy {
  val expansion_ontology: OWLOntology = getManager.createOntology(OWLFunctionalSyntaxFactory.IRI("expansion"))
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
      @tailrec def addWhile(ont: OWLOntology, expansion: Set[OWLAxiom]): Option[Set[OWLAxiom]] ={
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
          addWhile(ont, new_expansion)
          }
        }
      val axiomsJava = axioms.asJava
      expansion_ontology.addAxioms(axiomsJava)
      var (_, refutable) = axioms.partition(checker.getStatic)
      val result = addWhile(expansion_ontology, checker.getModule(refutable))
      expansion_ontology.removeAxioms(axiomsJava)
      result
    }
  }
}
