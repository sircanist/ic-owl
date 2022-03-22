package edu.hagenberg

import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory
import wvlet.log.LogSupport
import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}


case class PathElement(justifications: Set[OWLAxiom], selected: OWLAxiom, weakened: Option[OWLAxiom])

class JustificationFinder[E, I](val checker: Checker[E,I],
                                expansionStrategy: ExpansionStrategy[E, I],
                                contractionStrategy: ContractionStrategy[E, I]) extends LogSupport{
    private[this] def expand(workingAxioms: Set[I]): Option[Set[I]] = {
      info("starting expanding")
      val t1 = System.nanoTime
      val expanded = expansionStrategy.doExpansion(workingAxioms, checker)
      val duration = (System.nanoTime - t1) / 1e9d
      val size = {
        if (expanded.isDefined)
          expanded.get.size
        else
          0
    }
    info(s"expansion took $duration, expansion candidates amount: $size")
    expanded
  }


  private[this] def contract(expandedAxioms: Set[I]): Option[Set[I]] = {
    info("starting contraction")
    val t1 = System.nanoTime
    val removeCandidates = expandedAxioms
    val pruned = contractionStrategy.doPruning(removeCandidates, checker)
    val duration = (System.nanoTime - t1) / 1e9d
    val size = {
      if (pruned.isDefined)
        pruned.get.size
      else
        0
    }
    info(s"contraction took $duration, pruned candidates amount $size")
    pruned
  }

  def searchOneJustification(input: Set[I]): Option[Set[I]] = {
    val t1 = System.nanoTime
    val just = expand(input) match {
      case Some(axioms) => contract(axioms)
      case None => None
    }
    val duration = (System.nanoTime - t1) / 1e9d
    info(s"just search took $duration, just: $just")
    just
  }
}

class BlackBoxGenerator[E, I](input: Set[I],
                              static: Set[I],
                              checkerFactory: CheckerFactory[E, I],
                              reasonerFactory: OWLReasonerFactory,
                              expansionStrategy: ExpansionStrategy[E,I],
                              contractionStrategy: ContractionStrategy[E, I],
                              algorithm: Algorithm[E, I],
                              useModularisation: Boolean = true) extends LogSupport {
  def executeAlgorithm(entailment: E): Either[Error, List[List[PathElement]]] ={
    val checker: Checker[E, I] = checkerFactory.createChecker(entailment, static)
    val algorithmInput = if (useModularisation) checker.getModule(input) else input
    val finder: JustificationFinder[E, I] = new JustificationFinder(checker, expansionStrategy, contractionStrategy)
    info("checking start conditions")
    if (checker.isTautology)
      Left(new Error("Tautology"))
    else if (!checker.isEntailed(algorithmInput))
      Left(new Error("Not Entailed"))
    else if (checker.isEntailed(static))
      Left(new Error("Unwanted Axioms are entailed in static"))
    else
      Right(algorithm.findRemoveSet(algorithmInput, finder, reasonerFactory))
  }
}


