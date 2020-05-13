package net.prover.model.utils

import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions.{DefinedTerm, Term, TermVariable}

object TermUtils {
  def getSimpleTermVariable(term: Term): Option[String] = {
    term.asOptionalInstanceOf[TermVariable].filter(_.arguments.isEmpty).map(_.name)
  }
  def isSimpleTermVariable(term: Term): Boolean = {
    getSimpleTermVariable(term).isDefined
  }
  def getTermConstantDefinition(term: Term): Option[TermDefinition] = {
    term.asOptionalInstanceOf[DefinedTerm].filter(_.components.isEmpty).map(_.definition)
  }
  def isTermConstant(term: Term): Boolean = {
    getTermConstantDefinition(term).isDefined
  }
  def isCombinationOfConstants(t: Term): Boolean = {
    isTermConstant(t) || t.asOptionalInstanceOf[DefinedTerm].map(_.components).exists {
      case Seq(t1: Term, t2: Term) if isCombinationOfConstants(t1) && isCombinationOfConstants(t2) =>
        true
      case _ =>
        false
    }
  }
  def isWrappedSimpleTerm(t: Term): Boolean = {
    t.asOptionalInstanceOf[DefinedTerm].map(_.components).exists {
      case Seq(t1: Term) if isSimpleTermVariable(t1) || isTermConstant(t1) =>
        true
      case _ =>
        false
    }
  }
}
