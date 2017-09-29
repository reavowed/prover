package net.prover.model.expressions

import net.prover.model.{Html, ParsingContext, Substitutions}

import scala.collection.immutable.Nil

case class TermVariable(name: String) extends Term with Variable {
  override def boundVariables = Set.empty
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case otherTerm: Term if otherTerm.boundVariables.forall(_ >= boundVariableCount) =>
        substitutions.addVariable(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.expressionsByVariable.get(this).map(_.asInstanceOf[Term])
  }
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    super.calculateApplicatives(argument, substitutions, boundVariableCount) :+ (ConstantFunction(this), substitutions)
  }
  override def toString: String = name
  override def serialized: String = name

  override def expressionParser(implicit context: ParsingContext) = Term.parser
  override def applicativeParser(implicit context: ParsingContext) = Function.parser
}
