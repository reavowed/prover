package net.prover.model.expressions

import net.prover.model.{ParsingContext, Substitutions}

import scala.collection.immutable.Nil

case class StatementVariable(name: String) extends Statement with Variable {
  override def boundVariables = Set.empty
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case otherStatement: Statement =>
        substitutions.addVariable(this, otherStatement).toSeq
      case _ =>
        Nil
    }
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.expressionsByVariable.get(this).map(_.asInstanceOf[Statement])
  }
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(arguments: Seq[Term], substitutions: Substitutions, boundVariableCount: Int) = {
    Seq((ConstantPredicate(this), substitutions))
  }
  override def makeApplicative(argument: Term) = Some(PredicateVariable(name).apply(Seq(argument)))

  override def toString: String = name
  override def serialized: String = name

  override def expressionParser(implicit context: ParsingContext) = Statement.parser
  override def applicativeParser(implicit context: ParsingContext) = Predicate.parser
}
