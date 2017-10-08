package net.prover.model.expressions

import net.prover.model.{ParsingContext, Substitutions}

import scala.collection.immutable.Nil

case class StatementVariable(name: String) extends Statement with Variable {
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case otherStatement: Assertable =>
        substitutions.addVariable(this, otherStatement).toSeq
      case _ =>
        Nil
    }
  }
  def applySubstitutions(substitutions: Substitutions) = {
    substitutions.expressionsByVariable.get(this).map(_.asInstanceOf[Assertable])
  }
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    Seq((ConstantPredicate(this, 1), substitutions))
  }
  override def makeApplicative = Some(MetaPredicateApplication(PredicateVariable(name), Seq(FunctionParameter.anonymous(0)), 0))
  override def increaseDepth(additionalDepth: Int) = ConstantPredicate(this, additionalDepth)

  override def toString: String = name
  override def serialized: String = name

  override def expression = this
  override def expressionParser(parameterList: Seq[String])(implicit context: ParsingContext) = Assertable.parser
  override def depthDifference(expression: Expression): Option[Int] = {
    expression match {
      case _: Statement => Some(0)
      case predicate: Predicate => Some(predicate.depth)
      case _ => None
    }
  }
}
