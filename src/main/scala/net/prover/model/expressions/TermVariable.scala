package net.prover.model.expressions

import net.prover.model.{ParsingContext, Substitutions}

import scala.collection.immutable.Nil

case class TermVariable(name: String) extends Term with Variable {
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case otherTerm: Objectable =>
        substitutions.addVariable(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    substitutions.expressionsByVariable.get(this).map(_.asInstanceOf[Objectable])
  }
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    super.calculateApplicatives(arguments, substitutions) :+ (ConstantFunction(this, 1), substitutions)
  }
  override def increaseDepth(additionalDepth: Int) = ConstantFunction(this, additionalDepth)
  override def toString: String = name
  override def serialized: String = name

  override def expression = this
  override def expressionParser(parameterList: Seq[String])(implicit context: ParsingContext) = Objectable.parser
  override def depthDifference(expression: Expression): Option[Int] = {
    expression match {
      case _: Term => Some(0)
      case function: Function => Some(function.depth)
      case _ => None
    }
  }
}
