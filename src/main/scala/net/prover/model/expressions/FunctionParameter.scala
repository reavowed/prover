package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.definitions.ExpressionDefinition

case class FunctionParameter(index: Int, level: Int) extends Term {
  override def structuralComplexity: Int = 1
  override def definitionalComplexity: Int = 1
  override def definitionUsages: DefinitionUsages = DefinitionUsages.empty

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0) = {
    if (level >= internalDepth) {
      FunctionParameter(index, level + numberOfParametersToInsert)
    } else {
      this
    }
  }
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0) = {
    if (level < internalDepth)
      Some(this)
    else if (level < internalDepth + numberOfParametersToRemove)
      None
    else
      Some(FunctionParameter(index, level - numberOfParametersToRemove))
  }
  override def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): FunctionParameter = this

  override def specify(
    targetArguments: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[Term] = {
    if (level == internalDepth + externalDepth)
      targetArguments.get(index).map(_.insertExternalParameters(internalDepth))
    else
      Some(this)
  }

  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    if (level == internalDepth + externalDepth)
      targetArguments(index).applySubstitutions(substitutions, previousInternalDepth, externalDepth).map(_.insertExternalParameters(internalDepth))
    else
      Some(this.insertExternalParameters(previousInternalDepth, internalDepth))
  }

  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Substitutions.Possible] = {
    other match {
      case FunctionParameter(`index`, `level`) =>
        Some(substitutions)
      case _ =>
        None
    }
  }
  override def applySubstitutions(
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ) = {
    Some(this)
  }
  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, Substitutions.Possible)] = {
    (super.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth).toSet ++
      (if (level >= internalDepth + previousInternalDepth)
        // External context
        // Shifted down to cut out the shared internal context
        Seq(FunctionParameter(index, level - previousInternalDepth) -> substitutions)
      else if (level < internalDepth)
        // Internal context after the entry point to calculateApplicatives
        Seq(this -> substitutions)
      else
        // Shared internal context - must be passed in via the arguments
        Nil)
    ).iterator
  }
  override def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    previousInternalDepth: Int,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]] = {
    if (level == internalDepth + externalDepth) {
      for {
        argument <- target.asOptionalInstanceOf[Term].flatMap(_.removeExternalParameters(internalDepth))
        result <- argumentsSoFar.tryAdd(index, argument)
      } yield result
    } else if (target.removeExternalParameters(previousInternalDepth, internalDepth).contains(this)) {
      Some(argumentsSoFar)
    } else {
      None
    }
  }

  override def serialized = (0 to level).map(_ => "$").mkString("") + index
  override def serializedForHash = serialized
  override def toString = serialized
}
