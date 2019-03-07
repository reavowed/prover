package net.prover.model.expressions

import net.prover.model.Substitutions

case class FunctionParameter(index: Int, level: Int)(val name: Option[String]) extends Term {
  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0) = {
    if (level >= internalDepth) {
      FunctionParameter(index, level + numberOfParametersToInsert)(name)
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
      Some(FunctionParameter(index, level - numberOfParametersToRemove)(name))
  }

  override def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ): Term = {
    if (level == internalDepth + externalDepth)
      targetArguments(index).insertExternalParameters(internalDepth)
    else
      this
  }

  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    if (level == internalDepth + externalDepth)
      targetArguments(index).applySubstitutions(substitutions, 0, externalDepth).map(_.insertExternalParameters(internalDepth))
    else
      Some(this.insertExternalParameters(previousInternalDepth, internalDepth))
  }

  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ) = {
    other match {
      case FunctionParameter(`index`, `level`) if level < internalDepth =>
        Seq(substitutions)
      case _ =>
        Nil
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
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Seq[(Term, Substitutions)] = {
    (super.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      (if (level >= internalDepth + previousInternalDepth)
        // External context
        // Shifted down to cut out the shared internal context
        Seq(FunctionParameter(index, level - previousInternalDepth)(name) -> substitutions)
      else if (level < internalDepth)
        // Internal context after the entry point to calculateApplicatives
        Seq(this -> substitutions)
      else
        // Shared internal context - must be passed in via the arguments
        Nil)
    ).distinct
  }

  def matchesStructure(other: Expression): Boolean = {
    other match {
      case FunctionParameter(`index`, `level`) => true
      case _ => false
    }
  }

  override def serialized = (0 to level).map(_ => "$").mkString("") + index
  override def serializedForHash = serialized
  override def toString = serialized
}

object FunctionParameter {
  def apply(name: String, index: Int): FunctionParameter = {
    apply(name, index, 0)
  }
  def apply(name: String, index: Int, level: Int): FunctionParameter = {
    FunctionParameter(index, level)(Some(name))
  }
  def anonymous(index: Int, level: Int): FunctionParameter = {
    FunctionParameter(index, level)(None)
  }
}
