package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.Substitutions
import net.prover.model.entries.ExpressionDefinition

@JsonSerialize(using = classOf[FunctionParameterSerializer])
case class FunctionParameter(index: Int, level: Int) extends Term {
  override def complexity: Int = 0
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
      targetArguments(index).applySubstitutions(substitutions, previousInternalDepth, externalDepth).map(_.insertExternalParameters(internalDepth))
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
      case FunctionParameter(`index`, `level`) =>
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
  super.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
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
  }

  override def serialized = (0 to level).map(_ => "$").mkString("") + index
  override def serializedForHash = serialized
  override def toString = serialized
}

private class FunctionParameterSerializer extends JsonSerializer[FunctionParameter] {
  override def serialize(value: FunctionParameter, gen: JsonGenerator, serializers: SerializerProvider) = {
    gen.writeStartArray(2)
    gen.writeNumber(value.level)
    gen.writeNumber(value.index)
    gen.writeEndArray()
  }
}
