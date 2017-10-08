package net.prover.model.expressions

import net.prover.model.Substitutions

case class FunctionParameter(index: Int, level: Int, depth: Int)(val name: Option[String]) extends Function {
  override def apply(arguments: Seq[Objectable]): Objectable = {
    if (level == 1) {
      arguments(index)
    } else {
      FunctionParameter(index, level - 1, depth - 1)(name)
    }
  }
  override def increaseDepth(additionalDepth: Int) = {
    FunctionParameter(index, level + additionalDepth, depth + additionalDepth)(name)
  }

  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    if (other == this)
      Seq(substitutions)
    else
      Nil
  }
  override def applySubstitutions(substitutions: Substitutions) = Some(this)
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions): Seq[(Function, Substitutions)] = {
    super.calculateApplicatives(arguments, substitutions) ++ Seq(FunctionParameter(index, level + 1, depth + 1)(name) -> substitutions)
  }
  override def replacePlaceholder(other: Expression) = this

  override def serialized: String = ((1 to level).map(_ => "$") ++ (0 until (depth - level)).map(_ => ".")).mkString("") + index
  override def toString = name.getOrElse(serialized)
  override def safeToString = name.getOrElse(serialized)
}

object FunctionParameter {
  def apply(name: String, index: Int): FunctionParameter = {
    apply(name, index, 1)
  }
  def apply(name: String, index: Int, levelAndDepth: Int): FunctionParameter = {
    apply(name, index, levelAndDepth, levelAndDepth)
  }
  def apply(name: String, index: Int, level: Int, depth: Int): FunctionParameter = {
    FunctionParameter(index, level, depth)(Some(name))
  }
  def anonymous(index: Int): FunctionParameter = anonymous(index, 1)
  def anonymous(index: Int, levelAndDepth: Int): FunctionParameter = {
    anonymous(index, levelAndDepth, levelAndDepth)
  }
  def anonymous(index: Int, level: Int, depth: Int): FunctionParameter = {
    FunctionParameter(index, level, depth)(None)
  }
}