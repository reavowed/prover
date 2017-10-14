package net.prover.model.expressions

import net.prover.model.Substitutions

case class FunctionParameter(index: Int, level: Int, depth: Int)(val name: Option[String]) extends Term {
  override def specify(targetArguments: ArgumentList): Term = {
    if (level == 1) {
      targetArguments(index)
    } else {
      FunctionParameter(index, level - 1, depth - 1)(name)
    }
  }
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions,
    outerDepth: Int
  ) = {
    if (level == 1) {
      targetArguments(index).applySubstitutions(substitutions).map { result =>
        result.increaseDepth(depth - substitutions.depth - 1, result.depth)
      }
    } else if (level <= substitutions.depth + 1) {
      Some(FunctionParameter(index, level - 1, depth + outerDepth - 1)(name))
    } else {
      Some(FunctionParameter(index, level + outerDepth - 1, depth + outerDepth - 1)(name))
    }
  }
  override def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    FunctionParameter(
      index,
      if (level <= insertionPoint) level else level + additionalDepth,
      depth + additionalDepth)(
      name)
  }
  override def reduceDepth(difference: Int) = {
    if (level <= depth - difference)
      Some(FunctionParameter(index, level, depth - difference)(name))
    else
      None
  }

  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case FunctionParameter(`index`, otherLevel, _) if otherLevel == level + substitutions.depth
      =>
        Seq(substitutions)
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    Some(FunctionParameter(index, level + substitutions.depth, depth + substitutions.depth)(name))
  }
  override def calculateApplicatives(baseArguments: ArgumentList, substitutions: Substitutions): Seq[(Term, Substitutions)] = {
    (super.calculateApplicatives(baseArguments, substitutions) ++
      (if (level <= substitutions.depth)
        Seq(FunctionParameter(index, level + 1, depth - baseArguments.depth + 1)(name) -> substitutions)
      else if (level > substitutions.depth + baseArguments.depth)
        Seq(FunctionParameter(index, level - baseArguments.depth + 1, depth - baseArguments.depth + 1)(name) -> substitutions)
      else
        Nil)
    ).distinct
  }

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