package net.prover.model.expressions

import net.prover.model.Substitutions

case class FunctionParameter(index: Int, level: Int, depth: Int)(val name: Option[String]) extends Term {
  override def specify(targetArguments: ArgumentList): Term = {
    if (level == 1) {
      val result = targetArguments(index)
      result.increaseDepth(depth - result.depth - 1, result.depth)
    } else {
      FunctionParameter(index, level - 1, depth - 1)(name)
    }
  }

  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions
  ) = {
    if (level == 1) {
      // result.depth = substitutions.depth + targetArguments.depth
      targetArguments(index).applySubstitutions(substitutions).map { result =>
        result.increaseDepth(depth - substitutions.depth - 1, result.depth)
      }
    } else if (level <= substitutions.depth + 1) {
      // If we refer to one of the external scoped variables, maintain that reference
      Some(FunctionParameter(index, level - 1, depth + targetArguments.depth - 1)(name))
    } else {
      // Otherwise we refer to an interior bound variable of the local predicate, so increase the level to
      // make room for the bound variables outside the local predicate that are referred to in targetArguments
      Some(FunctionParameter(index, level + targetArguments.depth - 1, depth + targetArguments.depth - 1)(name))
    }
  }
  override def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    FunctionParameter(
      index,
      if (level <= insertionPoint) level else level + additionalDepth,
      depth + additionalDepth)(
      name)
  }
  override def reduceDepth(difference: Int, insertionPoint: Int) = {
    if (level <= insertionPoint)
      Some(FunctionParameter(index, level, depth - difference)(name))
    else if (level > insertionPoint + difference)
      Some(FunctionParameter(index, level - difference, depth - difference)(name))
    else
      None
  }

  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ) = {
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
      (if (level <= substitutions.depth) // level is 1-indexed
        // External context - shifted up 1 to allow for the applicable
        Seq(FunctionParameter(index, level + 1, depth - baseArguments.depth + 1)(name) -> substitutions)
      else if (level > substitutions.depth + baseArguments.depth)
        // Internal context after the entry point to calculateApplicatives
        // Shifted down to cut out the shared internal context
        Seq(FunctionParameter(index, level - baseArguments.depth + 1, depth - baseArguments.depth + 1)(name) -> substitutions)
      else
        // Shared internal context - must be passed in via the arguments
        Nil)
    ).distinct
  }

  def matchesStructure(other: Expression): Boolean = {
    other match {
      case FunctionParameter(`index`, `level`, _) => true
      case _ => false
    }
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