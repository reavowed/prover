package net.prover.model.expressions

import net.prover.model._
import net.prover.model.entries.StatementDefinition

case class DefinedPredicate(
    components: Seq[ExpressionFunction[Expression]],
    definition: StatementDefinition,
    depth: Int)(
    scopedBoundVariableNames: Seq[String])
  extends Predicate
{
  override def apply(arguments: Seq[Objectable]) = {
    if (depth == 1) {
      DefinedStatement(components.map(_(arguments)), definition)(scopedBoundVariableNames)
    } else {
      DefinedPredicate(
        components.map(_(arguments).asInstanceOf[ExpressionFunction[Expression]]),
        definition,
        depth - 1)(
        scopedBoundVariableNames)
    }
  }
  override def increaseDepth(additionalDepth: Int) = {
    DefinedPredicate(
      components.map(_.increaseDepth(additionalDepth)),
      definition,
      depth + additionalDepth)(
      scopedBoundVariableNames)
  }

  override def requiredSubstitutions = components.map(_.requiredSubstitutions).foldTogether
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case DefinedPredicate(otherComponents, `definition`, `depth`) =>
        components.calculateSubstitutions(otherComponents, substitutions)
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    for {
      updatedComponents <- components.map(_.applySubstitutions(substitutions)).traverseOption
    } yield {
      copy(components = updatedComponents)(scopedBoundVariableNames)
    }
  }
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    components.foldLeft(Seq((Seq.empty[ExpressionFunction[Expression]], substitutions))) { case (predicatesAndSubstitutionsSoFar, subcomponent) =>
      for {
        (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
        (predicate, newSubstitutions) <- subcomponent.calculateApplicatives(
          arguments,
          substitutionsSoFar)
      } yield (predicatesSoFar :+ predicate, newSubstitutions)
    }.map(_.mapLeft(DefinedPredicate(_, definition, depth + 1)(scopedBoundVariableNames)))
  }

  override def replacePlaceholder(other: Expression) = {
    copy(components = components.map(_.replacePlaceholder(other)))(scopedBoundVariableNames)
  }

  override def serialized = (Seq(definition.symbol) ++ scopedBoundVariableNames ++ components.map(_.serialized)).mkString(" ")
  override def toString = definition.format(scopedBoundVariableNames ++ components.map(_.safeToString))
  override def safeToString = definition.format.safe(scopedBoundVariableNames ++ components.map(_.safeToString))
}

object DefinedPredicate {
  def parser(definition: StatementDefinition)(implicit context: ParsingContext): Parser[DefinedPredicate] = {
    for {
      boundVariableNames <- Parser.nWords(definition.boundVariableNames.length)
      components <- definition.defaultVariables.expressionsParser(boundVariableNames).map(_.map(_.asInstanceOf[ExpressionFunction[Expression]]))
    } yield DefinedPredicate(components, definition, context.parameterDepth)(boundVariableNames)
  }
}
