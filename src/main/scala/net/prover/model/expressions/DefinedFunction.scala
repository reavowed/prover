package net.prover.model.expressions

import net.prover.model.entries.TermDefinition
import net.prover.model.{Parser, ParsingContext, Substitutions}

case class DefinedFunction(
    components: Seq[ExpressionFunction[Expression]],
    definition: TermDefinition,
    depth: Int)
  extends Function
{
  override def apply(arguments: Seq[Objectable]) = {
    if (depth == 1) {
      DefinedTerm(components.map(_(arguments)), definition)
    } else {
      DefinedFunction(
        components.map(_(arguments).asInstanceOf[ExpressionFunction[Expression]]),
        definition,
        depth - 1)
    }
  }
  override def increaseDepth(additionalDepth: Int) = {
    DefinedFunction(
      components.map(_.increaseDepth(additionalDepth)),
      definition,
      depth + additionalDepth)
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
      copy(components = updatedComponents)
    }
  }
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    super.calculateApplicatives(arguments, substitutions) ++
      components.foldLeft(Seq((Seq.empty[ExpressionFunction[Expression]], substitutions))) { case (predicatesAndSubstitutionsSoFar, subcomponent) =>
        for {
          (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
          (predicate, newSubstitutions) <- subcomponent.calculateApplicatives(
            arguments,
            substitutionsSoFar)
        } yield (predicatesSoFar :+ predicate, newSubstitutions)
      }.map(_.mapLeft(DefinedFunction(_, definition, depth + 1)))
  }

  override def replacePlaceholder(other: Expression) = {
    copy(components = components.map(_.replacePlaceholder(other)))
  }

  override def serialized = (Seq(definition.symbol) ++ components.map(_.serialized)).mkString(" ")
  override def toString = definition.format(components.map(_.safeToString))
  override def safeToString = definition.format.safe(components.map(_.safeToString))
}

object DefinedFunction {
  def parser(definition: TermDefinition)(implicit context: ParsingContext): Parser[DefinedFunction] = {
    for {
      components <- definition.defaultVariables.expressionsParser(Nil).map(_.map(_.asInstanceOf[ExpressionFunction[Expression]]))
    } yield DefinedFunction(components, definition, context.parameterDepth)
  }
}
