package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}
import net.prover.model.entries.StatementDefinition

case class DefinedPredicate(
    definition: StatementDefinition,
    components: Seq[ExpressionFunction[Expression]])(
    scopedBoundVariableNames: Seq[String])
  extends Predicate
{
  override def apply(term: Term) = DefinedStatement(components.map(_(term)), definition)(scopedBoundVariableNames)

  override def boundVariables = components.flatMap(_.boundVariables).toSet
  override def requiredSubstitutions = components.map(_.requiredSubstitutions).foldTogether
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case DefinedPredicate(`definition`, otherComponents) =>
        components.calculateSubstitutions(otherComponents, substitutions, boundVariableCount + scopedBoundVariableNames.length)
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
  override def replacePlaceholder(other: Expression) = {
    copy(components = components.map(_.replacePlaceholder(other)))(scopedBoundVariableNames)
  }

  override def serialized = s"defined ${definition.symbol} ${components.map(_.serialized).mkString(" ")}"
  override def toString = definition.format(scopedBoundVariableNames ++ components.map(_.safeToString))
  override def safeToString = definition.format.safe(scopedBoundVariableNames ++ components.map(_.safeToString))
}

object DefinedPredicate {
  def parser(implicit context: ParsingContext): Parser[DefinedPredicate] = {
    for {
      definition <- Parser.selectWord("statement type") {
        case context.RecognisedStatementDefinition(statementDefinition) => statementDefinition
      }
      boundVariableNames <- Parser.nWords(definition.boundVariableNames.length)
      updatedContext = context.addBoundVariables(boundVariableNames)
      components <- definition.defaultVariables.applicativesParser(updatedContext)
    } yield DefinedPredicate(definition, components)(boundVariableNames)
  }
}
