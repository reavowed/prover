package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}
import net.prover.model.entries.StatementDefinition

case class DefinedPredicate(
    definition: StatementDefinition,
    components: Seq[ExpressionFunction[Expression]])(
    scopedBoundVariableNames: Seq[String])
  extends Predicate
{
  override def apply(term: Term) = DefinedStatement(components.map(_(term)), definition)(scopedBoundVariableNames)
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
