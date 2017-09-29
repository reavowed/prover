package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Predicate extends ExpressionFunction[Statement] with Assertable {
  def applySubstitutions(substitutions: Substitutions): Option[Predicate]
  def replacePlaceholder(other: Expression): Predicate
}

object Predicate {
  def parser(implicit parsingContext: ParsingContext): Parser[Predicate] = {
    Parser.selectWordParser("predicate") {
      case "constant" => Statement.parser.map(ConstantPredicate.apply)
      case "defined" => DefinedPredicate.parser
      case "named" => PredicateVariable.parser
    }
  }
}
