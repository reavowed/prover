package net.prover.model.expressions

import net.prover.model._

trait Predicate extends ExpressionFunction[Assertable] with Assertable {
  def applySubstitutions(substitutions: Substitutions): Option[Predicate]
  def replacePlaceholder(other: Expression): Predicate
}

object Predicate {
  def parser(implicit parsingContext: ParsingContext): Parser[Predicate] = {
    Parser.selectWordParser("predicate") {
      case parsingContext.RecognisedStatementDefinition(definition) =>
        DefinedPredicate.parser(definition)
      case parsingContext.RecognisedStatementVariable(variable) =>
        Parser.constant(ConstantPredicate(variable, parsingContext.parameterDepth))
      case "with" =>
        for {
          arguments <- Objectable.parser.listOrSingle(Some(",")).map(_.map(_.asInstanceOf[Function]))
          name <- Parser.singleWord
        } yield {
            MetaPredicateApplication(PredicateVariable(name), arguments, parsingContext.parameterDepth)
        }
    }
  }
}
