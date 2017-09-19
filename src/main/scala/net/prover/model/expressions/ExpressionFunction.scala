package net.prover.model.expressions

import net.prover.model.entries.StatementDefinition
import net.prover.model.{Parser, ParsingContext}

sealed trait ExpressionFunction[+T <: Expression] {
  def apply(term: Term): T
  def serialized: String
  def safeToString: String
}

sealed trait Predicate extends ExpressionFunction[Statement]

object Predicate {
  case class Constant(statement: Statement) extends Predicate {
    override def apply(term: Term) = statement
    override def serialized = s"constant ${statement.serialized}"
    override def toString = statement.toString
    override def safeToString = statement.safeToString
  }
  case class Defined(
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
  object Defined {
    def parser(implicit context: ParsingContext): Parser[Defined] = {
      for {
        definition <- Parser.selectWord("statement type") {
          case context.RecognisedStatementDefinition(statementDefinition) => statementDefinition
        }
        boundVariableNames <- Parser.nWords(definition.boundVariableNames.length)
        updatedContext = context.addBoundVariables(boundVariableNames)
        components <- definition.expressionTypes.applicativesParser(updatedContext)
      } yield Defined(definition, components)(boundVariableNames)
    }
  }
  case class Named(name: String) extends Predicate {
    override def apply(term: Term) = PredicateApplication(name, term)
    override def serialized = s"named $name"
    override def toString = name
    override def safeToString = name
  }
  object Named {
    def parser(implicit context: ParsingContext): Parser[Named] = {
      Parser.singleWord.map(Named.apply)
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Predicate] = {
    Parser.selectWordParser("predicate") {
      case "constant" => Statement.parser.map(Constant.apply)
      case "defined" => Defined.parser
      case "named" => Named.parser
    }
  }
}

sealed trait Function extends ExpressionFunction[Term]

object Function {
  case class Constant(term: Term) extends Function {
    override def apply(otherTerm: Term) = term
    override def serialized = s"constant ${term.serialized}"
    override def toString = term.toString
    override def safeToString = term.safeToString
  }
  case object Identity extends Function {
    override def apply(term: Term): Term = term
    override def serialized: String = "identity"
    override def toString = "_"
    override def safeToString = "_"
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Function] = {
    Parser.selectWordParser("function") {
      case "constant" => Term.parser.map(Constant.apply)
      case "identity" => Parser.constant(Identity)
    }
  }
}

