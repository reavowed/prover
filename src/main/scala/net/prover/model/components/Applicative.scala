package net.prover.model.components

import net.prover.model.entries.StatementDefinition
import net.prover.model.{Parser, ParsingContext}

sealed trait Applicative[+T <: Component] {
  def apply(term: Term): T
  def isConstant: Boolean
  def serialized: String
}

sealed trait Predicate extends Applicative[Statement]

object Predicate {
  case class Constant(statement: Statement) extends Predicate {
    override def apply(term: Term): Statement = statement
    override def isConstant: Boolean = true
    override def serialized: String = s"constant ${statement.serialized}"
  }
  case class Defined(components: Seq[Applicative[Component]], definition: StatementDefinition) extends Predicate {
    override def apply(term: Term): Statement = definition(components.map(_(term)): _*)
    override def isConstant: Boolean = components.forall(_.isConstant)
    override def serialized: String = s"defined ${definition.symbol} ${components.map(_.serialized).mkString(" ")}"
  }
  object Defined {
    def parser(implicit context: ParsingContext): Parser[Defined] = {
      Parser.selectWord("statement type") {
        case context.RecognisedStatementDefinition(statementDefinition) =>
          statementDefinition.componentTypes.applicativesParser.map(Defined(_, statementDefinition))
      }
    }
  }
  case class Named(name: String) extends Predicate {
    override def apply(term: Term): Statement = PredicateApplication(name, term)
    override def isConstant: Boolean = false
    override def serialized: String = s"named $name"
  }
  object Named {
    def parser(implicit context: ParsingContext): Parser[Named] = {
      Parser.singleWord.map(Named.apply)
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Predicate] = {
    Parser.selectWord("predicate") {
      case "constant" => Statement.parser.map(Constant.apply)
      case "defined" => Defined.parser
      case "named" => Named.parser
    }
  }
}

sealed trait Function extends Applicative[Term]

object Function {
  case class Constant(term: Term) extends Function {
    override def apply(otherTerm: Term): Term = term
    override def isConstant: Boolean = true
    override def serialized: String = s"constant ${term.serialized}"
  }
  case object Identity extends Function {
    override def apply(term: Term): Term = term
    override def isConstant: Boolean = false
    override def serialized: String = "identity"
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Function] = {
    Parser.selectWord("function") {
      case "constant" => Term.parser.map(Constant.apply)
      case "identity" => Parser.constant(Identity)
    }
  }
}

