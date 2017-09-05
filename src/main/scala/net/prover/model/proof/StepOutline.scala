package net.prover.model.proof

import net.prover.model.{Parser, ParsingContext}
import net.prover.model.components.{Statement, Term, TermVariable}

sealed trait StepOutline

object StepOutline {
  sealed trait WithAssertion extends StepOutline {
    def innermostAssertionStep: StepOutline.Assertion
  }

  case class Location(fileName: String, lineNumber: Int)

  case class Assertion(
      assertion: Statement,
      location: Location,
      debug: Boolean = false)
    extends StepOutline.WithAssertion
  {
    override def innermostAssertionStep = this
  }
  object Assertion {
    def parser(implicit context: ParsingContext): Parser[Assertion] = Parser { tokenizer =>
      val innerParser = for {
        assertion <- Statement.parser
        debug <- Parser.optional("debug", Parser.constant(true), false)
      } yield Assertion(
        assertion,
        Location(tokenizer.currentFile, tokenizer.currentLine),
        debug)
      innerParser.parse(tokenizer)
    }
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[StepOutline])
    extends StepOutline
  object Assumption {
    def parser(implicit context: ParsingContext): Parser[Assumption] = {
      for {
        assumption <- Statement.parser
        steps <- listParser.inBraces
      } yield {
        Assumption(assumption, steps)
      }
    }
  }

  case class Naming(
      termVariable: TermVariable,
      statement: Statement,
      steps: Seq[StepOutline])
    extends StepOutline.WithAssertion
  {
    def innermostAssertionStep = steps.ofType[StepOutline.WithAssertion].lastOption
      .getOrElse(throw new Exception("Naming step must contain a step with an assertion"))
      .innermostAssertionStep
  }
  object Naming {
    def parser(implicit context: ParsingContext): Parser[Naming] = {
      for {
        termVariable <- Term.variableParser
        statement <- Statement.parser
        steps <- listParser.inBraces
      } yield {
        Naming(termVariable, statement, steps)
      }
    }
  }

  case class ScopedVariable(boundVariableName: String, steps: Seq[StepOutline]) extends StepOutline
  object ScopedVariable {
    def parser(implicit context: ParsingContext): Parser[ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = context.addBoundVariable(variableName)
        steps <- listParser(updatedContext).inBraces
      } yield ScopedVariable(variableName, steps)
    }
  }

  def parser(implicit context: ParsingContext): Parser[Option[StepOutline]] = {
    Parser.selectOptionalWord {
      case "assume" => Assumption.parser
      case "let" => Naming.parser
      case "prove" => Assertion.parser
      case "take" => ScopedVariable.parser
    }
  }
  def listParser(implicit context: ParsingContext): Parser[Seq[StepOutline]] = {
    parser.collectWhileDefined
  }
}
