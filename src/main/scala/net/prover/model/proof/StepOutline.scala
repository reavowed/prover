package net.prover.model.proof

import net.prover.model.{FileLocation, Parser, ParsingContext}
import net.prover.model.expressions.{Assertable, Statement, Term, TermVariable}

sealed trait StepOutline

object StepOutline {
  sealed trait WithAssertion extends StepOutline {
    def innermostAssertionStep: StepOutline.Assertion
  }

  case class Assertion(
      assertion: Assertable,
      location: Option[FileLocation],
      debug: Boolean = false)
    extends StepOutline.WithAssertion
  {
    override def innermostAssertionStep = this
  }
  object Assertion {
    def parser(implicit context: ParsingContext): Parser[Assertion] = Parser { tokenizer =>
      val innerParser = for {
        assertion <- Assertable.parser
        debug <- Parser.optional("debug", Parser.constant(true), false)
      } yield Assertion(
        assertion,
        Some(FileLocation(tokenizer.currentFile, tokenizer.currentLine)),
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
      definingAssumption: Assertable,
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
        definingAssumption <- Assertable.parser
        steps <- listParser.inBraces
      } yield {
        Naming(termVariable, definingAssumption, steps)
      }
    }
  }

  case class ScopedVariable(variableName: String, steps: Seq[StepOutline]) extends StepOutline
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
    Parser.selectOptionalWordParser {
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
