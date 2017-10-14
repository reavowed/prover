package net.prover.model.proof

import net.prover.model.{FileLocation, Parser, ParsingContext}
import net.prover.model.expressions.{Statement, Term, TermVariable}

sealed trait StepOutline

object StepOutline {
  sealed trait WithAssertion extends StepOutline {
    def innermostAssertionStep: StepOutline.Assertion
  }

  case class Assertion(
      assertion: Statement,
      location: Option[FileLocation],
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
      variableName: String,
      definingAssumption: Statement,
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
        variableName <- Parser.singleWord
        updatedContext = context.addParameterList(Seq(variableName))
        definingAssumption <- Statement.parser(updatedContext)
        steps <- listParser(updatedContext).inBraces
      } yield {
        Naming(variableName, definingAssumption, steps)
      }
    }
  }

  case class ScopedVariable(variableName: String, steps: Seq[StepOutline]) extends StepOutline
  object ScopedVariable {
    def parser(implicit context: ParsingContext): Parser[ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = context.addParameterList(Seq(variableName))
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
