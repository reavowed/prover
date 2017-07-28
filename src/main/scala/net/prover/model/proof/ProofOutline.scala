package net.prover.model.proof

import net.prover.model.components.{Statement, Term, TermVariable}
import net.prover.model.{Parser, ParsingContext}

case class ProofOutline(steps: Seq[ProofOutline.Step])

object ProofOutline {

  case class Location(fileName: String, lineNumber: Int)

  sealed trait Step

  sealed trait StepWithAssertion extends Step {
    def innermostAssertionStep: AssertionStep
  }

  case class AssumptionStep(
    assumption: Statement,
    steps: Seq[Step])
    extends Step

  case class NamingStep(
    termVariable: TermVariable,
    statement: Statement,
    steps: Seq[Step])
    extends StepWithAssertion {
    def innermostAssertionStep = steps.ofType[StepWithAssertion].lastOption
      .getOrElse(throw new Exception("Naming step must contain a step with an assertion"))
      .innermostAssertionStep
  }

  case class AssertionStep(
    assertion: Statement,
    location: Location,
    debug: Boolean = false)
    extends StepWithAssertion
  {
    override def innermostAssertionStep = this
  }

  private def assumptionStepParser(implicit context: ParsingContext): Parser[AssumptionStep] = {
    for {
      assumption <- Statement.parser
      steps <- stepsParser.inBraces
    } yield {
      AssumptionStep(assumption, steps)
    }
  }

  private def namingStepParser(implicit context: ParsingContext): Parser[NamingStep] = {
    for {
      termVariable <- Term.variableParser
      statement <- Statement.parser
      steps <- stepsParser.inBraces
    } yield {
      NamingStep(termVariable, statement, steps)
    }
  }

  private def assertionStepParser(implicit context: ParsingContext): Parser[AssertionStep] = Parser { tokenizer =>
    val innerParser = for {
      assertion <- Statement.parser
      debug <- Parser.optional("debug", Parser.constant(true), false)
    } yield AssertionStep(
      assertion,
      Location(tokenizer.currentFile, tokenizer.currentLine),
      debug)
    innerParser.parse(tokenizer)
  }

  private def stepParser(implicit context: ParsingContext): Parser[Option[Step]] = {
    Parser.singleWord.flatMap {
      case "assume" =>
        assumptionStepParser.map(Some.apply)
      case "let" =>
        namingStepParser.map(Some.apply)
      case "prove" =>
        assertionStepParser.map(Some.apply)
      case _ =>
        Parser.constant(None)
    }
  }

  private def stepsParser(implicit context: ParsingContext): Parser[Seq[Step]] = {
    stepParser.collectWhileDefined
  }

  def parser(implicit context: ParsingContext): Parser[ProofOutline] = {
    stepsParser.map(ProofOutline.apply)
  }
}
