package net.prover.model.proof

import net.prover.model.expressions.Statement
import net.prover.model.{FileLocation, Parser, ParsingContext, ProvingException}

sealed trait StepOutline {
  def prove(reference: Reference.Direct)(implicit context: ProvingContext): Step
}

object StepOutline {
  sealed trait WithAssertion extends StepOutline {
    def innermostAssertionStep: StepOutline.Assertion
  }

  case class Assertion(
      assertion: Statement,
      location: Option[FileLocation])
    extends StepOutline.WithAssertion
  {
    override def innermostAssertionStep = this
    override def prove(reference: Reference.Direct)(implicit context: ProvingContext) = {
      tryProve(reference)(context)
        .getOrElse(throw ProvingException(s"Could not prove assertion '$assertion'", location))
    }
    def tryProve(reference: Reference.Direct)(implicit context: ProvingContext) = {
      findProofByHint(reference) orElse
        ProofFinder(assertion, reference).findProof()
          .ifDefined(location.foreach(x => Proof.logger.info(s"$x Proved statement $assertion")))
    }
    private def findProofByHint(reference: Reference.Direct)(implicit context: ProvingContext) = {
      context.assertionHints
          .filter(_.assertion == assertion)
          .mapFind(_.validate(context))
          .map(_.copy(reference = reference))
    }
  }
  object Assertion {
    def parser(implicit context: ParsingContext): Parser[Assertion] = {
      for {
        location <- Parser.location
        assertion <- Statement.parser
      } yield Assertion(
        assertion,
        Some(location))
    }
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[StepOutline],
      location: Option[FileLocation])
    extends StepOutline
  {
    override def prove(reference: Reference.Direct)(implicit context: ProvingContext) = {
      val contextWithAssumption = context.addProvenStatement(assumption, reference)
      val substeps = steps.prove(Some(reference))(contextWithAssumption)
      val deductionStatement = context.deductionStatement
        .getOrElse(throw ProvingException("Cannot prove a deduction without an appropriate statement definition", location))
      Step.Assumption(assumption, substeps, deductionStatement, reference)
    }
  }
  object Assumption {
    def parser(implicit context: ParsingContext): Parser[Assumption] = {
      for {
        location <- Parser.location
        assumption <- Statement.parser
        steps <- listParser.inBraces
      } yield {
        Assumption(assumption, steps, Some(location))
      }
    }
  }

  case class Naming(
      variableName: String,
      definingAssumption: Statement,
      steps: Seq[StepOutline],
      location: Option[FileLocation])
    extends StepOutline.WithAssertion
  {
    def innermostAssertionStep = steps.ofType[StepOutline.WithAssertion].lastOption
      .getOrElse(throw new Exception("Naming step must contain a step with an assertion"))
      .innermostAssertionStep
    override def prove(reference: Reference.Direct)(implicit context: ProvingContext) = {
      val assumptionStep = Assumption(definingAssumption, steps, None)
        .prove(reference.withSuffix(".0"))(context.increaseDepth(1, context.depth))
      val deduction = assumptionStep.provenStatements.lastOption.getOrElse(throw ProvingException(
        "Naming step did not have a conclusion",
        location))
      val scopedDeduction = context.scoped(deduction.statement, variableName)
        .getOrElse(throw ProvingException("Cannot prove a scoped statement without an appropriate statement definition", location))
      val innerAssertion = assumptionStep
        .steps.flatMap(_.provenStatements).lastOption
        .getOrElse(throw ProvingException(
          "Naming step did not have a conclusion",
          location))
      val outerAssertion = innerAssertion.statement.reduceDepth(1, context.depth)
        .getOrElse(throw ProvingException(
          s"Assertion $innerAssertion was not independent of $variableName",
          location))
      val assertionStep = Assertion(outerAssertion, None)
        .tryProve(
          reference.withSuffix(".1"))(
          context.addProvenStatement(
            scopedDeduction,
            reference.withSuffix("d")))
        .getOrElse(throw ProvingException(
          s"Could not extract assertion $innerAssertion from naming step for $variableName",
          location))
      Step.Naming(variableName, assumptionStep, assertionStep, reference)
    }
  }
  object Naming {
    def parser(implicit context: ParsingContext): Parser[Naming] = {
      for {
        location <- Parser.location
        variableName <- Parser.singleWord
        updatedContext = context.addParameterList(Seq(variableName))
        definingAssumption <- Statement.parser(updatedContext)
        steps <- listParser(updatedContext).inBraces
      } yield {
        Naming(variableName, definingAssumption, steps, Some(location))
      }
    }
  }

  case class ScopedVariable(
      variableName: String,
      steps: Seq[StepOutline],
      location: Option[FileLocation])
    extends StepOutline
  {
    override def prove(reference: Reference.Direct)(implicit context: ProvingContext) = {
      val provenSteps = steps.prove(Some(reference))(context.increaseDepth(1, context.depth))
      val scopingStatement = context.scopingStatement
        .getOrElse(throw ProvingException("Cannot prove a deduction without an appropriate statement definition", location))
      Step.ScopedVariable(variableName, provenSteps, scopingStatement, reference)
    }
  }
  object ScopedVariable {
    def parser(implicit context: ParsingContext): Parser[ScopedVariable] = {
      for {
        location <- Parser.location
        variableName <- Parser.singleWord
        updatedContext = context.addParameterList(Seq(variableName))
        steps <- listParser(updatedContext).inBraces
      } yield ScopedVariable(variableName, steps, Some(location))
    }
  }

  implicit class StepOutlineSeqOps(stepOutlines: Seq[StepOutline]) {
    def prove(baseReference: Option[Reference.Direct])(implicit context: ProvingContext): Seq[Step] = {
      stepOutlines.zipWithIndex.foldLeft(Seq.empty[Step]) { case (steps, (stepOutline, index)) =>
        val updatedContext = context.addProvenStatement(steps.flatMap(_.provenStatements))
        steps :+ stepOutline.prove(Reference.nextReference(baseReference, index.toString))(updatedContext)
      }
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
