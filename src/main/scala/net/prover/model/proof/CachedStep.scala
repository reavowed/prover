package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement}

sealed trait CachedStep {
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[CachedStep.Assertion]
  def validate(context: ProvingContext): Option[Step]
  def matchesOutline(stepOutline: StepOutline): Boolean
  def serializedLines: Seq[String]
  def serialized: String = serializedLines.mkString("\n")
}

object CachedStep {
  case class Assertion(
      assertion: Statement,
      cachedInferenceApplication: CachedInferenceApplication,
      reference: Reference.Direct,
      isRearrangement: Boolean)
    extends CachedStep
  {
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[Assertion] = {
      Seq(this)
    }
    def validate(context: ProvingContext): Option[Step.Assertion] = {
      for {
        (conclusion, inferenceApplication) <- cachedInferenceApplication.validate()(context)
        _ = if (conclusion != assertion)
          CachedProof.logger.info(s"Inference conclusion '${conclusion.serialized}' was not '${assertion.serialized}'")
        if conclusion == assertion
      } yield Step.Assertion(assertion, inferenceApplication, reference, isRearrangement)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Assertion(`assertion`, _) =>
        true
      case _ =>
        false
    }
    override def serializedLines: Seq[String] = {
      Seq(s"assert ${if (isRearrangement) "rearranging " else ""}${assertion.serialized}") ++
        cachedInferenceApplication.serializedLines.indent
    }
  }
  object Assertion {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Assertion] = {
      for {
        isRearrangement <- Parser.optionalWord("rearranging").isDefined
        assertion <- Statement.parser
        cachedInferenceApplication <- CachedInferenceApplication.parser
      } yield {
        Assertion(assertion, cachedInferenceApplication, reference, isRearrangement)
      }
    }
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[CachedStep],
      reference: Reference.Direct)
    extends CachedStep
  {
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[Assertion] = {
      steps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def validate(context: ProvingContext): Option[Step.Assumption] = {
      val assumptionContext = context.addProvenStatement(assumption, reference)
      for {
        deductionStatement <- context.deductionStatement
        validatedSubsteps <- steps.validate(assumptionContext)
      } yield Step.Assumption(assumption, validatedSubsteps, deductionStatement, reference)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Assumption(`assumption`, stepOutlines, _) =>
        steps.matchOutlines(stepOutlines)
      case _ =>
        false
    }
    override def serializedLines = s"assume ${assumption.serialized} {" +: steps.flatMap(_.serializedLines).indent :+ "}"
  }
  object Assumption {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Assumption] = {
      for {
        assumption <- Statement.parser
        steps <- listParser(Some(reference)).inBraces
      } yield Assumption(assumption, steps, reference)
    }
  }

  case class Naming(
      variableName: String,
      assumptionStep: Assumption,
      assertionStep: Assertion,
      reference: Reference.Direct)
    extends CachedStep
  {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[Assertion] = {
      assumptionStep.getAssertionHints(availableInferences) ++ assertionStep.getAssertionHints(availableInferences)
    }
    override def validate(context: ProvingContext): Option[Step.Naming] = {
      val innerContext = context.increaseDepth(1, context.depth)
      for {
        validatedAssumptionStep <- assumptionStep.validate(innerContext)
        deduction <- validatedAssumptionStep.provenStatements.lastOption
        scopingStatement <- context.scopingStatement
        validatedAssertionStep <- assertionStep.validate(
          context.addProvenStatement(
            DefinedStatement(Seq(deduction.statement), scopingStatement, deduction.statement.depth - 1)(Seq(variableName)),
            reference.withSuffix("d")))
      } yield Step.Naming(variableName, validatedAssumptionStep, validatedAssertionStep.asInstanceOf[Step.Assertion], reference)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Naming(`variableName`, statement, stepOutlines, _) =>
        assumptionStep.assumption == statement && assumptionStep.steps.matchOutlines(stepOutlines)
      case _ =>
        false
    }
    override def serializedLines = {
      Seq(s"name $variableName ${assumptionStep.assumption.serialized} {") ++
        assumptionStep.steps.flatMap(_.serializedLines).indent ++
        Seq("}") ++
        assertionStep.serializedLines
    }
  }
  object Naming {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addParameterList(Seq(variableName))
        assumptionStep <- Assumption.parser(reference.withSuffix(".0"))(updatedContext)
        _ <- Parser.requiredWord("assert")
        assertionStep <- Assertion.parser(reference.withSuffix(".1"))
      } yield Naming(variableName, assumptionStep, assertionStep, reference)
    }
  }

  case class ScopedVariable(
      variableName: String,
      substeps: Seq[CachedStep],
      reference: Reference.Direct)
    extends CachedStep
  {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[Assertion] = {
      substeps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def validate(context: ProvingContext): Option[Step] = {
      for {
        scopingStatment <- context.scopingStatement
        validatedSubsteps <- substeps.validate(context.increaseDepth(1, context.depth))
      } yield Step.ScopedVariable(variableName, validatedSubsteps, scopingStatment, reference)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.ScopedVariable(`variableName`, substepOutlines, _) =>
        substeps.matchOutlines(substepOutlines)
      case _ =>
        false
    }
    def serializedLines: Seq[String] = Seq(s"take $variableName {") ++ substeps.flatMap(_.serializedLines).indent ++ Seq("}")
  }

  object ScopedVariable {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addParameterList(Seq(variableName))
        steps <- listParser(Some(reference))(updatedContext).inBraces
      } yield ScopedVariable(variableName, steps, reference)
    }
  }


  def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Option[CachedStep]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Assumption.parser(reference)
      case "assert" => Assertion.parser(reference)
      case "name" => Naming.parser(reference)
      case "take" => ScopedVariable.parser(reference)
    }
  }
  def listParser(baseReference: Option[Reference.Direct])(implicit parsingContext: ParsingContext): Parser[Seq[CachedStep]] = {
    Parser.iterateWhileDefined((Seq.empty[CachedStep], 0)) { case (steps, index) =>
      parser(Reference.nextReference(baseReference, index.toString)).mapMap {step =>
        (steps :+ step, index + 1)
      }
    }.map(_._1)
  }

  implicit class CachedStepSeqOps(steps: Seq[CachedStep]) {
    def validate(context: ProvingContext): Option[Seq[Step]] = {
      def helper(
        stepsToValidate: Seq[CachedStep],
        validatedSteps: Seq[Step],
        currentContext: ProvingContext
      ): Option[(Seq[Step], ProvingContext)] = {
        stepsToValidate match {
          case Nil =>
            Some((validatedSteps, currentContext))
          case stepToValidate +: otherStepsToValidate =>
            for {
              validatedStep <- stepToValidate.validate(currentContext)
              result <- helper(
                otherStepsToValidate,
                validatedSteps :+ validatedStep,
                currentContext.addProvenStatement(validatedStep.provenStatements))
            } yield result
        }
      }
      helper(steps, Nil, context).map(_._1)
    }

    def matchOutlines(stepOutlines: Seq[StepOutline]): Boolean = {
      steps.zipStrict(stepOutlines).exists(_.forall { case (step, stepOutline) =>
        step.matchesOutline(stepOutline)
      })
    }
  }
}
