package net.prover.model.proof

import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model._

sealed trait CachedStep {
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
  def validate(context: ProvingContext): Option[Step]
  def matchesOutline(stepOutline: StepOutline): Boolean
  def serializedLines: Seq[String]
  def serialized: String = serializedLines.mkString("\n")
}

object CachedStep {
  case class Assertion(
      statement: Statement,
      cachedInferenceApplication: CachedInferenceApplication,
      reference: Reference.Direct,
      isRearrangement: Boolean)
    extends CachedStep
  {
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }
    def validate(context: ProvingContext): Option[Step.Assertion] = {
      for {
        (conclusion, inferenceApplication) <- cachedInferenceApplication.validate(context)
        _ = if (conclusion != statement)
          CachedProof.logger.info(s"Inference conclusion '${conclusion.serialized}' was not '${statement.serialized}'")
        if conclusion == statement
      } yield Step.Assertion(statement, inferenceApplication, reference, isRearrangement)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Assertion(`statement`, _, _) =>
        true
      case _ =>
        false
    }
    override def serializedLines: Seq[String] = {
      Seq(s"assert ${if (isRearrangement) "rearranging " else ""}${statement.serialized}") ++
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
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      steps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def validate(context: ProvingContext): Option[Step.Assumption] = {
      val assumptionContext = context.addFact(Fact.Direct(assumption), reference.withSuffix("a"))
      for {
        validatedSubsteps <- steps.validate(assumptionContext)
      } yield Step.Assumption(assumption, validatedSubsteps, reference)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Assumption(`assumption`, stepOutlines) =>
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
      variable: TermVariable,
      assumptionStep: Assumption,
      assertionStep: Assertion,
      reference: Reference.Direct)
    extends CachedStep
  {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      assumptionStep.getAssertionHints(availableInferences) ++ assertionStep.getAssertionHints(availableInferences)
    }
    override def validate(context: ProvingContext): Option[Step.Naming] = {
      for {
        validatedAssumptionStep <- assumptionStep.validate(context)
        deduction = validatedAssumptionStep.referencedFact.getOrElse(throw new Exception("Naming step assumption must prove a fact"))
        validatedAssertionStep <- assertionStep.validate(
          context.addFact(deduction.fact, deduction.reference.asInstanceOf[Reference.Direct].withSuffix("d")))
      } yield Step.Naming(variable, validatedAssumptionStep, validatedAssertionStep.asInstanceOf[Step.Assertion], reference)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Naming(`variable`, statement, stepOutlines) =>
        assumptionStep.assumption == statement &&assumptionStep.steps.matchOutlines(stepOutlines)
      case _ =>
        false
    }
    override def serializedLines = {
      Seq(s"name ${variable.text} ${assumptionStep.assumption.serialized} {") ++
        assumptionStep.steps.flatMap(_.serializedLines).indent ++
        Seq("}") ++
        assertionStep.serializedLines
    }
  }
  object Naming {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Naming] = {
      for {
        variable <- Term.variableParser
        assumptionStep <- Assumption.parser(reference)
        _ <- Parser.requiredWord("assert")
        assertionStep <- Assertion.parser(reference)
      } yield Naming(variable, assumptionStep, assertionStep, reference)
    }
  }

  case class ScopedVariable(
      variableName: String,
      substeps: Seq[CachedStep],
      reference: Reference.Direct)
    extends CachedStep
  {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      substeps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def validate(context: ProvingContext): Option[Step] = {
      for {
        validatedSubsteps <- substeps.validate(context)
      } yield Step.ScopedVariable(variableName, validatedSubsteps, reference)
    }
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.ScopedVariable(`variableName`, substepOutlines) =>
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
        updatedContext = parsingContext.addBoundVariable(variableName)
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
                currentContext.addFact(validatedStep.referencedFact))
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
