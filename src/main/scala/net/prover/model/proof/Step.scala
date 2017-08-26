package net.prover.model.proof

import net.prover.model._
import net.prover.model.components.{Statement, Term, TermVariable}

sealed trait Step {
  def `type`: String
  def reference: Reference.Direct
  def fact: Option[Fact]
  def referencedFact: Option[ReferencedFact] = fact.map { f => ReferencedFact(f, reference)}
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def matchesOutline(stepOutline: StepOutline): Boolean
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
  def serialized: String = serializedLines.mkString("\n")
  def serializedLines: Seq[String]
}

object Step {

  sealed trait WithProvenStatement extends Step {
    def statement: Statement
    override def fact = Some(Fact.Direct(statement))
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[Step],
      reference: Reference.Direct)
    extends Step
  {
    def assumptionReference = reference.withSuffix("a")
    override val `type` = "assumption"
    override val fact = steps.ofType[Step.WithProvenStatement].lastOption.map(lastSubstep => Fact.Deduced(assumption, lastSubstep.statement))
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Assumption(`assumption`, stepOutlines) =>
        Step.matchOutlines(steps, stepOutlines)
      case _ =>
        false
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      steps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def serializedLines = s"assume ${assumption.serialized} {" +: steps.flatMap(_.serializedLines).indent :+ "}"
  }
  object Assumption {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.Assumption] = {
      for {
        assumption <- Statement.parser
        steps <- Step.listParser(Some(reference)).inBraces
      } yield Step.Assumption(assumption, steps, reference)
    }
  }

  case class Assertion(
      statement: Statement,
      inferenceApplication: InferenceApplication,
      reference: Reference.Direct,
      isRearrangement: Boolean)
    extends Step.WithProvenStatement
  {
    override val `type` = "assertion"
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
    override def referenceMap: ReferenceMap = ReferenceMap(reference.value -> inferenceApplication.directReferences)
    override def matchesOutline(stepOutline: StepOutline): Boolean = Step.matchAssertionOutline(statement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      inferenceApplication.getAssertionHints(availableInferences)
    }
    override def serializedLines: Seq[String] = {
      s"assert ${if (isRearrangement) "rearranging " else ""}${statement.serialized}" +: inferenceApplication.serializedLines.indent
    }
  }
  object Assertion {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.Assertion] = {
      for {
        isRearrangement <- Parser.optionalWord("rearranging").isDefined
        assertion <- Statement.parser
        inferenceApplication <- InferenceApplication.parser
      } yield {
        Step.Assertion(assertion, inferenceApplication, reference, isRearrangement)
      }
    }
  }

  case class Naming(
      variable: TermVariable,
      assumptionStep: Step.Assumption,
      assertionStep: Step.WithProvenStatement,
      reference: Reference.Direct)
    extends Step.WithProvenStatement
  {
    override val `type` = "naming"
    override def statement: Statement = assertionStep.statement
    override def referencedInferenceIds: Set[String] = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def referenceMap: ReferenceMap = assertionStep.referenceMap
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Naming(`variable`, statement, stepOutlines) =>
        assumptionStep.assumption == statement && Step.matchOutlines(assumptionStep.steps, stepOutlines)
      case _ =>
        false
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      assumptionStep.getAssertionHints(availableInferences) ++ assertionStep.getAssertionHints(availableInferences)
    }
    override def serializedLines = {
      Seq(s"name ${variable.text} ${assumptionStep.assumption.serialized} {") ++
        assumptionStep.steps.flatMap(_.serializedLines).indent ++
        Seq("}") ++
        assertionStep.serializedLines
    }
  }
  object Naming {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.Naming] = {
      for {
        variable <- Term.variableParser
        assumptionStep <- Assumption.parser(reference)
        _ <- Parser.requiredWord("assert")
        assertionStep <- Assertion.parser(reference)
      } yield Step.Naming(variable, assumptionStep, assertionStep, reference)
    }
  }

  def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Option[Step]] = {
    Parser.selectWord {
      case "assume" => Assumption.parser(reference)
      case "assert" => Assertion.parser(reference)
      case "name" => Naming.parser(reference)
    }
  }
  def listParser(baseReference: Option[Reference.Direct])(implicit parsingContext: ParsingContext): Parser[Seq[Step]] = {
    Parser.iterateWhileDefined((Seq.empty[Step], 0)) { case (steps, index) =>
      parser(Reference.nextReference(baseReference, index.toString)).mapMap {step =>
        (steps :+ step, index + 1)
      }
    }.map(_._1)
  }

  def matchOutlines(steps: Seq[Step], stepOutlines: Seq[StepOutline]): Boolean = {
    steps.zipStrict(stepOutlines).exists(_.forall { case (step, stepOutline) =>
      step.matchesOutline(stepOutline)
    })
  }
  def matchAssertionOutline(provenStatement: Statement, stepOutline: StepOutline): Boolean = stepOutline match {
    case StepOutline.Assertion(`provenStatement`, _, _) =>
      true
    case _ =>
      false
  }
}
