package net.prover.model.proof

import net.prover.model.components.{Statement, Term, TermVariable}
import net.prover.model.proof.Proof.Step
import net.prover.model.proof.ProofOutline.StepWithAssertion
import net.prover.model._
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Proof.Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  val conclusion: Statement = {
    steps.ofType[Proof.StepWithProvenStatement].lastOption
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
      .statement
  }
  def matchesOutline(outline: ProofOutline): Boolean = {
    Step.matchOutlines(steps, outline.steps)
  }
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
    steps.flatMap(_.getAssertionHints(availableInferences))
  }
  def serialized: String = steps.flatMap(_.serializedLines).mkString("\n")
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)

  sealed trait Step {
    def `type`: String
    def reference: Reference.Direct
    def fact: Option[Fact]
    def referencedFact: Option[ReferencedFact] = fact.map { f => ReferencedFact(f, reference)}
    def referencedInferenceIds: Set[String]
    def matchesOutline(stepOutline: ProofOutline.Step): Boolean
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
    def serialized: String = serializedLines.mkString("\n")
    def serializedLines: Seq[String]
  }
  object Step {
    def matchOutlines(steps: Seq[Step], stepOutlines: Seq[ProofOutline.Step]): Boolean = {
      steps.zipStrict(stepOutlines).exists(_.forall { case (step, stepOutline) =>
        step.matchesOutline(stepOutline)
      })
    }
    def matchAssertionOutline(provenStatement: Statement, stepOutline: ProofOutline.Step): Boolean = stepOutline match {
      case ProofOutline.AssertionStep(`provenStatement`, _, _) =>
        true
      case _ =>
        false
    }
  }
  sealed trait StepWithProvenStatement extends Step {
    def statement: Statement
    override def fact = Some(Fact.Direct(statement))
  }
  object StepWithProvenStatement {
    def unapply(step: Step): Option[Statement] = step match {
      case stepWithProvenStatement: StepWithProvenStatement =>
        Some(stepWithProvenStatement.statement)
      case _ =>
        None
    }
  }

  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step],
      reference: Reference.Direct)
    extends Step
  {
    override val `type` = "assumption"
    override val fact = steps.ofType[StepWithProvenStatement].lastOption.map(lastSubstep => Fact.Deduced(assumption, lastSubstep.statement))
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = stepOutline match {
      case ProofOutline.AssumptionStep(`assumption`, stepOutlines) =>
        Step.matchOutlines(steps, stepOutlines)
      case _ =>
        false
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      steps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def serializedLines = s"assume ${assumption.serialized} {" +: steps.flatMap(_.serializedLines).indent :+ "}"
  }
  case class AssertionStep(
      statement: Statement,
      inferenceApplication: InferenceApplication,
      reference: Reference.Direct)
    extends StepWithProvenStatement
  {
    override val `type` = "assertion"
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = Step.matchAssertionOutline(statement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      inferenceApplication.getAssertionHints(availableInferences)
    }
    override def serializedLines: Seq[String] = {
      s"assert ${statement.serialized}" +: inferenceApplication.serializedLines.indent
    }
  }
  case class RearrangementStep(
      statement: Statement,
      rearrangement: Reference.Rearrangement,
      reference: Reference.Direct)
    extends StepWithProvenStatement
  {
    override val `type` = "rearrange"
    override def referencedInferenceIds: Set[String] = rearrangement.referencedInferenceIds
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = Step.matchAssertionOutline(statement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      rearrangement.getAssertionHints(availableInferences)
    }
    override def serializedLines = {
      s"rearrange ${statement.serialized}" +: rearrangement.serializedLines.indent
    }
  }
  case class NamingStep(
      variable: TermVariable,
      assumptionStep: AssumptionStep,
      assertionStep: StepWithProvenStatement,
      reference: Reference.Direct)
    extends StepWithProvenStatement
  {
    override val `type` = "naming"
    override def statement: Statement = assertionStep.statement
    override def referencedInferenceIds: Set[String] = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = stepOutline match {
      case ProofOutline.NamingStep(`variable`, statement, stepOutlines) =>
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

  def getInitialContext(
    premises: Seq[Premise],
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint]
  ): ProvingContext = {
    ProvingContext(
      premises.map(_.referencedFact),
      premises,
      Nil,
      availableInferences,
      assertionHints)
  }

  def fillInOutline(
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint]
  ): Proof = {
    val context = getInitialContext(premises, availableInferences, assertionHints)
    val detailedSteps = proveSteps(
      proofOutline.steps,
      Nil,
      context,
      None)
    Proof(detailedSteps)
  }

  private def proveSteps(
    stepOutlines: Seq[ProofOutline.Step],
    accumulatedSteps: Seq[Step],
    context: ProvingContext,
    baseReference: Option[Reference.Direct]
  ): Seq[Step] = {
    stepOutlines.zipWithIndex.foldLeft(Seq.empty[Step]) { case (steps, (stepOutline, index)) =>
      val updatedContext = context.copy(referencedFacts = context.referencedFacts ++ steps.mapCollect(_.referencedFact))
      steps :+ proveStep(stepOutline, updatedContext, Reference.nextReference(baseReference, index))
    }
  }

  private def proveStep(
    stepOutline: ProofOutline.Step,
    context: ProvingContext,
    reference: Reference.Direct
  ): Step = {
    stepOutline match {
      case ProofOutline.AssumptionStep(assumption, substepOutlines) =>
        proveAssumptionStep(assumption, substepOutlines, context, reference)
      case ProofOutline.NamingStep(variable, namingStatement, substepOutlines) =>
        proveNamingStep(variable, namingStatement, substepOutlines, context, reference)
      case assertionStep: ProofOutline.AssertionStep =>
        proveAssertionStep(assertionStep, context, reference)
          .ifDefined {
            logger.info(s"Proved assertion ${assertionStep.assertion}")
          }
          .getOrElse(throw ProvingException(
            s"Could not prove assertion ${assertionStep.assertion}",
            assertionStep.location.fileName,
            assertionStep.location.lineNumber))
    }
  }

  private def proveAssumptionStep(
    assumption: Statement,
    substepOutlines: Seq[ProofOutline.Step],
    context: ProvingContext,
    reference: Reference.Direct
  ): AssumptionStep = {
    val contextWithAssumption = context.addFact(Fact.Direct(assumption), reference)
    val substeps = proveSteps(
      substepOutlines,
      Nil,
      contextWithAssumption,
      Some(reference))
    AssumptionStep(assumption, substeps, reference)
  }

  private def proveAssertionStep(
    stepOutline: ProofOutline.AssertionStep,
    context: ProvingContext,
    reference: Reference.Direct
  ):  Option[StepWithProvenStatement] = {
    Prover(
      stepOutline.assertion,
      reference,
      context,
      stepOutline.debug
    ).proveAssertion()
  }

  def proveNamingStep(
    variable: TermVariable,
    namingStatement: Statement,
    substepOutlines: Seq[ProofOutline.Step],
    context: ProvingContext,
    reference: Reference.Direct
  ): NamingStep = {
    val finalStepWithAssertion = substepOutlines match {
      case _ :+ (step: StepWithAssertion) =>
        step.innermostAssertionStep
      case _ =>
        throw new Exception("Naming step must end with an assertion")
    }
    val assumptionStep = proveAssumptionStep(namingStatement, substepOutlines, context, reference)
    val assertionStep = proveAssertionStep(
      finalStepWithAssertion,
      context.addFact(assumptionStep.referencedFact),
      reference
    ).getOrElse(
      throw ProvingException(
        s"Could not extract assertion ${finalStepWithAssertion.assertion} from naming step for $variable",
        finalStepWithAssertion.location.fileName,
        finalStepWithAssertion.location.lineNumber))
    NamingStep(variable, assumptionStep, assertionStep, reference)
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Proof] = {
    for {
      steps <- stepsParser(None)
    } yield {
      Proof(steps)
    }
  }

  def stepsParser(baseReference: Option[Reference.Direct])(implicit parsingContext: ParsingContext): Parser[Seq[Step]] = {
    Parser.iterateWhileDefined((Seq.empty[Step], 0)) { case (steps, index) =>
      stepParser(Reference.nextReference(baseReference, index)).mapMap {step =>
        (steps :+ step, index + 1)
      }
    }.map(_._1)
  }

  def stepParser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Option[Step]] = {
    Parser.selectWord {
      case "assume" => assumptionStepParser(reference)
      case "assert" => assertionStepParser(reference)
      case "rearrange" => rearrangementStepParser(reference)
      case "name" => namingStepParser(reference)
    }
  }

  def assumptionStepParser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[AssumptionStep] = {
    for {
      assumption <- Statement.parser
      steps <- stepsParser(Some(reference)).inBraces
    } yield AssumptionStep(assumption, steps, reference)
  }

  def assertionStepParser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[AssertionStep] = {
    for {
      assertion <- Statement.parser
      inferenceApplication <- InferenceApplication.parser
    } yield {
      AssertionStep(assertion, inferenceApplication, reference)
    }
  }

  def rearrangementStepParser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[RearrangementStep] = {
    for {
      provenStatement <- Statement.parser
      rearrangement <- Reference.parser.getOrElse(throw new Exception("Rearrangement step missing reference"))
        .map { r =>
          r.asOptionalInstanceOf[Reference.Rearrangement]
          .getOrElse(throw new Exception("Rerrangment step had non-rearrangement reference"))
        }
    } yield RearrangementStep(provenStatement, rearrangement, reference)
  }

  def namingStepParser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[NamingStep] = {
    for {
      variable <- Term.variableParser
      assumptionStep <- assumptionStepParser(reference)
      _ <- Parser.requiredWord("assert")
      assertionStep <- assertionStepParser(reference)
    } yield NamingStep(variable, assumptionStep, assertionStep, reference)
  }
}
