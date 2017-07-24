package net.prover.model.proof

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, Summary}
import net.prover.model.components.{Statement, Term, TermVariable}
import net.prover.model.proof.Proof.Step
import net.prover.model.proof.ProofOutline.StepWithAssertion
import net.prover.model.{Inference, InferenceTransform, Parser, ParsingContext, ProvenStatement, ProvingException, Substitutions}
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Proof.Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  val conclusion: ProvenStatement = {
    steps.ofType[Proof.StepWithProvenStatement].lastOption
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
      .provenStatement
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

  implicit class SeqStringOps(seq: Seq[String]) {
    def indent: Seq[String] = seq.map("  " + _)
  }

  sealed trait Step {
    def `type`: String
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
    def matchAssertionOutline(provenStatement: ProvenStatement, stepOutline: ProofOutline.Step): Boolean = stepOutline match {
      case ProofOutline.AssertionStep(provenStatement.statement, nonArbitraryVariables, nonDistinctVariables, _, _) =>
        provenStatement.conditions.arbitraryVariables.intersect(nonArbitraryVariables).isEmpty &&
          nonDistinctVariables.forall { case (termVariable, variable) =>
            !provenStatement.conditions.distinctVariables.areDistinct(termVariable, variable)
          }
      case _ =>
        false
    }
  }

  sealed trait StepWithProvenStatement extends Step {
    def provenStatement: ProvenStatement
  }
  object StepWithProvenStatement {
    def unapply(step: Step): Option[ProvenStatement] = step match {
      case stepWithProvenStatement: StepWithProvenStatement =>
        Some(stepWithProvenStatement.provenStatement)
      case _ =>
        None
    }
  }
  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step])
    extends Step
  {
    override val `type` = "assumption"
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
  case class NamingStep(
      variable: TermVariable,
      assumptionStep: AssumptionStep,
      assertionStep: StepWithProvenStatement)
    extends StepWithProvenStatement
  {
    override val `type` = "naming"
    override def provenStatement: ProvenStatement = assertionStep.provenStatement
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
  case class AssertionStep(
      provenStatement: ProvenStatement,
      inference: Inference.Summary,
      substitutions: Substitutions,
      references: Seq[Reference])
    extends StepWithProvenStatement
  {
    override val `type` = "assertion"
    override def referencedInferenceIds: Set[String] = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = Step.matchAssertionOutline(provenStatement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      AssertionHint.attempt(inference, availableInferences, substitutions, provenStatement).toSeq ++
        references.flatMap(_.getAssertionHints(availableInferences))
    }
    override def serializedLines: Seq[String] = {
      s"assert ${provenStatement.serialized}" +:
        (Seq(inference.serialized, substitutions.serialized) ++ references.flatMap(_.serializedLines)).indent
    }
  }
  case class RearrangementStep(
      provenStatement: ProvenStatement,
      reference: Reference)
    extends StepWithProvenStatement
  {
    override val `type` = "rearrange"
    override def referencedInferenceIds: Set[String] = reference.referencedInferenceIds
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = Step.matchAssertionOutline(provenStatement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      reference.getAssertionHints(availableInferences)
    }
    override def serializedLines = {
      s"rearrange ${provenStatement.serialized}" +: reference.serializedLines.indent
    }
  }
  case class TransformedInferenceStep(
      provenStatement: ProvenStatement,
      inference: Inference.Summary,
      premises: Seq[DirectPremise],
      transformationSteps: Seq[AssertionStep],
      substitutions: Substitutions,
      references: Seq[Reference])
    extends StepWithProvenStatement
  {
    override val `type` = "assertion"
    override def referencedInferenceIds: Set[String] = transformationSteps.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def matchesOutline(stepOutline: ProofOutline.Step): Boolean = Step.matchAssertionOutline(provenStatement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      AssertionHint.attempt(inference, availableInferences, substitutions, provenStatement).toSeq ++
        references.flatMap(_.getAssertionHints(availableInferences))
    }
    override def serializedLines: Seq[String] = {
      s"proveWithTransform ${provenStatement.serialized}" +:
        (Seq(inference.serialized) ++
          premises.map(_.serialized) ++
          transformationSteps.flatMap(_.serializedLines) ++
          Seq("qed", substitutions.serialized) ++
          references.flatMap(_.serializedLines)
        ).indent
    }
  }

  case class Rearrangement(
    inference: Inference.Summary,
    substitutions: Substitutions,
    provenStatement: ProvenStatement,
    references: Seq[Reference])

  sealed trait Reference {
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
    def referenceType: String
    def referencedInferenceIds: Set[String]
    def serialized: String = serializedLines.mkString("\n")
    def serializedLines: Seq[String]
  }
  case class DirectReference(index: Int) extends Reference {
    val referenceType = "direct"
    def referencedInferenceIds: Set[String] = Set.empty
    def serializedLines = Seq(s"direct $index")
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = Nil
  }
  case class DeducedReference(antecedentIndex: Int, consequentIndex: Int) extends Reference {
    val referenceType = "deduced"
    def referencedInferenceIds: Set[String] = Set.empty
    def serializedLines = Seq(s"deduced $antecedentIndex $consequentIndex")
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = Nil
  }
  case class SimplificationReference(
    statement: Statement,
    inference: Summary,
    substitutions: Substitutions,
    reference: Reference
  ) extends Reference {
    val referenceType = "simplification"
    def referencedInferenceIds: Set[String] = reference.referencedInferenceIds + inference.id
    def serializedLines = {
      s"simplification ${statement.serialized}" +: (Seq(inference.serialized, substitutions.serialized) ++ reference.serializedLines).indent
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      AssertionHint.attempt(inference, availableInferences, substitutions).toSeq ++ reference.getAssertionHints(availableInferences)
    }
  }
  case class ElidedReference(inference: Summary, substitutions: Substitutions, references: Seq[Reference]) extends Reference {
    val referenceType = "elided"
    def referencedInferenceIds: Set[String] = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    def serializedLines = {
      "elided {" +: (Seq(inference.serialized, substitutions.serialized) ++ references.flatMap(_.serializedLines)).indent :+ "}"
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      AssertionHint.attempt(inference, availableInferences, substitutions).toSeq ++
        references.flatMap(_.getAssertionHints(availableInferences))
    }
  }
  case class ExpandedReference(inference: Summary, substitutions: Substitutions, references: Seq[Reference]) extends Reference {
    val referenceType = "expanded"
    def referencedInferenceIds: Set[String] = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    def serializedLines = {
      "expanded {" +: (Seq(inference.serialized, substitutions.serialized) ++ references.flatMap(_.serializedLines)).indent :+ "}"
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      AssertionHint.attempt(inference, availableInferences, substitutions).toSeq ++
        references.flatMap(_.getAssertionHints(availableInferences))
    }
  }

  case class ReferencedAssertion(provenStatement: ProvenStatement, reference: Reference)
  case class ReferencedDeduction(assumption: Statement, deduction: ProvenStatement, reference: Reference)

  def getInitialContext(
    premises: Seq[Premise],
    availableInferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    assertionHints: Seq[AssertionHint]
  ): ProvingContext = {
    val premiseAssertions = premises.zipWithIndex.collect {
      case (DirectPremise(premise), index) =>
        ReferencedAssertion(ProvenStatement.withNoConditions(premise), DirectReference(index))
    }
    val premiseDeductions = premises.zipWithIndex.collect {
      case (DeducedPremise(assumption, conclusion), index) =>
        ReferencedDeduction(assumption, ProvenStatement.withNoConditions(conclusion), DirectReference(index))
    }
    ProvingContext(
      premiseAssertions,
      premiseDeductions,
      premises,
      Nil,
      availableInferences,
      inferenceTransforms,
      assertionHints)
  }

  def fillInOutline(
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    availableInferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    assertionHints: Seq[AssertionHint]
  ): Proof = {
    val context = getInitialContext(premises, availableInferences, inferenceTransforms, assertionHints)
    val (detailedSteps, _) = proveSteps(
      proofOutline.steps,
      Nil,
      context,
      premises.length)
    Proof(detailedSteps)
  }

  private def proveSteps(
    stepOutlines: Seq[ProofOutline.Step],
    accumulatedSteps: Seq[Step],
    context: ProvingContext,
    nextReference: Int
  ): (Seq[Step], ProvingContext) = {
    stepOutlines match {
      case Nil =>
        (accumulatedSteps, context)
      case stepOutline +: otherStepOutlines =>
        val (step, updatedContext) = proveStep(stepOutline, context, nextReference)
        proveSteps(
          otherStepOutlines,
          accumulatedSteps :+ step,
          updatedContext,
          nextReference + 1)
    }
  }

  private def proveStep(
    stepOutline: ProofOutline.Step,
    context: ProvingContext,
    nextReference: Int
  ): (Step, ProvingContext) = {
    stepOutline match {
      case ProofOutline.AssumptionStep(assumption, substepOutlines) =>
        proveAssumptionStep(assumption, substepOutlines, context, nextReference)
      case ProofOutline.NamingStep(variable, namingStatement, substepOutlines) =>
        val finalStepWithAssertion = substepOutlines match {
          case _ :+ (step: StepWithAssertion) =>
            step.innermostAssertionStep
          case _ =>
            throw new Exception("Naming step must end with an assertion")
        }
        val (assumptionStep, assumptionContext) = proveAssumptionStep(namingStatement, substepOutlines, context, nextReference)
        val (assertionStep, updatedContext) = proveAssertionStep(
          finalStepWithAssertion,
          assumptionContext,
          nextReference
        ).getOrElse(
          throw ProvingException(
            s"Could not extract assertion ${finalStepWithAssertion.assertion} from naming step for $variable",
            finalStepWithAssertion.location.fileName,
            finalStepWithAssertion.location.lineNumber))
        (NamingStep(variable, assumptionStep, assertionStep), updatedContext)
      case assertionStep: ProofOutline.AssertionStep =>
        proveAssertionStep(assertionStep, context, nextReference)
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
    nextReference: Int
  ): (AssumptionStep, ProvingContext) = {
    val contextWithAssumption = context.addAssumption(assumption, nextReference)
    val (substeps, _) = proveSteps(
      substepOutlines,
      Nil,
      contextWithAssumption,
      nextReference + 1)
    val assumptionStep = AssumptionStep(assumption, substeps)
    val newProvenDeductions = assumptionStep.steps.zipWithIndex.collect {
      case (StepWithProvenStatement(deduction), index) =>
        ReferencedDeduction(assumption, deduction, DeducedReference(nextReference, nextReference + index + 1))
    }
    (assumptionStep, context.add(newProvenDeductions))
  }

  private def proveAssertionStep(
    stepOutline: ProofOutline.AssertionStep,
    context: ProvingContext,
    nextReference: Int
  ):  Option[(StepWithProvenStatement, ProvingContext)] = {
    for {
      assertionStep <- Prover(
        stepOutline.assertion,
        stepOutline.nonArbitraryVariables,
        stepOutline.nonDistinctVariables,
        context,
        stepOutline.debug
      ).proveAssertion()
    } yield {
      (assertionStep, context.addAssertion(assertionStep.provenStatement, nextReference))
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Proof] = {
    for {
      steps <- stepsParser
    } yield {
      Proof(steps)
    }
  }

  def stepsParser(implicit parsingContext: ParsingContext): Parser[Seq[Step]] = {
    stepParser.collectWhileDefined
  }

  def stepParser(implicit parsingContext: ParsingContext): Parser[Option[Step]] = {
    Parser.selectWord {
      case "assume" => assumptionStepParser
      case "assert" => assertionStepParser
      case "proveWithTransform" => transformedInferenceStepParser
      case "rearrange" => rearrangementStepParser
      case "name" => namingStepParser
    }
  }

  def assumptionStepParser(implicit parsingContext: ParsingContext): Parser[AssumptionStep] = {
    for {
      assumption <- Statement.parser
      steps <- stepParser.collectWhileDefined.inBraces
    } yield AssumptionStep(assumption, steps)
  }

  def assertionStepParser(implicit parsingContext: ParsingContext): Parser[AssertionStep] = {
    for {
      assertion <- ProvenStatement.parser
      inferenceSummary <- Inference.Summary.parser
      substitutions <- Substitutions.parser
      references <- referencesParser
    } yield {
      AssertionStep(assertion, inferenceSummary, substitutions, references)
    }
  }

  def transformedInferenceStepParser(implicit parsingContext: ParsingContext): Parser[TransformedInferenceStep] = {
    for {
      provenStatement <- ProvenStatement.parser
      inferenceSummary <- Inference.Summary.parser
      premises <- Inference.premisesParser.map(_.ofType[DirectPremise])
      transformationSteps <- stepsParser.map(_.ofType[AssertionStep])
      _ <- Parser.requiredWord("qed")
      substitutions <- Substitutions.parser
      references <- referencesParser
    } yield TransformedInferenceStep(provenStatement, inferenceSummary, premises, transformationSteps, substitutions, references)
  }

  def rearrangementStepParser(implicit parsingContext: ParsingContext): Parser[RearrangementStep] = {
    for {
      provenStatement <- ProvenStatement.parser
      reference <- referenceParser.getOrElse(throw new Exception("Rearrangement step missing reference"))
    } yield RearrangementStep(provenStatement, reference)
  }

  def namingStepParser(implicit parsingContext: ParsingContext): Parser[NamingStep] = {
    for {
      variable <- Term.variableParser
      assumptionStep <- assumptionStepParser
      _ <- Parser.requiredWord("assert")
      assertionStep <- assertionStepParser
    } yield NamingStep(variable, assumptionStep, assertionStep)
  }

  def referencesParser(implicit parsingContext: ParsingContext): Parser[Seq[Reference]] = {
    referenceParser.collectWhileDefined
  }
  def referenceParser(implicit parsingContext: ParsingContext): Parser[Option[Reference]] = {
    Parser.selectWord {
      case "direct" => directReferenceParser
      case "deduced" => deducedReferenceParser
      case "simplification" => simplificationReferenceParser
      case "elided" => elidedReferenceParser
      case "expanded" => expandedReferenceParser
    }
  }
  def directReferenceParser: Parser[DirectReference] = {
    for {
      index <- Parser.int
    } yield {
      DirectReference(index)
    }
  }
  def deducedReferenceParser: Parser[DeducedReference] = {
    for {
      antecedentIndex <- Parser.int
      consequentIndex <- Parser.int
    } yield {
      DeducedReference(antecedentIndex, consequentIndex)
    }
  }
  def simplificationReferenceParser(implicit parsingContext: ParsingContext): Parser[SimplificationReference] = {
    for {
      statement <- Statement.parser
      inferenceSummary <- Inference.Summary.parser
      substitutions <- Substitutions.parser
      reference <- referenceParser.getOrElse(throw new Exception("Missing reference for simplification"))
    } yield SimplificationReference(statement, inferenceSummary, substitutions, reference)
  }
  def elidedReferenceParser(implicit parsingContext: ParsingContext): Parser[ElidedReference] = {
    (for {
      inferenceSummary <- Inference.Summary.parser
      substitutions <- Substitutions.parser
      references <- referencesParser
    } yield ElidedReference(inferenceSummary, substitutions, references)).inBraces
  }
  def expandedReferenceParser(implicit parsingContext: ParsingContext): Parser[ExpandedReference] = {
    (for {
      inferenceSummary <- Inference.Summary.parser
      substitutions <- Substitutions.parser
      references <- referencesParser
    } yield ExpandedReference(inferenceSummary, substitutions, references)).inBraces
  }
}
