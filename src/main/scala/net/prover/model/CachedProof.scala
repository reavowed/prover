package net.prover.model

import java.nio.file.Path

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}
import net.prover.model.Proof._
import net.prover.model.components.Statement
import org.slf4j.LoggerFactory

case class CachedProof(path: Path, premises: Seq[Premise], proof: Proof) {
  def validate(
    availableInferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform]
  ): Option[Proof] = {
    val context = Proof.getInitialContext(premises, availableInferences, inferenceTransforms)
    for {
      (validatedSteps, _) <- CachedProof.validateSteps(proof.steps, context, premises.length)
    } yield Proof(validatedSteps)
  }

  def serialized: String = (premises.map(_.serialized) :+ proof.serialized).mkString("\n")
}

object CachedProof {
  val logger = LoggerFactory.getLogger(CachedProof.getClass)

  def parser(path: Path)(implicit parsingContext: ParsingContext): Parser[CachedProof] = {
    for {
      premises <- Inference.premisesParser
      proof <- Proof.parser
    } yield CachedProof(path, premises, proof)
  }

  private def validateSteps(
    steps: Seq[Step],
    context: ProvingContext,
    nextReference: Int
  ): Option[(Seq[Step], ProvingContext)] = {
    def helper(
      stepsToValidate: Seq[Step],
      validatedSteps: Seq[Step],
      currentContext: ProvingContext,
      currentNextReference: Int
    ): Option[(Seq[Step], ProvingContext)] = {
      stepsToValidate match {
        case Nil =>
          Some((validatedSteps, currentContext))
        case stepToValidate +: otherStepsToValidate =>
          for {
            (validatedStep, updatedContext) <- validateStep(stepToValidate, currentContext, currentNextReference)
            result <- helper(
              otherStepsToValidate,
              validatedSteps :+ validatedStep,
              updatedContext,
              currentNextReference + 1)
          } yield result
      }
    }
    helper(steps, Nil, context, nextReference)
  }

  def validateStep(
    step: Step,
    context: ProvingContext,
    nextReference: Int
  ): Option[(Step, ProvingContext)] = {
    step match {
      case assumptionStep: AssumptionStep =>
        validateAssumptionStep(assumptionStep, context, nextReference)
      case assertionStep: AssertionStep =>
        validateAssertionStep(assertionStep, context, nextReference)
      case transformedInferenceStep: TransformedInferenceStep =>
        validateTransformedInferenceStep(transformedInferenceStep, context, nextReference)
      case rearrangementStep: RearrangementStep =>
        validateRearrangementStep(rearrangementStep, context, nextReference)
      case namingStep: NamingStep =>
        validateNamingStep(namingStep, context, nextReference)
      case _ =>
        None
    }
  }

  private def validateAssertionStep(
    assertionStep: AssertionStep,
    context: ProvingContext,
    nextReference: Int
  ): Option[(AssertionStep, ProvingContext)] = {
    import assertionStep._
    for {
      (validatedProvenStatement, validatedInferenceSummary, validatedReferences) <-
        validateInference(inference, substitutions, references, context)
      _ = if (validatedProvenStatement != provenStatement)
        CachedProof.logger.info(s"Inference conclusion '${validatedProvenStatement.serialized}' was not '${provenStatement.serialized}'")
      if validatedProvenStatement == provenStatement
    } yield (
      AssertionStep(provenStatement, validatedInferenceSummary, substitutions, validatedReferences),
      context.addAssertion(provenStatement, nextReference))
  }

  private def validateAssumptionStep(
    assumptionStep: AssumptionStep,
    context: ProvingContext,
    nextReference: Int
  ): Option[(AssumptionStep, ProvingContext)] = {
    import assumptionStep._
    val assumptionContext = context.addAssumption(assumption, nextReference)
    for {
      (validatedSubsteps, _) <- validateSteps(steps, assumptionContext, nextReference + 1)
    } yield {
      val newDeductions = assumptionStep.steps.zipWithIndex.collect {
        case (StepWithProvenStatement(deduction), index) =>
          ReferencedDeduction(assumption, deduction, DeducedReference(nextReference, nextReference + index + 1))
      }
      (AssumptionStep(assumption, validatedSubsteps), context.add(newDeductions))
    }
  }

  def validateTransformedInferenceStep(
    transformedInferenceStep: TransformedInferenceStep,
    context: ProvingContext,
    nextReference: Int
  ): Option[(TransformedInferenceStep, ProvingContext)] = {
    import transformedInferenceStep._
    val transformationContext = Proof.getInitialContext(premises, context.availableInferences, Nil)
    for {
      (validatedTransformationSteps, _) <- validateSteps(transformationSteps, transformationContext, premises.length)
      validatedTransformationAssertionSteps <- validatedTransformationSteps.toType[AssertionStep]
      validatedAssertionSteps <- validatedTransformationSteps.toType[AssertionStep]
      fullInference <- context.availableInferences.find(_.id == inference.id)
      transformedInference = TransformedInference(fullInference, premises, validatedAssertionSteps.last.provenStatement)
      (conclusion, validatedReferences) <- validateInference(transformedInference, substitutions, references, context)
      if conclusion == provenStatement
    } yield {
      (TransformedInferenceStep(
        provenStatement,
        transformedInference.summary,
        premises,
        validatedTransformationAssertionSteps,
        substitutions,
        validatedReferences),
        context.addAssertion(provenStatement, nextReference))
    }
  }

  def validateRearrangementStep(
    rearrangementStep: RearrangementStep,
    context: ProvingContext,
    nextReference: Int
  ): Option[(RearrangementStep, ProvingContext)] = {
    import rearrangementStep._
    for {
      (referencedStatement, validatedReference) <- validateDirectReference(reference, context)
      _ = if (referencedStatement != provenStatement)
        CachedProof.logger.info(s"Rearrangement result '${referencedStatement.serialized}' was not '${provenStatement.serialized}'")
      if referencedStatement == provenStatement
    } yield (
      RearrangementStep(referencedStatement, validatedReference),
      context.addAssertion(provenStatement, nextReference))
  }

  def validateNamingStep(
    namingStep: NamingStep,
    context: ProvingContext,
    nextReference: Int
  ): Option[(NamingStep, ProvingContext)] = {
    import namingStep._
    for {
      (validatedAssumptionStep, assumptionContext) <- validateAssumptionStep(assumptionStep, context, nextReference)
      (validatedAssertionStep, assertionContext) <- validateStep(assertionStep, assumptionContext, nextReference)
    } yield (NamingStep(variable, validatedAssumptionStep, validatedAssertionStep.asInstanceOf[StepWithProvenStatement]), assertionContext)
  }

  private def validateInference(
    inferenceSummary: Inference.Summary,
    substitutions: Substitutions,
    references: Seq[Reference],
    context: ProvingContext
  ): Option[(ProvenStatement, Inference.Summary, Seq[Reference])] = {
    for {
      inference <- context.availableInferences.find(_.id == inferenceSummary.id).ifEmpty {
        CachedProof.logger.info(s"Could not find inference ${inferenceSummary.id}")
      }
      (conclusion, validatedReferences) <- validateInference(inference, substitutions, references, context)
    } yield (conclusion, inference.summary, validatedReferences)
  }

  private def validateInference(
    inference: Inference,
    substitutions: Substitutions,
    references: Seq[Reference],
    context: ProvingContext
  ): Option[(ProvenStatement, Seq[Reference])] = {
    for {
      substitutedPremises <- inference.premises.map(_.applySubstitutions(substitutions)).traverseOption.ifEmpty {
        CachedProof.logger.info(
          (Seq(s"Could not substitute premises into premises of inference '${inference.name}'") ++
            inference.premises.map(_.serialized)
            :+ substitutions.serialized
          ).mkString("\n"))
      }
      substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions).ifEmpty {
        CachedProof.logger.info(Seq(
          s"Could not substitute premises into conclusion inference '${inference.name}'",
          inference.conclusion.serialized,
          substitutions.serialized
        ).mkString("\n"))
      }
      (premiseConditions, validatedReferences) <- validatePremises(substitutedPremises, references, context).map(_.split)
      conditions <- Conditions
        .combine(
          substitutedConclusion,
          premiseConditions,
          context.premises,
          context.assumptions,
          substitutions)
        .ifEmpty {
          CachedProof.logger.info("Invalid conditions")
        }
    } yield (ProvenStatement(substitutedConclusion.statement, conditions), validatedReferences)
  }

  private def validatePremises(
    premises: Seq[Premise],
    references: Seq[Reference],
    context: ProvingContext
  ): Option[Seq[(Conditions, Reference)]] = {
    for {
      premisesWithReferences <- premises.zipStrict(references)
      resolvedPremises <- premisesWithReferences.map { case (premise, reference) =>
        premise match {
          case DirectPremise(statement) =>
            validateDirectPremise(statement, reference, context)
          case DeducedPremise(antecedent, consequent) =>
            validateDeducedPremise(antecedent, consequent, reference, context)
        }
      }.traverseOption
    } yield resolvedPremises
  }

  private def validateDirectPremise(
    statement: Statement,
    reference: Reference,
    context: ProvingContext
  ): Option[(Conditions, Reference)] = {
    for {
      (referencedProvenStatement, validatedReference) <- validateDirectReference(reference, context)
      _ = if (referencedProvenStatement.statement != statement)
        CachedProof.logger.info(s"Reference '${reference.serialized}' was to ${referencedProvenStatement.statement}, not $statement")
      if referencedProvenStatement.statement == statement
    } yield (referencedProvenStatement.conditions, validatedReference)
  }

  private def validateDeducedPremise(
    antecedent: Statement,
    consequent: Statement,
    reference: Reference,
    context: ProvingContext
  ): Option[(Conditions, Reference)] = {
    context.provenDeductions
      .find(_.reference == reference)
      .map(_.deduction.conditions)
      .map(_ -> reference)
      .ifEmpty {
        CachedProof.logger.info(s"Deduced premise '${reference.serialized}' did not exist")
      }
  }

  private def validateDirectReference(reference: Reference, context: ProvingContext): Option[(ProvenStatement, Reference)] = {
    reference match {
      case DirectReference(index) =>
        context.provenAssertions
          .find(_.reference == reference)
          .map(_.provenStatement)
          .map(_ -> reference)
          .ifEmpty {
            CachedProof.logger.info(s"Direct premise '${reference.serialized}' did not exist")
          }
      case SimplificationReference(simplifiedStatement, inferenceSummary, substitutions, simplificationReference) =>
        for {
          (provenStatement, validatedInferenceSummary, validatedReferences) <- validateInference(
            inferenceSummary,
            substitutions,
            Seq(simplificationReference),
            context)
        } yield (provenStatement, SimplificationReference(simplifiedStatement, validatedInferenceSummary, substitutions, validatedReferences.head))
      case ElidedReference(inferenceSummary, substitutions, references) =>
        for {
          (statement, validatedInferenceSummary, validatedReferences) <- validateInference(inferenceSummary, substitutions, references, context)
        } yield (statement, ElidedReference(validatedInferenceSummary, substitutions, validatedReferences))
      case ExpandedReference(inferenceSummary, substitutions, references) =>
        for {
          (statement, validatedInferenceSummary, validatedReferences) <- validateInference(inferenceSummary, substitutions, references, context)
        } yield (statement, ExpandedReference(validatedInferenceSummary, substitutions, validatedReferences))
      case _ =>
        CachedProof.logger.info(s"Unrecognised reference '${reference.serialized}'")
        None
    }
  }
}
