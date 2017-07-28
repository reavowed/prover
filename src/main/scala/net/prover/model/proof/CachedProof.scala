package net.prover.model.proof

import java.nio.file.Path

import net.prover.model._
import net.prover.model.components.Statement
import net.prover.model.proof.Proof._
import org.slf4j.LoggerFactory

case class CachedProof(path: Path, premises: Seq[Premise], proof: Proof) {
  def validate(availableInferences: Seq[Inference]): Option[Proof] = {
    val context = Proof.getInitialContext(premises, availableInferences, Nil)
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
      premises <- Premise.listParser
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
      _ = if (validatedProvenStatement != statement)
        CachedProof.logger.info(s"Inference conclusion '${validatedProvenStatement.serialized}' was not '${statement.serialized}'")
      if validatedProvenStatement == statement
    } yield (
      AssertionStep(statement, validatedInferenceSummary, substitutions, validatedReferences),
      context.addAssertion(statement, nextReference))
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
      val updatedContext = assumptionStep.steps.ofType[StepWithProvenStatement].lastOption match {
        case Some(StepWithProvenStatement(provenStatement)) =>
          context.add(ReferencedDeduction(assumption, provenStatement, DirectReference(nextReference)))
        case None =>
          context
      }
      (AssumptionStep(assumption, validatedSubsteps), updatedContext)
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
      _ = if (referencedStatement != statement)
        CachedProof.logger.info(s"Rearrangement result '${referencedStatement.serialized}' was not '${statement.serialized}'")
      if referencedStatement == statement
    } yield (
      RearrangementStep(referencedStatement, validatedReference),
      context.addAssertion(statement, nextReference))
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
  ): Option[(Statement, Inference.Summary, Seq[Reference])] = {
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
  ): Option[(Statement, Seq[Reference])] = {
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
      validatedReferences <- validatePremises(substitutedPremises, references, context)
    } yield (substitutedConclusion, validatedReferences)
  }

  private def validatePremises(
    premises: Seq[Premise],
    references: Seq[Reference],
    context: ProvingContext
  ): Option[Seq[Reference]] = {
    for {
      premisesWithReferences <- premises.zipStrict(references)
      resolvedPremises <- premisesWithReferences.map { case (premise, reference) =>
        premise match {
          case Premise.DirectPremise(statement) =>
            validateDirectPremise(statement, reference, context)
          case Premise.DeducedPremise(antecedent, consequent) =>
            validateDeducedPremise(antecedent, consequent, reference, context)
        }
      }.traverseOption
    } yield resolvedPremises
  }

  private def validateDirectPremise(
    statement: Statement,
    reference: Reference,
    context: ProvingContext
  ): Option[Reference] = {
    for {
      (referencedStatement, validatedReference) <- validateDirectReference(reference, context)
      _ = if (referencedStatement != statement)
        CachedProof.logger.info(s"Reference '${reference.serialized}' was to $referencedStatement, not $statement")
      if referencedStatement == statement
    } yield validatedReference
  }

  private def validateDeducedPremise(
    antecedent: Statement,
    consequent: Statement,
    reference: Reference,
    context: ProvingContext
  ): Option[Reference] = {
    context.provenDeductions
      .find(_.reference == reference)
      .map(_.reference)
      .ifEmpty {
        CachedProof.logger.info(s"Deduced premise '${reference.serialized}' did not exist")
      }
  }

  private def validateDirectReference(reference: Reference, context: ProvingContext): Option[(Statement, Reference)] = {
    reference match {
      case DirectReference(_) =>
        context.provenAssertions
          .find(_.reference == reference)
          .map(_.statement -> reference)
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
