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
      validatedSteps <- CachedProof.validateSteps(proof.steps, context)
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
    context: ProvingContext
  ): Option[Seq[Step]] = {
    def helper(
      stepsToValidate: Seq[Step],
      validatedSteps: Seq[Step],
      currentContext: ProvingContext
    ): Option[(Seq[Step], ProvingContext)] = {
      stepsToValidate match {
        case Nil =>
          Some((validatedSteps, currentContext))
        case stepToValidate +: otherStepsToValidate =>
          for {
            validatedStep <- validateStep(stepToValidate, currentContext)
            result <- helper(
              otherStepsToValidate,
              validatedSteps :+ validatedStep,
              currentContext.addFact(validatedStep.referencedFact))
          } yield result
      }
    }
    helper(steps, Nil, context).map(_._1)
  }

  def validateStep(
    step: Step,
    context: ProvingContext
  ): Option[Step] = {
    step match {
      case assumptionStep: AssumptionStep =>
        validateAssumptionStep(assumptionStep, context)
      case assertionStep: AssertionStep =>
        validateAssertionStep(assertionStep, context)
      case rearrangementStep: RearrangementStep =>
        validateRearrangementStep(rearrangementStep, context)
      case namingStep: NamingStep =>
        validateNamingStep(namingStep, context)
      case _ =>
        None
    }
  }

  private def validateAssertionStep(
    assertionStep: AssertionStep,
    context: ProvingContext
  ): Option[AssertionStep] = {
    import assertionStep._
    for {
      (validatedConclusion, validatedInferenceApplication) <-
        validateInferenceApplication(inferenceApplication, context)
      _ = if (validatedConclusion != statement)
        CachedProof.logger.info(s"Inference conclusion '${validatedConclusion.serialized}' was not '${statement.serialized}'")
      if validatedConclusion == statement
    } yield AssertionStep(statement, validatedInferenceApplication, reference)
  }

  private def validateAssumptionStep(
    assumptionStep: AssumptionStep,
    context: ProvingContext
  ): Option[AssumptionStep] = {
    import assumptionStep._
    val assumptionContext = context.addFact(Fact.Direct(assumption), reference)
    for {
      validatedSubsteps <- validateSteps(steps, assumptionContext)
    } yield AssumptionStep(assumption, validatedSubsteps, reference)
  }

  def validateRearrangementStep(
    rearrangementStep: RearrangementStep,
    context: ProvingContext
  ): Option[RearrangementStep] = {
    import rearrangementStep._
    for {
      (validatedRearrangement, rearrangedFact) <- validateReference(rearrangement, context)
        .map(_.mapLeft(_.asInstanceOf[Reference.Rearrangement]))
        .map(_.mapRight(_.asInstanceOf[Fact.Direct]))
      _ = if (rearrangedFact.statement != statement)
        CachedProof.logger.info(s"Rearrangement '${rearrangement.serialized}' was to ${rearrangedFact.statement.serialized}," +
          s" not ${statement.serialized}")
      if rearrangedFact.statement == statement
    } yield RearrangementStep(statement, validatedRearrangement, reference)
  }

  def validateNamingStep(
    namingStep: NamingStep,
    context: ProvingContext
  ): Option[NamingStep] = {
    import namingStep._
    for {
      validatedAssumptionStep <- validateAssumptionStep(assumptionStep, context)
      deduction = validatedAssumptionStep.referencedFact.getOrElse(throw new Exception("Naming step assumption must prove a fact"))
      validatedAssertionStep <- validateStep(assertionStep, context.addFact(deduction))
    } yield NamingStep(variable, validatedAssumptionStep, validatedAssertionStep.asInstanceOf[StepWithProvenStatement], reference)
  }

  private def validateInferenceApplication(
    inferenceApplication: InferenceApplication,
    context: ProvingContext
  ): Option[(Statement, InferenceApplication)] = {
    import inferenceApplication._
    for {
      inference <- context.availableInferences.find(_.id == inferenceSummary.id).ifEmpty {
        CachedProof.logger.info(s"Could not find inference ${inferenceSummary.id}")
      }
      (conclusion, validatedReferences) <- validateInference(inference, substitutions, references, context)
    } yield (conclusion, InferenceApplication(inference.summary, substitutions, validatedReferences))
  }

  private def validateInference(
    inference: Inference,
    inferenceSubstitutions: Inference.Substitutions,
    references: Seq[Reference],
    context: ProvingContext
  ): Option[(Statement, Seq[Reference])] = {
    for {
      substitutions <- inference.generalizeSubstitutions(inferenceSubstitutions)
      substitutedPremiseFacts <- inference.premises.map(_.fact.applySubstitutions(substitutions)).traverseOption.ifEmpty {
        CachedProof.logger.info(
          (Seq(s"Could not substitute into premises of inference '${inference.name}'") ++
            inference.premises.map(_.serialized)
            :+ substitutions.serialized
          ).mkString("\n"))
      }
      substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions).ifEmpty {
        CachedProof.logger.info(Seq(
          s"Could not substitute into conclusion inference '${inference.name}'",
          inference.conclusion.serialized,
          substitutions.serialized
        ).mkString("\n"))
      }
      validatedReferences <- validateReferences(references, substitutedPremiseFacts, context)
    } yield (substitutedConclusion, validatedReferences)
  }

  private def validateReferences(
    references: Seq[Reference],
    premiseFacts: Seq[Fact],
    context: ProvingContext
  ): Option[Seq[Reference]] = {
    for {
      premiseFactsWithReferences <- premiseFacts.zipStrict(references)
      resolvedPremises <- premiseFactsWithReferences.map { case (premiseFact, reference) =>
        validateReference(reference, premiseFact, context)
      }.traverseOption
    } yield resolvedPremises
  }

  private def validateReference(
    reference: Reference,
    premiseFact: Fact,
    context: ProvingContext
  ): Option[Reference] = {
    for {
      (validatedReference, referencedFact) <- validateReference(reference, context)
      _ = if (referencedFact != premiseFact)
        CachedProof.logger.info(s"Reference '${reference.serialized}' was to ${referencedFact.serialized}, not ${premiseFact.serialized}")
      if referencedFact == premiseFact
    } yield validatedReference
  }

  private def validateReference(
    reference: Reference,
    context: ProvingContext
  ): Option[(Reference, Fact)] = {
    reference match {
      case directReference: Reference.Direct =>
        validateDirectReference(directReference, context).map(directReference -> _)
      case expansionReference: Reference.Expansion =>
        validateExpansionReference(expansionReference, context)
      case simplificationReference: Reference.Simplification =>
        validateSimplificationReference(simplificationReference, context)
      case elidedReference: Reference.Elided =>
        validateElidedReference(elidedReference, context)
    }
  }

  private def validateDirectReference(reference: Reference.Direct, context: ProvingContext): Option[Fact] = {
    context.referencedFacts
      .find(_.reference == reference)
      .map(_.fact)
      .ifEmpty {
        CachedProof.logger.info(s"Direct reference '${reference.value}' did not exist")
      }
  }

  private def validateExpansionReference(reference: Reference.Expansion, context: ProvingContext): Option[(Reference, Fact)] = {
    for {
      (statement, validatedApplication) <- validateInferenceApplication(reference.inferenceApplication, context)
    } yield (Reference.Expansion(validatedApplication), Fact.Direct(statement))
  }

  private def validateSimplificationReference(reference: Reference.Simplification, context: ProvingContext): Option[(Reference, Fact)] = {
    for {
      (statement, validatedApplication) <- validateInferenceApplication(reference.inferenceApplication, context)
    } yield (Reference.Simplification(validatedApplication), Fact.Direct(statement))
  }

  private def validateElidedReference(reference: Reference.Elided, context: ProvingContext): Option[(Reference, Fact)] = {
    for {
      (statement, validatedApplication) <- validateInferenceApplication(reference.inferenceApplication, context)
    } yield (Reference.Elided(validatedApplication), Fact.Direct(statement))
  }
}
