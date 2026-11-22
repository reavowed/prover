package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.{StepWithContext, TypedStepWithContext}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.premiseFinding.DerivationOrTargetFinder
import net.prover.proving.rewrite.RewritePremise
import net.prover.theorems.{CompoundTheoremUpdater, GetReferencedPremises, RecalculateReferences}
import org.slf4j.{Logger, LoggerFactory}
import scalaz.Scalaz.Id

import scala.util.{Failure, Success}

/**
  * Rederives the premises of every step that derives them, using the current premise finding. This also replaces any
  * derivations saved before derivations were structured. A step whose premises can't be rederived is left as it was.
  */
object RederivePremises extends CompoundTheoremUpdater[Id] {
  val logger: Logger = LoggerFactory.getLogger(RederivePremises.getClass)

  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply)
  }

  override def updateInferenceWithPremiseDerivations(step: Step.InferenceWithPremiseDerivationsStep, stepWithContext: StepWithContext): Step = {
    val assertionStepWithContext = TypedStepWithContext(
      step.assertionStep,
      stepWithContext.proofWithContext)(
      using summon,
      stepWithContext.stepContext.forChild().addSteps(step.premises.flatMap(_.toProofSteps)))
    val premiseStatements = GetReferencedPremises(assertionStepWithContext).map(_.statement)
    rederive(premiseStatements, stepWithContext)
      .map(premises => replace(step, Step.InferenceWithPremiseDerivationsStep.ifNecessary(premises, step.assertionStep), stepWithContext))
      .getOrElse(step)
  }

  override def updateExistingStatementExtraction(step: Step.ExistingStatementExtractionStep, stepWithContext: StepWithContext): Step = {
    rederive(step.premises.map(_.statement), stepWithContext)
      .flatMap(premises => Step.ExistingStatementExtractionStep.ifNecessary(premises, step.extraction))
      .map(replace(step, _, stepWithContext))
      .getOrElse(step)
  }

  override def updateRewriteStep(step: Step.RewriteStep, stepWithContext: StepWithContext): Step = {
    val newPremise = step.premise match {
      case RewritePremise.Known(derivation) =>
        rederive(Seq(derivation.statement), stepWithContext).map(premises => RewritePremise.Known(premises.head))
      case RewritePremise.ByInference(premises, extraction) =>
        rederive(premises.map(_.statement), stepWithContext).map(RewritePremise.ByInference(_, extraction))
    }
    newPremise
      .map(premise => replace(step, Step.RewriteStep.ifNecessary(premise, step.substitutionStep), stepWithContext))
      .getOrElse(step)
  }

  private def rederive(premiseStatements: Seq[Statement], stepWithContext: StepWithContext): Option[Seq[DerivedStatement]] = {
    val (premises, targets) = DerivationOrTargetFinder.findDerivationsOrTargets(premiseStatements)(stepWithContext.stepProvingContext)
    if (targets.isEmpty) {
      Some(premises)
    } else {
      logger.warn(s"${describe(stepWithContext)}: could not rederive ${targets.map(_.statement).mkString(", ")}")
      None
    }
  }

  private def replace(oldStep: Step, newStep: Step, stepWithContext: StepWithContext): Step = {
    if (newStep == oldStep) {
      oldStep
    } else {
      RecalculateReferences(stepWithContext.withStep(newStep)) match {
        case Success((recalculatedStep, _)) =>
          recalculatedStep
        case Failure(exception) =>
          logger.warn(s"${describe(stepWithContext)}: could not recalculate references (${exception.getMessage})")
          oldStep
      }
    }
  }

  private def describe(stepWithContext: StepWithContext): String = {
    s"${stepWithContext.proofWithContext.theoremWithContext.theorem.title} step ${stepWithContext.stepContext.stepReference.stepPath.mkString(".")}"
  }
}
