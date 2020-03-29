package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.model.expressions.Statement

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  lazy val allPremiseExtractions: Seq[(Statement, Seq[(Step, Inference)])] = {
    for {
      premise <- stepContext.premises
      extractionOption <- SubstatementExtractor.getExtractionOptions(premise.statement)(this)
      if extractionOption.premises.isEmpty
      extractionApplication <- ExtractionHelper.applyExtractions(premise, extractionOption.extractionInferences, Substitutions.empty, None, None, s => (Nil, s.map(Step.Target(_))))(provingContext, stepContext).toOption.toSeq
        .map(ExtractionHelper.removeAllStructuralSimplifications(_)(provingContext))
    } yield (extractionApplication.result, extractionApplication.extractionSteps.map(s => (s, s.inference)))
  }

  lazy val allPremiseSimplifications: Seq[(Statement, Seq[(Step, Inference)])] = {
    val simplifiers = provingContext.premiseRelationSimplificationInferences ++ provingContext.premiseRelationRewriteInferences
    def helper(previous: Seq[(Statement, Seq[(Step, Inference)])], current: Seq[(Statement, Seq[(Step, Inference)])]): Seq[(Statement, Seq[(Step, Inference)])] = {
      if (current.isEmpty)
        previous
      else {
        val existing = previous ++ current
        val newSimplifications = current.flatMap { case (statement, stepsAndInferences) =>
          simplifiers.mapCollect { simplifier =>
            simplifier.getPremiseSimplification(statement, existing)(this)
              .filter { case (statement, _) => !existing.exists(_._1 == statement)}
              .map { case (statement, step) => (statement, stepsAndInferences :+ (step, simplifier.inference)) }
          }
        }
        helper(existing, newSimplifications)
      }
    }
    helper(Nil, allPremiseExtractions)
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    allPremises.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }
}

object StepProvingContext {
  def updateStepContext(f: StepContext => StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    withStepContext(f(stepProvingContext.stepContext))
  }
  def withStepContext(newStepContext: StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    if (newStepContext != stepProvingContext.stepContext)
      stepProvingContext.copy(stepContext = newStepContext)
    else
      stepProvingContext
  }
}
