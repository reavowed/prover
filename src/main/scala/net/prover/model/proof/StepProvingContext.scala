package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.definitions.PremiseSimplificationInference
import net.prover.model.expressions.Statement
import net.prover.model.{ProvingContext, Substitutions}

import scala.collection.mutable

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  lazy val allPremiseExtractions: Seq[(Statement, Seq[PremiseStep])] = {
    for {
      premise <- stepContext.premises
      extractionOption <- SubstatementExtractor.getExtractionOptions(premise.statement)(this)
      if extractionOption.premises.isEmpty
      extractionApplication <- ExtractionHelper.applyExtractions(premise, extractionOption.extractionInferences, Substitutions.empty, None, None, s => (Nil, s.map(Step.Target(_))))(this).toOption.toSeq
        .map(ExtractionHelper.removeAllStructuralSimplifications(_)(provingContext))
    } yield (extractionApplication.result, extractionApplication.extractionSteps.map(PremiseStep.fromAssertion))
  }

  lazy val allPremiseSimplifications: Seq[(Statement, Seq[PremiseStep])] = {
    def simplifyAll(previous: Seq[(Statement, Seq[PremiseStep])], current: Seq[(Statement, Seq[PremiseStep])], simplifiers: Seq[PremiseSimplificationInference]): Seq[(Statement, Seq[PremiseStep])] = {
      if (current.isEmpty)
        previous
      else {
        val existing = previous ++ current
        val newSimplifications = current.flatMap { case (statement, currentPremiseSteps) =>
          simplifiers.mapCollect { simplifier =>
            simplifier.getPremiseSimplification(statement, existing)(this)
              .filter { case (statement, _) => !existing.exists(_._1 == statement)}
              .map { case (statement, newPremiseSteps) => (statement, currentPremiseSteps ++ newPremiseSteps) }
          }
        }
        simplifyAll(existing, newSimplifications, simplifiers)
      }
    }
    val afterSimplifications = simplifyAll(Nil, allPremiseExtractions, provingContext.premiseRelationSimplificationInferences)
    simplifyAll(Nil, afterSimplifications, provingContext.premiseRelationRewriteInferences)
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    allPremises.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }

  def updateStepContext(f: StepContext => StepContext): StepProvingContext = {
    withStepContext(f(stepContext))
  }
  def withStepContext(newStepContext: StepContext): StepProvingContext = {
    if (newStepContext != stepContext)
      copy(stepContext = newStepContext)
    else
      this
  }

  val cachedPremises: mutable.Map[String, Option[Seq[PremiseStep]]] = mutable.Map.empty
}

object StepProvingContext {
  def updateStepContext(f: StepContext => StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.updateStepContext(f)
  }
  def withStepContext(newStepContext: StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.withStepContext(newStepContext)
  }
}
