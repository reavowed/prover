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
      extractionApplication <- ExtractionHelper.applyExtractions(premise, extractionOption.extractionInferences, Substitutions.empty, None, s => (Nil, s.map(Step.Target(_))))(this).toOption.toSeq
        .map(ExtractionHelper.removeAllStructuralSimplifications(_)(provingContext))
    } yield (extractionApplication.result, extractionApplication.extractionSteps.map(s => (s, s.inference)))
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    allPremises.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }
}
