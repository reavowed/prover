package net.prover.model.proof

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  def premisesThenSimplifications: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.map(_._1) ++ premisesAndSimplifications.flatMap(_._2)
  }
  def allPremisesSimplestFirst: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }
  def allPremisesSimplestLast: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => premise +: simplifications }
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    premisesThenSimplifications.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }
}
