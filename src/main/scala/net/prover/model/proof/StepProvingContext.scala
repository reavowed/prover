package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.definitions.{BinaryRelation, BinaryRelationStatement, Equality, PremiseSimplificationInference, Wrapper}
import net.prover.model.expressions.{DefinedTerm, FunctionParameter, Statement, Term, TermVariable}
import net.prover.model._
import net.prover.model.utils.TermUtils

import scala.collection.mutable

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  lazy val allPremiseExtractions: Seq[(Statement, Seq[DerivationStep])] = {
    for {
      premise <- stepContext.premises
      extractionOption <- SubstatementExtractor.getExtractionOptions(premise.statement)(this)
      if extractionOption.premises.isEmpty
      (result, derivationSteps) <- ExtractionHelper.getPremiseExtractionWithoutPremises(premise, extractionOption.extractionInferences)(this).toSeq
    } yield (result, derivationSteps)
  }

  lazy val allPremiseSimplifications: Seq[(Statement, Seq[DerivationStep])] = {
    implicit val substitutionContext: SubstitutionContext = stepContext
    def simplifyAll(
      previous: Seq[(Statement, Seq[DerivationStep])],
      current: Seq[(Statement, Seq[DerivationStep])],
      simplifiersByRelation: Map[BinaryRelation, Seq[PremiseSimplificationInference]]
    ): Seq[(Statement, Seq[DerivationStep])] = {
      if (current.isEmpty)
        previous
      else {
        val existing = previous ++ current
        val newSimplifications = current.flatMap { case (statement, currentDerivationSteps) => {
          val simplifiers = provingContext.findRelation(statement).flatMap(r => simplifiersByRelation.get(r.relation)).getOrElse(Nil)
          simplifiers.mapCollect { simplifier =>
            simplifier.getPremiseSimplification(statement, existing)(this)
              .filter { case (statement, _) => !existing.exists(_._1 == statement)}
              .map { case (statement, newDerivationSteps) => (statement, currentDerivationSteps ++ newDerivationSteps) }
          }
        }}
        simplifyAll(existing, newSimplifications, simplifiersByRelation)
      }
    }
    def replaceEqualities(current: Seq[(Statement, Seq[DerivationStep])]): Seq[(Statement, Seq[DerivationStep])] = {
      def getReplacements(lhs: Term, rhs: Term, equality: Equality): Seq[(Statement, Seq[DerivationStep])] = {
        for {
          (premiseToRewrite, previousSteps) <- current
          newStep <- EqualityRewriter.getForwardReplacements(premiseToRewrite, lhs, rhs, equality)(stepContext)
        } yield (newStep.statement, previousSteps :+ newStep)
      }

      val equalityPremises = for {
        equality <- provingContext.equalityOption.toSeq
        (equalityPremise, previousSteps) <- current
        (premiseLhs, premiseRhs) <- equality.unapply(equalityPremise).toSeq
        if (premiseLhs.asOptionalInstanceOf[TermVariable].exists(_.arguments.isEmpty) || premiseLhs.isInstanceOf[FunctionParameter])
        (result, replacementSteps) <- getReplacements(premiseLhs, premiseRhs, equality)
      } yield (result, previousSteps ++ replacementSteps)

      current ++ equalityPremises
    }

    val afterSimplifications = simplifyAll(Nil, allPremiseExtractions, provingContext.premiseRelationSimplificationInferences)
    val afterRewrites = simplifyAll(Nil, afterSimplifications, provingContext.premiseRelationRewriteInferences)
    replaceEqualities(afterRewrites)
  }

  lazy val premiseSimplificationsBySerializedStatement: Map[String, Seq[DerivationStep]] = {
    allPremiseSimplifications.map(_.mapLeft(_.serialized)).toMapPreservingEarliest
  }

  lazy val renamedTerms: Seq[(Term, Term, () => Seq[DerivationStep], Equality)] = {
    def isValid(lhs: Term, rhs: Term) = {
      (TermUtils.isSimpleTermVariable(lhs) || TermUtils.isCombinationOfConstants(lhs)) && TermUtils.isWrappedSimpleTerm(rhs)
    }

    def fromPremises = for {
      equality <- provingContext.equalityOption.toSeq
      (premise, derivationSteps) <- allPremiseSimplifications.map(_.mapRight(v => () => v))
      (lhs, rhs) <- equality.unapply(premise)(stepContext).toSeq
      if isValid(lhs, rhs)
    } yield (lhs, rhs, derivationSteps, equality)

    def fromFacts = for {
      equality <- provingContext.equalityOption.toSeq
      (fact, inference, extractionOption) <- provingContext.facts
      (lhs, rhs) <- equality.unapply(fact)(stepContext).toSeq
      if isValid(lhs, rhs)
    } yield (lhs, rhs, () => Seq(ExtractionHelper.getInferenceExtractionWithoutPremises(inference, Substitutions.empty, extractionOption)(this).get), equality)

    fromPremises ++ fromFacts
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

  val cachedPremises: mutable.Map[String, Option[Seq[DerivationStep]]] = mutable.Map.empty
}

object StepProvingContext {
  def updateStepContext(f: StepContext => StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.updateStepContext(f)
  }
  def withStepContext(newStepContext: StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.withStepContext(newStepContext)
  }
}
