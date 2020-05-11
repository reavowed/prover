package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.definitions.{Equality, PremiseSimplificationInference, Wrapper}
import net.prover.model.expressions.{FunctionParameter, Statement, Term, TermVariable}
import net.prover.model._

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
    def simplifyAll(previous: Seq[(Statement, Seq[DerivationStep])], current: Seq[(Statement, Seq[DerivationStep])], simplifiers: Seq[PremiseSimplificationInference]): Seq[(Statement, Seq[DerivationStep])] = {
      if (current.isEmpty)
        previous
      else {
        val existing = previous ++ current
        val newSimplifications = current.flatMap { case (statement, currentDerivationSteps) =>
          simplifiers.mapCollect { simplifier =>
            simplifier.getPremiseSimplification(statement, existing)(this)
              .filter { case (statement, _) => !existing.exists(_._1 == statement)}
              .map { case (statement, newDerivationSteps) => (statement, currentDerivationSteps ++ newDerivationSteps) }
          }
        }
        simplifyAll(existing, newSimplifications, simplifiers)
      }
    }
    def replaceEqualities(current: Seq[(Statement, Seq[DerivationStep])]): Seq[(Statement, Seq[DerivationStep])] = {
      def replacePath(statement: Statement, path: Seq[Int], lhs: Term, rhs: Term, equality: Equality): Option[DerivationStep] = {
        for {
          wrapper <- statement.getTerms().find(_._4 == path).filter(_._1 == lhs).map(_._2).map(Wrapper.fromExpression)
          step = equality.substitution.assertionStep(lhs, rhs, wrapper)
        } yield DerivationStep.fromAssertion(step)
      }
      def replaceAllPaths(statement: Statement, paths: Seq[Seq[Int]], lhs: Term, rhs: Term, equality: Equality): Option[DerivationStep] = {
        for {
          (statement, derivationSteps) <- paths.mapFoldOption(statement) { replacePath(_, _, lhs, rhs, equality).map(step => (step.statement, step))  }
          finalStep <- Step.Elided.ifNecessary(derivationSteps.steps, equality.substitution.inference)
        } yield DerivationStep(statement, equality.substitution.inference, finalStep)
      }
      def getReplacements(lhs: Term, rhs: Term, equality: Equality): Seq[(Statement, Seq[DerivationStep])] = {
        if (lhs.asOptionalInstanceOf[TermVariable].exists(_.arguments.isEmpty) || lhs.isInstanceOf[FunctionParameter])
          for {
            (premiseToRewrite, previousSteps) <- current
            paths = premiseToRewrite.getTerms()(stepContext).filter(_._1 == lhs).map(_._4)
            if paths.nonEmpty && (equality.unapply(premiseToRewrite).isEmpty || paths.exists(_.length > 1))
            replacementStep <- replaceAllPaths(premiseToRewrite, paths, lhs, rhs, equality)
          } yield (replacementStep.statement, previousSteps :+ replacementStep)
        else
          Nil
      }

      val equalityPremises = for {
        equality <- provingContext.equalityOption.toSeq
        (equalityPremise, previousSteps) <- current
        (premiseLhs, premiseRhs) <- equality.unapply(equalityPremise).toSeq
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
