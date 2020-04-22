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
    implicit val substitutionContext: SubstitutionContext = stepContext
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
    def replaceEqualities(current: Seq[(Statement, Seq[PremiseStep])]): Seq[(Statement, Seq[PremiseStep])] = {
      def replacePath(statement: Statement, path: Seq[Int], lhs: Term, rhs: Term, equality: Equality): Option[PremiseStep] = {
        for {
          wrapper <- statement.getTerms().find(_._4 == path).filter(_._1 == lhs).map(_._2).map(Wrapper.fromExpression)
          step = equality.substitution.assertionStep(lhs, rhs, wrapper)
        } yield PremiseStep.fromAssertion(step)
      }
      def replaceAllPaths(statement: Statement, paths: Seq[Seq[Int]], lhs: Term, rhs: Term, equality: Equality): Option[PremiseStep] = {
        for {
          (statement, premiseSteps) <- paths.mapFoldOption(statement) { replacePath(_, _, lhs, rhs, equality).map(step => (step.statement, step))  }
          finalStep <- Step.Elided.ifNecessary(premiseSteps.steps, equality.substitution.inference)
        } yield PremiseStep(statement, equality.substitution.inference, finalStep)
      }
      def getReplacements(lhs: Term, rhs: Term, equality: Equality): Seq[(Statement, Seq[PremiseStep])] = {
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
