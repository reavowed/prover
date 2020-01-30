package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.definitions.{BinaryRelation, Reversal, Transitivity, Wrapper}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.model.proof.{Step, StepProvingContext, StepReference, SubstitutionContext}
import org.springframework.http.ResponseEntity

import scala.util.{Failure, Try}

trait TransitivityEditing extends BookModification {
  protected def getRelation(
    statement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(BinaryRelation, Term, Term)] = {
    stepProvingContext.provingContext.definedBinaryRelations.reverse.mapFind { relation =>
      for {
        (lhs, rhs) <- relation.unapply(statement)(stepProvingContext.stepContext)
      } yield (relation, lhs, rhs)
    }.orBadRequest("Target step is not a relation")
  }

  def getTransitivityStep(
    source: Term,
    intermediate: Term,
    target: Term,
    firstRelation: BinaryRelation,
    secondRelation: BinaryRelation)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[(BinaryRelation, Step)] = {
    def byTransitivity = for {
      transitivity <- provingContext.transitivities.ofType[Transitivity[Term]].find(_.statement == firstRelation)
      if firstRelation == secondRelation
    } yield firstRelation -> transitivity.assertionStep(source, intermediate, target)
    def bySubstitutionFromFirst = for {
      substitution <- provingContext.substitutions.find(_.relation == firstRelation)
      reversal <- provingContext.reversals.ofType[Reversal[Term]].find(_.relation == firstRelation)
    } yield {
      secondRelation -> Step.Elided.forInference(substitution.inference)(Seq(
        reversal.assertionStep(intermediate, source),
        substitution.assertionStep(intermediate, source, new Wrapper(secondRelation(_, target)))))
    }
    def bySubstitutionFromSecond = for {
      substitution <- provingContext.substitutions.find(_.relation == secondRelation)
    } yield firstRelation -> substitution.assertionStep(intermediate, target, new Wrapper(firstRelation(source, _)))
    byTransitivity orElse bySubstitutionFromFirst orElse bySubstitutionFromSecond orBadRequest "Could not chain steps"
  }

  protected def insertTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    createSteps: (StepProvingContext, BinaryRelation, Term, Term) => Try[(BinaryRelation, Option[Step], BinaryRelation, Option[Step], Term, Seq[Step.Target])]
  ): ResponseEntity[_] = {
    (stepPath.indexes match {
      case Nil =>
        Failure(NotFoundException(s"Step $stepPath"))
      case init :+ last =>
        modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
          theorem.modifySteps(proofIndex, init) { (steps, outerStepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              implicit val stepContext = outerStepContext.addSteps(before).atIndex(last)
              implicit val stepProvingContext = StepProvingContext(stepContext, provingContext)
              for {
                targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest(s"Step was not target")
                (targetRelation, targetLhs, targetRhs) <- getRelation(targetStep.statement)
                (firstRelation, firstStep, secondRelation, secondStep, intermediateTerm, targetSteps) <- createSteps(stepProvingContext, targetRelation, targetLhs, targetRhs)
                getWithFollowingTransitivity = () => for {
                  (followingStep, remainingSteps) <- after.headAndTailOption
                  followingStatement <- followingStep.provenStatement
                  (followingRelation, followingLhs, followingRhs) <- getRelation(followingStatement).toOption
                  if followingRhs == targetRhs
                  stepPath = outerStepContext.atIndex(last).stepReference.stepPath
                  followingStepPath = outerStepContext.atIndex(last + 1).stepReference.stepPath
                  followingPremises = followingStep.recursivePremises.filter(p => !p.asOptionalInstanceOf[SingleLinePremise].flatMap(_.referencedLine.asOptionalInstanceOf[StepReference]).exists(_.stepPath.startsWith(followingStepPath)))
                  if followingPremises.exists(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath)))
                  otherPremise <- followingPremises.removeWhere(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath))).single
                  (initialRelation, initialLhs, _) <- getRelation(otherPremise.statement).toOption
                  if (initialLhs == followingLhs)
                } yield {
                  for {
                    (intermediateRelation, firstTransitivityStep) <- getTransitivityStep(initialLhs, targetLhs, intermediateTerm, initialRelation, firstRelation)
                    (lastRelation, secondTransitivityStep) <- getTransitivityStep(initialLhs, intermediateTerm, targetRhs, intermediateRelation, secondRelation)
                    _ <- (lastRelation == followingRelation).orBadRequest("Could not chain steps")
                  } yield (firstStep.toSeq :+ firstTransitivityStep) ++ (secondStep.toSeq :+ secondTransitivityStep) ++ remainingSteps
                }
                withNewTransitivity = () => for {
                  (_, transitivityStep) <- getTransitivityStep(targetLhs, intermediateTerm, targetRhs, firstRelation, secondRelation)
                } yield (firstStep.toSeq ++ secondStep.toSeq :+ transitivityStep) ++ after
                steps <- getWithFollowingTransitivity() getOrElse withNewTransitivity()
              } yield insertTargetsBeforeTransitivity(before, steps, targetSteps)
            }
          }.orNotFound(s"Step $stepPath").flatten
        }
    }).toResponseEntity
  }
}
