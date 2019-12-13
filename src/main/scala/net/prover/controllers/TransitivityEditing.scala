package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.exceptions.NotFoundException
import net.prover.model.definitions.{BinaryRelation, Transitivity}
import net.prover.model.expressions.Term
import net.prover.model.proof.{Step, StepProvingContext}
import org.springframework.http.ResponseEntity

import scala.util.{Failure, Try}

trait TransitivityEditing extends BookModification {
  protected def getRelation(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData
  ): Try[(Term, Term, BinaryRelation, StepProvingContext)] = {
    for {
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      statement <- step.provenStatement.orBadRequest("Step had no proven statement")
      (lhs, rhs, relation) <- stepProvingContext.provingContext.definedBinaryRelations.map(_._2).mapFind { relation =>
        for {
          (lhs, rhs) <- relation.unapply(statement)(stepProvingContext.stepContext)
        } yield (lhs, rhs, relation)
      }.orBadRequest("Target step is not a relation")
    } yield (lhs, rhs, relation, stepProvingContext)
  }

  protected def insertTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    f: (StepProvingContext, Transitivity, Term, Term) => Try[(Option[Step], Option[Step], Term, Seq[Step.Target])]
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
                (targetLhs, targetRhs, transitivityDefinition) <- provingContext.transitivityDefinitions.map(_._2).mapFind { transitivityDefinition =>
                  for {
                    (lhs, rhs) <- transitivityDefinition.relation.unapply(targetStep.statement)
                  } yield (lhs, rhs, transitivityDefinition)
                }.orBadRequest("Target step is not a transitive statement")
                createFinalTransitivitySteps = after.headAndTailOption match {
                  case Some((Step.Assertion(transitivityDefinition.relation(mainLhs, _), transitivityDefinition.inference, _, _), restOfSteps)) =>
                    (firstStep: Option[Step], secondStep: Option[Step], intermediateTerm: Term) =>
                      val firstTransitivityStep = transitivityDefinition.assertionStep(mainLhs, targetLhs, intermediateTerm)
                      val secondTransitivityStep = transitivityDefinition.assertionStep(mainLhs, intermediateTerm, targetRhs)
                      (firstStep.toSeq :+ firstTransitivityStep) ++ (secondStep.toSeq :+ secondTransitivityStep) ++ restOfSteps
                  case _ =>
                    (firstStep: Option[Step], secondStep: Option[Step], intermediateTerm: Term) =>
                      val secondTransitivityStep = transitivityDefinition.assertionStep(targetLhs, intermediateTerm, targetRhs)
                      (firstStep.toSeq ++ secondStep.toSeq :+ secondTransitivityStep) ++ after
                }
                (firstStep, secondStep, intermediateTerm, targetSteps) <- f(stepProvingContext, transitivityDefinition, targetLhs, targetRhs)
                transitivitySteps = createFinalTransitivitySteps(firstStep, secondStep, intermediateTerm)
              } yield insertTargetsBeforeTransitivity(before, targetStep, transitivitySteps, targetSteps)
            }
          }.orNotFound(s"Step $stepPath").flatten
        }
    }).toResponseEntity
  }
}
