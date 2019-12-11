package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.exceptions.NotFoundException
import net.prover.model.definitions.Transitivity
import net.prover.model.expressions.Term
import net.prover.model.proof.{Step, StepProvingContext}
import org.springframework.http.ResponseEntity

import scala.util.{Failure, Try}

trait TransitivityEditing extends BookModification {
  protected def getTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData
  ): Try[(Term, Term, Term, Transitivity, StepProvingContext)] = {
    for {
      (init, last) <- :+.unapply(stepPath.indexes).orNotFound(s"Step $stepPath")
      (parentStep, parentStepProvingContext) <- findStep[Step.WithSubsteps](bookKey, chapterKey, theoremKey, proofIndex, PathData(init))
      (_, step, after) <- parentStep.substeps.splitAtIndexIfValid(last).orNotFound(s"Step $stepPath")
      stepContext = parentStep.contextForChild(parentStepProvingContext.stepContext, last)
      stepProvingContext = StepProvingContext(stepContext, parentStepProvingContext.provingContext)
      statement <- step.provenStatement.orBadRequest("Step had no proven statement")
      (lhs, rhs, transitivityDefinition) <- stepProvingContext.provingContext.transitivityDefinitions.map(_._2).mapFind { transitivityDefinition =>
        for {
          (lhs, rhs) <- transitivityDefinition.relation.unapply(statement)(stepProvingContext.stepContext)
        } yield (lhs, rhs, transitivityDefinition)
      }.orBadRequest("Target step is not a transitive statement")
      (followingStep, _) <- after.headAndTailOption.orBadRequest("No following step")
      (mainLhs, _) <- followingStep.asOptionalInstanceOf[Step.Assertion]
        .filter(_.inference.id == transitivityDefinition.inference.id)
        .flatMap(s => transitivityDefinition.relation.unapply(s.statement)(stepProvingContext.stepContext))
        .orBadRequest("Following step not transitivity")
    } yield (mainLhs, lhs, rhs, transitivityDefinition, stepProvingContext)
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
                (followingStep, restOfSteps) <- after.headAndTailOption.orBadRequest("No following step")
                (mainLhs, _) <- followingStep.asOptionalInstanceOf[Step.Assertion]
                  .filter(_.inference.id == transitivityDefinition.inference.id)
                  .flatMap(s => transitivityDefinition.relation.unapply(s.statement))
                  .orBadRequest("Following step not transitivity")
                (firstStep, secondStep, intermediateTerm, targetSteps) <- f(stepProvingContext, transitivityDefinition, targetLhs, targetRhs)
                firstTransitivityStep = transitivityDefinition.assertionStep(mainLhs, targetLhs, intermediateTerm)
                secondTransitivityStep = transitivityDefinition.assertionStep(mainLhs, intermediateTerm, targetRhs)
              } yield insertTargetsBeforeTransitivity(before, targetStep, restOfSteps, (firstStep.toSeq :+ firstTransitivityStep) ++ secondStep.toSeq :+ secondTransitivityStep, targetSteps)
            }
          }.orNotFound(s"Step $stepPath").flatten
        }
    }).toResponseEntity
  }
}
