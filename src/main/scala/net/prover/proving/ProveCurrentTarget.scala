package net.prover.proving

import net.prover.controllers.{BookService, BooleanWithResponseExceptionOps, OptionWithResponseExceptionOps}
import net.prover.controllers.models.{InsertionAndReplacementProps, PathData, ProofUpdateProps, StepDefinition}
import net.prover.model.proof.Step
import net.prover.model.unwrapping.UnwrappedStatement
import net.prover.proving.stepReplacement.ReplaceStepAddingTargetsBeforeChain

import scala.util.{Success, Try}

object ProveCurrentTarget {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepReference: PathData,
    definition: StepDefinition)(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[InsertionAndReplacementProps]] = {
    ReplaceStepAddingTargetsBeforeChain[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { implicit stepWithContext =>
      for {
        (targetStatement, unwrappers) <- UnwrappedStatement.getUnwrappedStatements(stepWithContext.step.statement).find(_.definitionSymbols == definition.wrappingSymbols).map(x => (x.statement, x.unwrappers)).orBadRequest("Invalid wrapping symbols")
        (newStep, targets) <- CreateProofStep(definition, (_, _) => Success(Some(targetStatement)), unwrappers)
        _ <- (newStep.statement == stepWithContext.step.statement).orBadRequest("Conclusion was incorrect")
      } yield (newStep, targets)
    }
  }
}
