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
    ReplaceStepAddingTargetsBeforeChain[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (targetStep, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        (targetStatement, unwrappers) <- UnwrappedStatement.getUnwrappedStatements(targetStep.statement).find(_.definitionSymbols == definition.wrappingSymbols).map(x => (x.statement, x.unwrappers)).orBadRequest("Invalid wrapping symbols")
        (result, newStep, targets) <- CreateProofStep(definition, (_, _) => Success(Some(targetStatement)), unwrappers)
        _ <- (result == targetStep.statement).orBadRequest("Conclusion was incorrect")
      } yield (newStep, targets)
    }
  }
}
