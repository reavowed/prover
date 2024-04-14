package net.prover.proving.fromExistingStatement

import net.prover.controllers._
import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises}
import net.prover.model.Substitutions
import net.prover.model.expressions.{FunctionParameter, Statement, StatementVariable, TermVariable}
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.proving.extraction.ExtractionCalculator
import net.prover.proving.extraction.ExtractionCalculator.PremiseExtraction

import scala.util.Try

trait SuggestExistingStatementsBase {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    serializedPremiseStatement: String)(
    implicit bookService: BookService
  ): Try[Seq[PossibleConclusionWithPremises]] = {
    bookService.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath).flatMap(implicit stepWithContext =>
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
        premise <- stepWithContext.stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        // First of all, initialise the substitutions with all the existing variables in the theorem
        baseSubstitutions = Substitutions.Possible(
          stepWithContext.stepContext.variableDefinitions.statements.mapWithIndex((variableDefinition, index) => index -> StatementVariable(index, (0 until variableDefinition.arity).map(FunctionParameter(_, 0)))).toMap,
          stepWithContext.stepContext.variableDefinitions.terms.mapWithIndex((variableDefinition, index) => index -> TermVariable(index, (0 until variableDefinition.arity).map(FunctionParameter(_, 0)))).toMap)
        // Then add any premise-specific variables that might be missing
      } yield ExtractionCalculator.getPremiseExtractions(premise.statement).flatMap(getPossibleConclusionWithPremises(_, stepWithContext.step, baseSubstitutions))
    )
  }

  def getPossibleConclusionWithPremises(
    premiseExtraction: PremiseExtraction,
    step: Step.TargetStep,
    baseSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[PossibleConclusionWithPremises]
}
