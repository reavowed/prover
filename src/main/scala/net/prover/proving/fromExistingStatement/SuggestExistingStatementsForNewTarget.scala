package net.prover.proving.fromExistingStatement

import net.prover.controllers._
import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises}
import net.prover.model.Substitutions
import net.prover.model.expressions.{FunctionParameter, Statement, StatementVariable, TermVariable}
import net.prover.model.proof.{Step, SubstatementExtractor}

import scala.util.Try

object SuggestExistingStatementsForNewTarget {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    serializedPremiseStatement: String)(
    implicit bookService: BookService
  ): Try[Seq[PossibleConclusionWithPremises]] = {
    (for {
      (_, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
      premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
      // First of all, initialise the substitutions with all the existing variables in the theorem
      baseSubstitutions = Substitutions.Possible(
        stepProvingContext.stepContext.variableDefinitions.statements.mapWithIndex((variableDefinition, index) => index -> StatementVariable(index, (0 until variableDefinition.arity).map(FunctionParameter(_, 0)))).toMap,
        stepProvingContext.stepContext.variableDefinitions.terms.mapWithIndex((variableDefinition, index) => index -> TermVariable(index, (0 until variableDefinition.arity).map(FunctionParameter(_, 0)))).toMap)
      // Then add any premise-specific variables that might be missing
    } yield {
      implicit val spc = stepProvingContext
      SubstatementExtractor.getPremiseExtractions(premise.statement)
        .map(PossibleConclusionWithPremises.fromExtraction(_, Some(baseSubstitutions))(stepProvingContext))
    })
  }
}
