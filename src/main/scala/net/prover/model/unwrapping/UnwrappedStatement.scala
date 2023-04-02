package net.prover.model.unwrapping

import net.prover.model.expressions.Statement
import net.prover.model.proof.StepContext
import net.prover.model.proof.SubstatementExtractor.VariableTracker

case class UnwrappedStatement(statement: Statement, unwrappers: Seq[Unwrapper]) {
  def definitionSymbols: Seq[String] = unwrappers.map(_.definitionSymbol)
}

object UnwrappedStatement {
  def getUnwrappedStatements(statement: Statement)(implicit stepContext: StepContext): Seq[UnwrappedStatement] = {
    import stepContext.provingContext
    def helper(currentUnwrappedStatement: UnwrappedStatement, resultsSoFar: Seq[UnwrappedStatement], variableTracker: VariableTracker): Seq[UnwrappedStatement] = {
      def byGeneralization = for {
        generalizationDefinition <- provingContext.generalizationDefinitionOption
        (specificationInference, _) <- provingContext.specificationInferenceOption
        (variableName, predicate) <- generalizationDefinition.unapply(currentUnwrappedStatement.statement)
        (uniqueVariableName, _, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(variableName)
        newUnwrappedStatement = UnwrappedStatement(predicate, currentUnwrappedStatement.unwrappers :+ GeneralizationUnwrapper(uniqueVariableName, generalizationDefinition, specificationInference))
      } yield (newUnwrappedStatement, newVariableTracker)
      def byDeduction = for {
        deductionDefinition <- provingContext.deductionDefinitionOption
        (deductionEliminationInference, _, _) <- provingContext.deductionEliminationInferenceOption
        (antecedent, consequent) <- deductionDefinition.unapply(currentUnwrappedStatement.statement)
        newUnwrappedStatement = UnwrappedStatement(consequent, currentUnwrappedStatement.unwrappers :+ DeductionUnwrapper(antecedent, deductionDefinition, deductionEliminationInference))
      } yield (newUnwrappedStatement, variableTracker)
      (byGeneralization orElse byDeduction) match {
        case Some((newUnwrappedStatement, newVariableTracker)) =>
          helper(newUnwrappedStatement, resultsSoFar :+ newUnwrappedStatement, newVariableTracker)
        case None =>
          resultsSoFar
      }
    }
    val initialUnwrappedStatement = UnwrappedStatement(statement, Nil)
    helper(initialUnwrappedStatement, Seq(initialUnwrappedStatement), VariableTracker.fromStepContext)
  }
}
