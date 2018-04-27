package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions._

case class Transformation(statementDefinition: StatementDefinition, boundVariableName: String) {
  private def generalise(statement: Statement): Statement = {
    DefinedStatement(Seq(statement), statementDefinition, statement.depth - 1)(statementDefinition.boundVariableNames)
  }
  private def specify(statement: Statement): Statement = {
    statement.specify(ArgumentList(Seq(TermVariable("_", 0)), 0))
  }

  def getSubstitutions(inference: Inference): Option[Substitutions] = {
    val requiredSubstitutions = inference.requiredSubstitutions
    if (requiredSubstitutions.terms.nonEmpty || requiredSubstitutions.predicates.nonEmpty || requiredSubstitutions.functions.nonEmpty)
      None
    else
      Some(
        Substitutions(statements =
          requiredSubstitutions.statements.map { name =>
            name -> PredicateApplication(name, ArgumentList(Seq(FunctionParameter(boundVariableName, 0)), 1))
          }.toMap,
        depth = 1))
  }

  def applyFully(inference: Inference): Option[(Seq[Premise], Statement, Seq[StepOutline])] = {
    getSubstitutions(inference).flatMap(applyFully(inference, _))
  }

  def applyFully(inference: Inference, transformationSubstitutions: Substitutions): Option[(Seq[Premise], Statement, Seq[StepOutline])] = {
    for {
      conclusionStatementToProve <- inference.conclusion.applySubstitutions(transformationSubstitutions)
      transformedConclusion = generalise(conclusionStatementToProve)
      (transformedPremises, premiseStatementsToProve) <- inference.premises.map { premise =>
        premise.statement.applySubstitutions(transformationSubstitutions).map { statementToProve =>
          premise.withStatement(generalise(statementToProve)) -> statementToProve
        }
      }.traverseOption.map(_.split)
      statementsToProve = premiseStatementsToProve :+ conclusionStatementToProve
      stepsToProve = Seq(StepOutline.ScopedVariable(boundVariableName, statementsToProve.map(s => StepOutline.Assertion(s, None, None)), None))
    } yield (transformedPremises, transformedConclusion, stepsToProve)
  }

  def applyPartially(inference: Inference,  transformationSubstitutions: Substitutions): Option[(Seq[Seq[(Premise, Option[StepOutline])]], Statement, StepOutline)] = {
    for {
      transformedConclusion <- inference.conclusion.applySubstitutions(transformationSubstitutions).map(specify)
      transformedPremisesAndSteps <- inference.premises.map { premise =>
        premise.statement.applySubstitutions(transformationSubstitutions).map { statement =>
          Seq(
            premise.withStatement(generalise(statement)) -> Some(StepOutline.Assertion(specify(statement), None, None)),
            premise.withStatement(specify(statement)) -> None)
        }
      }.traverseOption
    } yield (transformedPremisesAndSteps, transformedConclusion, StepOutline.Assertion(transformedConclusion, None, None))
  }
}

object Transformation {
  def apply(statementDefinition: StatementDefinition): Option[Transformation] = {
    for {
      variableName <- statementDefinition.boundVariableNames.single
    } yield {
      Transformation(statementDefinition, variableName)
    }
  }
}