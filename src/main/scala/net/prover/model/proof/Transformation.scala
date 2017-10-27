package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions._

case class Transformation(statementDefinition: StatementDefinition, variableName: String) {
  private def transformAll(applicablePremiseStatements: Seq[Statement], applicableConclusion: Statement) = {
    def toFull(statement: Statement): Statement = {
      DefinedStatement(Seq(statement), statementDefinition, statement.depth - 1)(statementDefinition.boundVariableNames)
    }
    def toSpecified(statement: Statement): Statement = {
      statement.specify(ArgumentList(Seq(TermVariable("_", 0)), 0))
    }
    def toBound(statement: Statement): Statement = {
      statement.increaseDepth(1, 0).specify(ArgumentList(Seq(FunctionParameter(variableName, 0)), 1))
    }
    def transformNext(s: Statement) = {
      Seq((toFull(s), Some(s)), (toSpecified(s), None))
    }
    val premisesAndStatementsToProve = applicablePremiseStatements.zipWithIndex
      .foldLeft(Seq((Seq.empty[Premise], Seq.empty[Statement]))) { case (acc, (premise, index)) =>
        for {
          (premises, toProve) <- acc
          (nextPremiseStatement, nextToProve) <- transformNext(premise)
          nextPremise = Premise(nextPremiseStatement, index)(isElidable = false)
        } yield (premises :+ nextPremise, toProve ++ nextToProve)
      }
    premisesAndStatementsToProve.headOption.map { case (p, toProve) =>
      val boundSubsteps = (toProve :+ applicableConclusion).map(toBound)
      val steps = Seq(StepOutline.ScopedVariable(variableName, boundSubsteps.map(s => StepOutline.Assertion(s, None)), None))
      (p, toFull(applicableConclusion), steps)
    } ++ premisesAndStatementsToProve.drop(1).map { case (p, toProve) =>
      (p, toSpecified(applicableConclusion), (toProve :+ applicableConclusion).map(s => StepOutline.Assertion(toSpecified(s), None)))
    }
  }

  def applyToInference(
    premises: Seq[Premise],
    conclusion: Statement
  ): Seq[(Seq[Premise], Statement, Seq[StepOutline])] = {
    val requiredSubstitutions =(
      premises.map(_.requiredSubstitutions) :+
        conclusion.requiredSubstitutions
    ).foldTogether
    if (requiredSubstitutions.terms.nonEmpty || requiredSubstitutions.predicates.nonEmpty || requiredSubstitutions.functions.nonEmpty)
      Nil
    else {
      val possibleSubstitutions = requiredSubstitutions.statements
        .foldProduct(s => Seq(
          s -> PredicateApplication(s, ArgumentList(Seq(FunctionParameter(variableName, 0)), 1)),
          s -> StatementVariable(s, 1)))
        .map(ss => Substitutions(statements = ss.toMap, depth = 1))
      for {
        substitutions <- possibleSubstitutions
        applicableConclusion <- conclusion.applySubstitutions(substitutions).toSeq
        applicablePremiseStatements <- premises.map(_.statement.applySubstitutions(substitutions)).traverseOption.toSeq
        (premises, conclusion, stepsToProve) <- transformAll(applicablePremiseStatements, applicableConclusion)
      } yield (premises, conclusion, stepsToProve)
    }

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