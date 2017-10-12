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
      statement.specify(Seq(TermVariable("_", 0)))
    }
    def toBound(statement: Statement): Statement = {
      statement.increaseDepth(1).specify(Seq(FunctionParameter(variableName, 0)))
    }
    def transformNext(s: Statement) = {
      Seq((toFull(s), Some(s)), (toSpecified(s), None))
    }
    val premisesAndStatementsToProve = applicablePremiseStatements.zipWithIndex
      .foldLeft(Seq((Seq.empty[Premise], Seq.empty[Statement]))) { case (acc, (premise, index)) =>
        for {
          (premises, toProve) <- acc
          (nextPremiseStatement, nextToProve) <- transformNext(premise)
          nextPremise = Premise(Fact.Direct(nextPremiseStatement), index)(isElidable = false)
        } yield (premises :+ nextPremise, toProve ++ nextToProve)
      }
    premisesAndStatementsToProve.headOption.map { case (p, toProve) =>
      val boundSubsteps = (toProve :+ applicableConclusion).map(toBound)
      val steps =
        if (boundSubsteps.nonEmpty)
          Seq(StepOutline.ScopedVariable(variableName, boundSubsteps.map(s => StepOutline.Assertion(s, None))))
        else
          Nil
      (p, toFull(applicableConclusion), steps)
    } ++ premisesAndStatementsToProve.drop(1).map { case (p, toProve) =>
      (p, toSpecified(applicableConclusion), toProve.map(s => StepOutline.Assertion(toSpecified(s), None)))
    }
  }

  def applyToInference(
    premiseStatements: Seq[Statement],
    conclusion: Statement
  ): Seq[(Seq[Premise], Statement, Seq[StepOutline])] = {
    val requiredSubstitutions =(
      premiseStatements.map(_.requiredSubstitutions) :+
        conclusion.requiredSubstitutions
    ).foldTogether
    if (requiredSubstitutions.terms.nonEmpty || requiredSubstitutions.predicates.nonEmpty || requiredSubstitutions.functions.nonEmpty)
      Nil
    else {
      val possibleSubstitutions = requiredSubstitutions.statements
        .foldProduct(s => Seq(
          s -> PredicateApplication(s, Seq(FunctionParameter(variableName, 0)), 1),
          s -> StatementVariable(s, 1)))
        .map(ss => Substitutions(statements = ss.toMap, depth = 1))
      for {
        substitutions <- possibleSubstitutions
        applicableConclusion <- conclusion.applySubstitutions(substitutions).toSeq
        applicablePremiseStatements <- premiseStatements.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
        (premises, conclusion, stepsToProve) <- transformAll(applicablePremiseStatements, applicableConclusion)
      } yield (premises, conclusion, stepsToProve)
    }

  }
}