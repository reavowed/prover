package net.prover.controllers.models

import monocle.Lens
import net.prover.model.{Substitutions, VariableDefinition, VariableDefinitions}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.StepContext
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.{ApplicativeCalculator, PossibleSubstitutionApplier}

case class SuggestedSubstitutions(
  statements: Seq[Option[Statement]],
  terms: Seq[Option[Term]],
  statementApplications: Seq[Seq[Statement]],
  termApplications: Seq[Seq[Term]])

object SuggestedSubstitutions {
  def apply(variableDefinitions: VariableDefinitions, possibleSubstitutions: PossibleSubstitutions)(implicit stepContext: StepContext): SuggestedSubstitutions = {
    def filterValues[T <: Expression](
      definitions: Seq[VariableDefinition],
      map: Map[Int, T]
    ): Seq[Option[T]] = {
      definitions.indices.map(map.get)
    }

    def filterApplications[T <: Expression](
      definitions: Seq[VariableDefinition],
      applicationsMap: Map[Int, Seq[(Seq[Term], T, Int)]]
    ): Seq[Seq[T]] = {
      definitions.mapWithIndex { (definition, index) =>
        applicationsMap.getOrElse(index, Nil)
          .find { case (arguments, _, _) => (0 until definition.arity).forall(i => PossibleSubstitutionApplier.applySubstitutions(arguments(i), possibleSubstitutions).isSuccess) }
          .map { case (arguments, value, depth) =>
            ApplicativeCalculator.calculateApplicatives(value, arguments, possibleSubstitutions, depth, stepContext).map(_._1.asInstanceOf[T]).toSeq
          }
          .getOrElse(Nil)
      }
    }

    SuggestedSubstitutions(
      filterValues(variableDefinitions.statements, possibleSubstitutions.statements),
      filterValues(variableDefinitions.terms, possibleSubstitutions.terms),
      filterApplications(variableDefinitions.statements,  possibleSubstitutions.statementApplications),
      filterApplications(variableDefinitions.terms, possibleSubstitutions.termApplications))
  }
}
