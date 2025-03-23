package net.prover.controllers.models

import monocle.Lens
import net.prover.model.*
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.SubstitutionContext

case class SuggestedSubstitutions(
  statements: Seq[Option[Statement]],
  terms: Seq[Option[Term]],
  statementApplications: Seq[Seq[Statement]],
  termApplications: Seq[Seq[Term]])

object SuggestedSubstitutions {
  def apply(variableDefinitions: VariableDefinitions, possibleSubstitutions: Substitutions.Possible)(implicit substitutionContext: SubstitutionContext): SuggestedSubstitutions = {
    def filterValues[T <: Expression](
      definitions: Seq[VariableDefinition],
      map: Map[Int, T]
    ): Seq[Option[T]] = {
      definitions.indices.map(map.get)
    }

    def filterApplications[T <: Expression](
      definitions: Seq[VariableDefinition],
      applicationsMap: Map[Int, Seq[(Seq[Term], T, Int)]],
      lens: Lens[Substitutions.Possible, Map[Int, T]],
      applicationLens: Lens[Substitutions.Possible, Map[Int, Seq[(Seq[Term], T, Int)]]]
    ): Seq[Seq[T]] = {
      definitions.mapWithIndex { (definition, index) =>
        applicationsMap.getOrElse(index, Nil)
          .find { case (arguments, _, _) => (0 until definition.arity).forall(i => arguments(i).tryApplySubstitutions(possibleSubstitutions).nonEmpty) }
          .map { case (arguments, value, depth) =>
            value.calculateApplicatives(arguments, possibleSubstitutions, 0, depth, substitutionContext.externalDepth).map(_._1.asInstanceOf[T]).toSeq
          }
          .getOrElse(Nil)
      }
    }

    SuggestedSubstitutions(
      filterValues(variableDefinitions.statements, possibleSubstitutions.statements),
      filterValues(variableDefinitions.terms, possibleSubstitutions.terms),
      filterApplications(variableDefinitions.statements,  possibleSubstitutions.statementApplications, Substitutions.Possible.statementsLens, Substitutions.Possible.statementApplicationsLens),
      filterApplications(variableDefinitions.terms, possibleSubstitutions.termApplications, Substitutions.Possible.termsLens, Substitutions.Possible.termApplicationsLens))
  }
}
