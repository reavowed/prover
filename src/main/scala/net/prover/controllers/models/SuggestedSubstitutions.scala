package net.prover.controllers.models

import monocle.Lens
import net.prover.model.Substitutions
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.StepContext

case class SuggestedSubstitutions(
  statements: Map[String, Statement],
  terms: Map[String, Term],
  predicates: Map[(String, Int), Statement],
  functions: Map[(String, Int), Term],
  predicateApplications: Map[(String, Int), Seq[Statement]],
  functionApplications: Map[(String, Int), Seq[Term]])
{
  def confirmTotality: Option[Substitutions] = {
    if (predicateApplications.isEmpty && functionApplications.isEmpty)
      Some(Substitutions(statements, terms, predicates, functions))
    else
      None
  }
}

object SuggestedSubstitutions {
  val empty = SuggestedSubstitutions(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def apply(possibleSubstitutions: Substitutions.Possible)(implicit stepContext: StepContext): SuggestedSubstitutions = {
    val plainSubstitutions = possibleSubstitutions.stripApplications()
    def filterApplications[T <: Expression](
      applicationsMap: Map[(String, Int), Seq[(Seq[Term], T, Int)]],
      lens: Lens[Substitutions.Possible, Map[(String, Int), T]],
      applicationLens: Lens[Substitutions.Possible, Map[(String, Int), Seq[(Seq[Term], T, Int)]]]
    ): Map[(String, Int), Seq[T]] = {
      applicationsMap
        .map { case ((name, arity), applications) =>
          val results = applications
            .find { case (arguments, _, _) => (0 until arity).forall(i => arguments(i).applySubstitutions(plainSubstitutions).nonEmpty) }
            .map { case (arguments, value, depth) =>
              value.calculateApplicatives(arguments, plainSubstitutions, 0, depth, stepContext.externalDepth).map(_._1.asInstanceOf[T]).toSeq
            }
            .getOrElse(Nil)

          ((name, arity), results)
        }
        .filter { case (_, applications) => applications.nonEmpty }
    }

    SuggestedSubstitutions(
      possibleSubstitutions.statements,
      possibleSubstitutions.terms,
      possibleSubstitutions.predicates,
      possibleSubstitutions.functions,
      filterApplications(possibleSubstitutions.predicateApplications, Substitutions.Possible.predicatesLens, Substitutions.Possible.predicateApplicationsLens),
      filterApplications(possibleSubstitutions.functionApplications, Substitutions.Possible.functionsLens, Substitutions.Possible.functionApplicationsLens))
  }
}
