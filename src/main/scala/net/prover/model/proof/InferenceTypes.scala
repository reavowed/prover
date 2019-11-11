package net.prover.model.proof

import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.model.{Inference, Substitutions}

object InferenceTypes {
  def isTransitivity(inference: Inference): Boolean =  {
    getTransitivityPredicate(inference).isDefined
  }

  def getTransitivityPredicate(inference: Inference): Option[Statement] = {
    inference.requiredSubstitutions match {
      case Substitutions.Required(Nil, Seq(a, b, c), Nil, Nil) =>
        inference.conclusion.calculateApplicatives(Seq(TermVariable(a), TermVariable(c)), Substitutions.Possible.empty, 0, 0, 0).find { case (predicate, substitutions) =>
          predicate.requiredSubstitutions.isEmpty &&
            substitutions == Substitutions.Possible(terms = Map(a -> TermVariable(a), c -> TermVariable(c))) &&
            inference.premises == Seq(predicate.specify(Seq(TermVariable(a), TermVariable(b)), 0, 0), predicate.specify(Seq(TermVariable(b), TermVariable(c)), 0, 0))
        }.map(_._1)
      case _ =>
        None
    }

  }
}
