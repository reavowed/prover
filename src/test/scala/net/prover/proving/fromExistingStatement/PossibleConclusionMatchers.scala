package net.prover.proving.fromExistingStatement

import net.prover.controllers.models.{PossibleConclusionWithPremises, SuggestedSubstitutions}
import net.prover.model.expressions.Statement
import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationLike

trait PossibleConclusionMatchers extends SpecificationLike {
  def bePossibleConclusionWithPremises(conclusion: Statement, premises: Seq[Statement]): Matcher[PossibleConclusionWithPremises] = {
    (beEqualTo(conclusion) ^^ ((_: PossibleConclusionWithPremises).conclusion)) and
      (contain(exactly[Statement](premises*)) ^^ ((_: PossibleConclusionWithPremises).possiblePremises.map(_.premise)))
  }
  def bePossibleConclusionWithSubstitutions(substitutions: SuggestedSubstitutions): Matcher[PossibleConclusionWithPremises] = {
    beSome(beEqualTo(substitutions)) ^^ ((_: PossibleConclusionWithPremises).substitutions)
  }
}
