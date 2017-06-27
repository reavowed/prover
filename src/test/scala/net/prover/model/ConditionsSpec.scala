package net.prover.model

class ConditionsSpec extends ProverSpec {
  "conditions" should {
    "correctly apply distinct variable condition to a statement with the term variable substituted out" in {
      Conditions(Set.empty, DistinctVariables(x -> φ))
        .applySubstitutions(Substitutions(Map(φ -> SubstitutedStatementVariable(φ, z, y)), Map(x -> y), DistinctVariables.empty)).get
        .distinctVariables mustEqual DistinctVariables(y -> z)
    }

    "correctly apply distinct variable condition to a compound statement with the term variable substituted out" in {
      Conditions(Set.empty, DistinctVariables(x -> φ))
        .applySubstitutions(Substitutions(Map(φ -> ForAll(z, SubstitutedStatementVariable(φ, z, y))), Map(x -> y), DistinctVariables.empty)).get
        .distinctVariables mustEqual DistinctVariables(y -> z)
    }

    "correctly apply distinct variable condition to a term variable substituted for a function" in {
      Conditions(Set.empty, DistinctVariables(x -> y))
        .applySubstitutions(Substitutions(Map.empty, Map(x -> x, y -> PowerSet(y)), DistinctVariables.empty)).get
        .distinctVariables mustEqual DistinctVariables(x -> y)
    }

    "not allow a substitution that violates distinct variables" in {
      Conditions(Set.empty, DistinctVariables(x -> y))
        .applySubstitutions(Substitutions(Map.empty, Map(x -> y, y -> y), DistinctVariables.empty))
        .must(beNone)
    }

    "remove arbitrary variable that only appears bound" in {
      Conditions(Set(x), DistinctVariables.empty)
        .restrictToStatements(Seq(ForAll(x, φ), Exists(y, ForAll(x, ψ))))
        .arbitraryVariables must beEmpty
    }

    "remove arbitrary variable if its only possible appearance is another arbitrary variable" in {
      Conditions(Set(x, y), DistinctVariables.empty)
        .restrictToStatements(Seq(SubstitutedStatementVariable(φ, y, x)))
        .arbitraryVariables mustEqual Set(y)
    }

    "only add present variables to distinct conditions" in {
      Conditions.empty
          .addDistinctVariables(Set(x), Seq(SubstitutedStatementVariable(φ, y, x)))
          .distinctVariables mustEqual DistinctVariables(x -> y)
    }
  }
}
