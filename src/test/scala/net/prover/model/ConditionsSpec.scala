package net.prover.model

class ConditionsSpec extends ProverSpec {
  "conditions" should {
    "correctly apply distinct variable condition to a statement with the term variable substituted out" in {
      Conditions(Set.empty, Map(x -> Variables(Set(φ), Set.empty)))
        .applySubstitutions(Substitutions(Map(φ -> SubstitutedStatementVariable(φ, z, y)), Map(x -> y))).get
        .distinctVariables mustEqual Map(y -> Variables(Set.empty, Set(z)))
    }

    "remove arbitrary variables under distinct variable conditions" in {
      Conditions(Set(x), Map(y -> Variables(Set.empty, Set(x))))
        .restrictToStatements(Seq(ForAll(x, φ), Exists(y, ForAll(x, ψ))))
        .arbitraryVariables must beEmpty
    }

    "only add present variables to distinct conditions" in {
      Conditions.empty
          .addDistinctVariables(Set(x), Seq(SubstitutedStatementVariable(φ, y, x))).get
          .distinctVariables mustEqual Map(x -> Variables(Set.empty, Set(y)))
    }
  }
}
