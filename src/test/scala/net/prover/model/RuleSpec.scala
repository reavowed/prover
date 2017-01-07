package net.prover.model

class RuleSpec extends ProverSpec {
  def parseRule(text: String): Rule = {
    Rule.parse(text, defaultContext)
  }

  "rule parser" should {
    "parse a rule with a single premise" in {
      parseRule("restate 1 ⇒ 1") mustEqual DirectRule(
        "restate",
        Seq(StatementVariable(1)),
        StatementVariable(1),
        Nil,
        DistinctVariables.empty)
    }

    "parse a rule with two premises" in {
      parseRule("eliminateImplication → 1 2 & 1 ⇒ 2") mustEqual
        DirectRule(
          "eliminateImplication",
          Seq(Implication(StatementVariable(1), StatementVariable(2)), StatementVariable(1)),
          StatementVariable(2),
          Nil,
          DistinctVariables.empty)
    }

    "parse a rule with a discharged assumption" in {
      parseRule("introduceImplication 1 ⊢ 2 ⇒ → 1 2") mustEqual
        FantasyRule(
          "introduceImplication",
          StatementVariable(1),
          Seq(StatementVariable(2)),
          Implication(StatementVariable(1), StatementVariable(2)))
    }

    "not allow multiple discharged assumptions" in {
      parseRule("introduceImplication 1 & 2 ⊢ 3 ⇒ → → 1 2 3") must
        throwAn[Exception]
    }

    "parse a rule with arbitrary variables" in {
      parseRule("introduceAll sub 2 1 1 ⇒ ∀ 1 1 | 2") mustEqual
        DirectRule(
          "introduceAll",
          Seq(StatementVariableWithReplacement(1, 2, 1)),
          ForAll(1, 1),
          Seq(2),
          DistinctVariables.empty)
    }
  }
}
