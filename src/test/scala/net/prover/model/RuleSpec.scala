package net.prover.model

class RuleSpec extends ProverSpec {
  def parseRule(text: String): Rule = {
    Rule.parse(text, defaultContext)
  }

  "rule parser" should {
    "parse a rule with a single premise" in {
      parseRule("restate 1 ⇒ 1") mustEqual DirectRule("restate", Seq(Atom(1)), Atom(1))
    }
    "parse a rule with two premises" in {
      parseRule("eliminateImplication implies 1 2 & 1 ⇒ 2") mustEqual
        DirectRule("eliminateImplication", Seq(Implication(Atom(1), Atom(2)), Atom(1)), Atom(2))
    }
    "parse a rule with a discharged assumption" in {
      parseRule("introduceImplication 1 ⊢ 2 ⇒ implies 1 2") mustEqual
        FantasyRule("introduceImplication", Atom(1), Seq(Atom(2)), Implication(Atom(1), Atom(2)))
    }
    "not allow multiple discharged assumptions" in {
      parseRule("introduceImplication 1 & 2 ⊢ 3 ⇒ implies implies 1 2 3") must
        throwAn[Exception]
    }
  }
}
