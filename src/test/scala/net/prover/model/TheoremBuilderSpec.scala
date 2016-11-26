package net.prover.model

class TheoremBuilderSpec extends ProverSpec {
  "theorem builder" should {
    "handle simple direct rule" in {
      val rule = DirectRule("restate", Seq(Atom(1)), Atom(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(Atom(1))
      val updatedTheoremBuilder = rule.applyToTheorem(theoremBuilder, "h1", Book(""))
      updatedTheoremBuilder.steps mustEqual Seq(Step(Atom(1)))
    }
    "handle direct rule referencing fantasy hypothesis" in {
      val rule = DirectRule("restate", Seq(Atom(1)), Atom(1))
      val theoremBuilder = TheoremBuilder().addFantasy(Atom(1))
      val updatedTheoremBuilder = rule.applyToTheorem(theoremBuilder, "f.h", Book(""))
      updatedTheoremBuilder.fantasyOption.get.steps mustEqual Seq(Step(Atom(1)))
    }

    "handle simple direct rule with a more complicated match" in {
      val rule = DirectRule("restate", Seq(Atom(1)), Atom(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(Conjunction(Atom(1), Atom(2)))
      val updatedTheoremBuilder = rule.applyToTheorem(theoremBuilder, "h1", Book(""))
      updatedTheoremBuilder.steps mustEqual Seq(Step(Conjunction(Atom(1), Atom(2))))
    }

    "fail an assumption discharging rule when theorem builder has no fantasy" in {
      val rule = FantasyRule("???", Atom(1), Nil, Atom(1))
      rule.applyToTheorem(TheoremBuilder(), "", Book("")) must throwAn[Exception]
    }

    "handle simple assumption discharging rule" in {
      val rule = FantasyRule("???", Atom(1), Nil, Atom(1))
      val theoremBuilder = TheoremBuilder().addFantasy(Atom(1))
      val updatedTheoremBuilder = rule.applyToTheorem(theoremBuilder, "", Book(""))
      updatedTheoremBuilder.steps mustEqual Seq(Step(Atom(1), Some(Step.Fantasy(Atom(1), Nil))))
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule with premise" in {
      val rule = FantasyRule("introduceImplication", Atom(1), Seq(Atom(2)), Implication(Atom(1), Atom(2)))
      val theoremBuilder = TheoremBuilder().addFantasy(Atom(1)).addStep(Step(Atom(2)))
      val updatedTheoremBuilder = rule.applyToTheorem(theoremBuilder, "1", Book(""))
      updatedTheoremBuilder.steps mustEqual Seq(Step(Implication(Atom(1), Atom(2)), Some(Step.Fantasy(Atom(1), Seq(Step(Atom(2)))))))
      updatedTheoremBuilder.fantasyOption must beNone
    }
  }
}
