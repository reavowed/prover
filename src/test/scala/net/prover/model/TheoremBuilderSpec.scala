package net.prover.model

class TheoremBuilderSpec extends ProverSpec {
  "theorem builder" should {
    "handle simple direct rule" in {
      val rule = DirectRule("restate", Seq(Atom(1)), Atom(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(Atom(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "h1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Atom(1)))
    }
    "handle direct rule referencing fantasy hypothesis" in {
      val rule = DirectRule("restate", Seq(Atom(1)), Atom(1))
      val theoremBuilder = TheoremBuilder().addFantasy(Atom(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "f.h", defaultContext)
      updatedTheoremBuilder.fantasyOption.get.steps mustEqual Seq(Step(Atom(1)))
    }

    "handle simple direct rule with a more complicated match" in {
      val rule = DirectRule("restate", Seq(Atom(1)), Atom(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(Conjunction(Atom(1), Atom(2)))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "h1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Conjunction(Atom(1), Atom(2))))
    }

    "fail an assumption discharging rule when theorem builder has no fantasy" in {
      val rule = FantasyRule("???", Atom(1), Nil, Atom(1))
      rule.readAndUpdateTheoremBuilder(TheoremBuilder(), "", defaultContext) must throwAn[Exception]
    }

    "handle simple assumption discharging rule" in {
      val rule = FantasyRule("???", Atom(1), Nil, Atom(1))
      val theoremBuilder = TheoremBuilder().addFantasy(Atom(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Atom(1), Some(Step.Fantasy(Atom(1), Nil))))
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule with premise" in {
      val rule = FantasyRule("introduceImplication", Atom(1), Seq(Atom(2)), Implication(Atom(1), Atom(2)))
      val theoremBuilder = TheoremBuilder().addFantasy(Atom(1)).addStep(Step(Atom(2)))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "f.1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Implication(Atom(1), Atom(2)), Some(Step.Fantasy(Atom(1), Seq(Step(Atom(2)))))))
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule applied to theorem" in {
      val rule = FantasyRule("introduceImplication", Atom(1), Seq(Atom(2)), Implication(Atom(1), Atom(2)))
      val theorem = Theorem("and-sym", "Conjunction is Symmetric", Seq(Conjunction(1, 2)), Nil, Conjunction(2, 1))
      val theoremBuilder = rule.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "and-sym 2 1",
        defaultContext.copy(theorems = Seq(theorem)))
      theoremBuilder.steps mustEqual Seq(Step(Implication(Conjunction(2, 1), Conjunction(1, 2))))
    }

    "handle assumption discharging rule applied to definition" in {
      val rule = FantasyRule("introduceImplication", Atom(1), Seq(Atom(2)), Implication(Atom(1), Atom(2)))
      val definition = Definition(Disjunction, Implication(Negation(1), 2))
      val theoremBuilder = rule.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "definition-or or not 1 2",
        defaultContext.copy(definitions = Seq(definition)))
      theoremBuilder.steps mustEqual Seq(Step(
        Implication(
          Disjunction(Negation(1), 2),
          Implication(Negation(Negation(1)), 2))))
    }
  }
}
