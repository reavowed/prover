package net.prover.model

class TheoremBuilderSpec extends ProverSpec {
  "theorem builder" should {
    "handle simple direct rule" in {
      val rule = DirectRule("restate", Seq(StatementVariable(1)), StatementVariable(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(StatementVariable(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "h1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(StatementVariable(1)))
    }

    "handle direct rule referencing fantasy hypothesis" in {
      val rule = DirectRule("restate", Seq(StatementVariable(1)), StatementVariable(1))
      val theoremBuilder = TheoremBuilder().addFantasy(StatementVariable(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "f.h", defaultContext)
      updatedTheoremBuilder.fantasyOption.get.steps mustEqual Seq(Step(StatementVariable(1)))
    }

    "handle simple direct rule with a more complicated match" in {
      val rule = DirectRule("restate", Seq(StatementVariable(1)), StatementVariable(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(Conjunction(StatementVariable(1), StatementVariable(2)))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "h1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Conjunction(StatementVariable(1), StatementVariable(2))))
    }

    "fail an assumption discharging rule when theorem builder has no fantasy" in {
      val rule = FantasyRule("???", StatementVariable(1), Nil, StatementVariable(1))
      rule.readAndUpdateTheoremBuilder(TheoremBuilder(), "", defaultContext) must throwAn[Exception]
    }

    "handle simple assumption discharging rule" in {
      val rule = FantasyRule("???", StatementVariable(1), Nil, StatementVariable(1))
      val theoremBuilder = TheoremBuilder().addFantasy(StatementVariable(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(StatementVariable(1), Some(Step.Fantasy(StatementVariable(1), Nil))))
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule with premise" in {
      val rule = FantasyRule("introduceImplication", StatementVariable(1), Seq(StatementVariable(2)), Implication(StatementVariable(1), StatementVariable(2)))
      val theoremBuilder = TheoremBuilder().addFantasy(StatementVariable(1)).addStep(Step(StatementVariable(2)))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "f.1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Implication(StatementVariable(1), StatementVariable(2)), Some(Step.Fantasy(StatementVariable(1), Seq(Step(StatementVariable(2)))))))
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule applied to theorem" in {
      val rule = FantasyRule("introduceImplication", StatementVariable(1), Seq(StatementVariable(2)), Implication(StatementVariable(1), StatementVariable(2)))
      val theorem = Theorem("and-sym", "Conjunction is Symmetric", Seq(Conjunction(1, 2)), Nil, Conjunction(2, 1))
      val theoremBuilder = rule.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "and-sym 2 1",
        defaultContext.copy(theorems = Seq(theorem)))
      theoremBuilder.steps mustEqual Seq(Step(Implication(Conjunction(2, 1), Conjunction(1, 2))))
    }

    "handle assumption discharging rule applied to definition" in {
      val rule = FantasyRule("introduceImplication", StatementVariable(1), Seq(StatementVariable(2)), Implication(StatementVariable(1), StatementVariable(2)))
      val theoremBuilder = rule.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "definition-∨ ∨ ¬ 1 2",
        defaultContext)
      theoremBuilder.steps mustEqual Seq(Step(
        Implication(
          Disjunction(Negation(1), 2),
          Implication(Negation(Negation(1)), 2))))
    }

    "handle rule with a substitution in the conclusion" in {
      val rule = DirectRule("eliminateAll", Seq(ForAll(1, 1)), StatementVariableWithReplacement(1, 2, 1))
      val theoremBuilder = rule.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(ForAll(2, Equals(2, 1))),
        "1 3",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual Equals(3, 1)
    }
  }
}
