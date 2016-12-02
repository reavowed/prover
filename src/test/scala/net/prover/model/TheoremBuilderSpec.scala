package net.prover.model

class TheoremBuilderSpec extends ProverSpec {
  "theorem builder" should {
    "handle simple direct rule" in {
      val rule = DirectRule("restate", Seq(StatementVariable(1)), StatementVariable(1), Nil)
      val theoremBuilder = TheoremBuilder().addHypothesis(StatementVariable(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "h1", defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(StatementVariable(1)))
    }

    "handle direct rule referencing fantasy hypothesis" in {
      val rule = DirectRule("restate", Seq(StatementVariable(1)), StatementVariable(1), Nil)
      val theoremBuilder = TheoremBuilder().addFantasy(StatementVariable(1))
      val updatedTheoremBuilder = rule.readAndUpdateTheoremBuilder(theoremBuilder, "f.h", defaultContext)
      updatedTheoremBuilder.fantasyOption.get.steps mustEqual Seq(Step(StatementVariable(1)))
    }

    "handle simple direct rule with a more complicated match" in {
      val rule = DirectRule("restate", Seq(StatementVariable(1)), StatementVariable(1), Nil)
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
      val theorem = Theorem("and-sym", "Conjunction is Symmetric", Seq(Conjunction(1, 2)), Nil, Conjunction(2, 1), Nil)
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
      val rule = DirectRule("eliminateAll", Seq(ForAll(1, 1)), StatementVariableWithReplacement(1, 2, 1), Nil)
      val theoremBuilder = rule.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(ForAll(2, Equals(2, 1))),
        "1 3",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual Equals(3, 1)
    }

    val IntroduceAll = DirectRule(
      "introduceAll",
      Seq(StatementVariableWithReplacement(1, 2, 1)),
      ForAll(1, 1),
      Seq(2))

    "handle rule with a substitution in the premise applied to a non-substituted statement" in {
      val theoremBuilder = IntroduceAll.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Conjunction(Equals(3, 3), ElementOf(1, 2))),
        "1 ∧ = 4 4 ∈ 1 2 3 4",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(4, Conjunction(Equals(4, 4), ElementOf(1, 2)))
    }

    "handle rule with a substitution in the premise applied to a substituted statement" in {
      val theoremBuilder = IntroduceAll.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(StatementVariableWithReplacement(3, 2, 1)),
        "1",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(1, 3)
    }

    "add a rule's arbitrary variable to the theorem if it appears in a theorem hypothesis" in {
      val theoremBuilder = IntroduceAll.readAndUpdateTheoremBuilder(
        TheoremBuilder().addHypothesis(Equals(1, 2)),
        "h1 = 1 3 2 3",
        defaultContext)
      theoremBuilder.arbitraryVariables mustEqual Seq(TermVariable(2))
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy hypothesis" in {
      IntroduceAll.readAndUpdateTheoremBuilder(
        TheoremBuilder().addFantasy(Equals(1, 2)),
        "f.h = 1 3 2 3",
        defaultContext
      ) must throwAn[ArbitraryVariableException]
    }

    "apply a rule with a single substitution premise to a self construction" in {
      val theoremBuilder = IntroduceAll.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Equals(1, 2)),
        "1 self 1",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(1, Equals(1, 2))
    }
  }
}
