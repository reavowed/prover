package net.prover.model

class TheoremBuilderSpec extends ProverSpec {

  "theorem builder" should {
    "handle simple direct rule" in {
      val updatedTheoremBuilder = Restate.readAndUpdateTheoremBuilder(
        TheoremBuilder().addPremise(StatementVariable(1)),
        "p1",
        defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(StatementVariable(1)))
    }

    "handle direct rule referencing fantasy hypothesis" in {
      val updatedTheoremBuilder = Restate.readAndUpdateTheoremBuilder(
        TheoremBuilder().addFantasy(StatementVariable(1)),
        "f.a",
        defaultContext)
      updatedTheoremBuilder.fantasyOption.get.steps mustEqual Seq(Step(StatementVariable(1)))
    }

    "handle simple direct rule with a more complicated match" in {
      val updatedTheoremBuilder = Restate.readAndUpdateTheoremBuilder(
        TheoremBuilder().addPremise(Conjunction(StatementVariable(1), StatementVariable(2))),
        "p1",
        defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(Conjunction(StatementVariable(1), StatementVariable(2))))
    }

    "fail an assumption discharging rule when theorem builder has no fantasy" in {
      IntroduceImplication.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "",
        defaultContext
      ) must throwAn[Exception]
    }

    "handle assumption discharging rule with premise" in {
      val updatedTheoremBuilder = IntroduceImplication.readAndUpdateTheoremBuilder(
        TheoremBuilder().addFantasy(StatementVariable(1)).addStep(Step(StatementVariable(2))),
        "f.1",
        defaultContext)
      updatedTheoremBuilder.steps mustEqual Seq(Step(
        Implication(StatementVariable(1), StatementVariable(2)),
        Some(Step.Fantasy(StatementVariable(1), Seq(Step(StatementVariable(2)))))))
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule applied to theorem" in {
      val theorem = Theorem(
        "and-sym",
        "Conjunction is Symmetric",
        Seq(Conjunction(1, 2)),
        Nil,
        Conjunction(2, 1),
        Nil,
        DistinctVariables.empty)
      val theoremBuilder = IntroduceImplication.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "and-sym ∧ 2 1",
        defaultContext.copy(otherTheoremLineParsers = Seq(theorem)))
      theoremBuilder.steps mustEqual Seq(Step(Implication(Conjunction(2, 1), Conjunction(1, 2))))
    }

    "handle assumption discharging rule applied to definition" in {
      val theoremBuilder = IntroduceImplication.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "unapply-∨ ∨ ¬ 1 2",
        defaultContext)
      theoremBuilder.steps mustEqual Seq(Step(
        Implication(
          Disjunction(Negation(1), 2),
          Implication(Negation(Negation(1)), 2))))
    }

    "handle rule with a substitution in the conclusion" in {
      val theoremBuilder = EliminateForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(ForAll(2, Equals(2, 1))),
        "1 3",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual Equals(3, 1)
    }

    "handle rule with a substitution in the premise applied to a non-substituted statement" in {
      val theoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Conjunction(Equals(3, 3), ElementOf(1, 2))),
        "1 ∧ = 4 4 ∈ 1 2 3 4",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(4, Conjunction(Equals(4, 4), ElementOf(1, 2)))
    }

    "handle rule with a substitution in the premise applied to a substituted statement" in {
      val theoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(StatementVariableWithReplacement(3, 2, 1)),
        "1",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(1, 3)
    }

    "add a rule's arbitrary variable to the theorem if it appears in a theorem hypothesis" in {
      val theoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addPremise(Equals(1, 2)),
        "p1 = 1 3 2 3",
        defaultContext)
      theoremBuilder.arbitraryVariables mustEqual Seq(TermVariable(2))
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy hypothesis" in {
      IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addFantasy(Equals(1, 2)),
        "f.a = 1 3 2 3",
        defaultContext
      ) must throwAn[ArbitraryVariableException]
    }

    "disallow a rule if distinct variable requirements are not followed" in {
      IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Equals(1, 1)),
        "1 = 2 1 1 2",
        defaultContext
      ) must throwA[DistinctVariableViolationException]
    }

    "apply a rule by adding distinct variables if necessary" in {
      val updatedTheoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Equivalence(StatementVariableWithReplacement(1, 3, 1), Equals(3, 4))),
        "1 ↔ 1 = 1 4 3 1",
        defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual ForAll(1, Equivalence(1, Equals(1, 4)))
    }
  }
}
