package net.prover.model

class TheoremBuilderSpec extends ProverSpec {

  "theorem builder" should {
    "handle simple direct rule" in {
      val updatedTheoremBuilder = Restate.readAndUpdateTheoremBuilder(
        TheoremBuilder().addPremise(φ),
        "p1",
        defaultContext)
      updatedTheoremBuilder.steps.head.statement mustEqual φ
    }

    "handle direct rule referencing fantasy hypothesis" in {
      val updatedTheoremBuilder = Restate.readAndUpdateTheoremBuilder(
        TheoremBuilder().addFantasy(φ),
        "f.a",
        defaultContext)
      updatedTheoremBuilder.fantasyOption.get.steps.head.statement mustEqual φ
    }

    "handle simple direct rule with a more complicated match" in {
      val updatedTheoremBuilder = Restate.readAndUpdateTheoremBuilder(
        TheoremBuilder().addPremise(Conjunction(φ, ψ)),
        "p1",
        defaultContext)
      updatedTheoremBuilder.steps.head.statement mustEqual Conjunction(φ, ψ)
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
        TheoremBuilder().addFantasy(φ).addStep(ψ),
        "f.1",
        defaultContext)
      val step = updatedTheoremBuilder.steps.head
      step.statement mustEqual Implication(φ, ψ)
      step.fantasy must beSome
      updatedTheoremBuilder.fantasyOption must beNone
    }

    "handle assumption discharging rule applied to theorem" in {
      val theorem = Theorem(
        "and-sym",
        "Conjunction is Symmetric",
        Seq(Conjunction(φ, ψ)),
        Nil,
        Conjunction(ψ, φ),
        Nil,
        DistinctVariables.empty)
      val theoremBuilder = IntroduceImplication.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "and-sym ∧ ψ φ",
        defaultContext.copy(theoremLineParsers = Seq(theorem)))
      theoremBuilder.steps.head.statement mustEqual Implication(Conjunction(ψ, φ), Conjunction(φ, ψ))
    }

    "handle assumption discharging rule applied to definition" in {
      val theoremBuilder = IntroduceImplication.readAndUpdateTheoremBuilder(
        TheoremBuilder(),
        "unapply-∨ ∨ ¬ φ ψ",
        defaultContext)
      theoremBuilder.steps.head.statement mustEqual Implication(
        Disjunction(Negation(φ), ψ),
        Implication(Negation(Negation(φ)), ψ))
    }

    "handle rule with a substitution in the conclusion" in {
      val theoremBuilder = EliminateForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(ForAll(y, Equals(y, x))),
        "1 z",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual Equals(z, x)
    }

    "handle rule with a substitution in the premise applied to a non-substituted statement" in {
      val theoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Conjunction(Equals(x, x), ElementOf(y, x))),
        "1 ∧ = z z ∈ y z x z",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(z, Conjunction(Equals(z, z), ElementOf(y, z)))
    }

    "handle rule with a substitution in the premise applied to a substituted statement" in {
      val theoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(StatementVariableWithReplacement(φ, y, x)),
        "1",
        defaultContext)
      theoremBuilder.steps(1).statement mustEqual ForAll(x, φ)
    }

    "add a rule's arbitrary variable to the theorem if it appears in a theorem premise" in {
      val theoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addPremise(Equals(x, y)),
        "p1 = x z y z",
        defaultContext)
      theoremBuilder.arbitraryVariables mustEqual Seq(y)
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy assumption" in {
      IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addFantasy(Equals(x, y)),
        "f.a = x z y z",
        defaultContext
      ) must throwAn[ArbitraryVariableException]
    }

    "disallow a rule if distinct variable requirements are not followed" in {
      IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Equals(z, z)),
        "1 = y z z y",
        defaultContext
      ) must throwA[DistinctVariableViolationException]
    }

    "apply a rule by adding distinct variables if necessary" in {
      val updatedTheoremBuilder = IntroduceForall.readAndUpdateTheoremBuilder(
        TheoremBuilder().addStep(Equivalence(StatementVariableWithReplacement(φ, y, x), Equals(y, z))),
        "1 ↔ φ = x z y x",
        defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual ForAll(x, Equivalence(φ, Equals(x, z)))
    }
  }
}
