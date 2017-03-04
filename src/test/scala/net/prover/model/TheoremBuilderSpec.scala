package net.prover.model

class TheoremBuilderSpec extends ProverSpec {

  "theorem builder" should {
    "handle simple direct rule" in {
      val updatedTheoremBuilder = Restate
        .parser(TheoremBuilder().addPremise(φ))
        .parseAndDiscard("p1")
      updatedTheoremBuilder.steps.head.statement mustEqual φ
    }

    "handle direct rule referencing fantasy hypothesis" in {
      val updatedTheoremBuilder = Restate
        .parser(TheoremBuilder().addFantasy(φ))
        .parseAndDiscard("f.a")
      updatedTheoremBuilder.fantasyOption.get.steps.head.statement mustEqual φ
    }

    "handle simple direct rule with a more complicated match" in {
      val updatedTheoremBuilder = Restate
        .parser(TheoremBuilder().addPremise(Conjunction(φ, ψ)))
        .parseAndDiscard("p1")
      updatedTheoremBuilder.steps.head.statement mustEqual Conjunction(φ, ψ)
    }

    "fail an assumption discharging rule when theorem builder has no fantasy" in {
      IntroduceImplication
        .parser(TheoremBuilder())
        .parseAndDiscard("") must throwAn[Exception]
    }

    "handle assumption discharging rule with premise" in {
      val updatedTheoremBuilder = IntroduceImplication
        .parser(TheoremBuilder().addFantasy(φ).addStep(ψ))
        .parseAndDiscard("f.1")
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
      val theoremBuilder = IntroduceImplication
        .parser(
          TheoremBuilder())(
          implicitly[Context].copy(theoremLineParsers = Seq(theorem)))
        .parseAndDiscard("and-sym ∧ ψ φ")
      theoremBuilder.steps.head.statement mustEqual Implication(Conjunction(ψ, φ), Conjunction(φ, ψ))
    }

    "handle assumption discharging rule applied to definition" in {
      val theoremBuilder = IntroduceImplication
        .parser(TheoremBuilder())
        .parseAndDiscard("unapply-∨ ∨ ¬ φ ψ")
      theoremBuilder.steps.head.statement mustEqual Implication(
        Disjunction(Negation(φ), ψ),
        Implication(Negation(Negation(φ)), ψ))
    }

    "handle rule with a substitution in the conclusion" in {
      val theoremBuilder = EliminateForall
        .parser(TheoremBuilder().addStep(ForAll(y, Equals(y, x))))
        .parseAndDiscard("1 z")
      theoremBuilder.steps(1).statement mustEqual Equals(z, x)
    }

    "handle rule with a substitution in the premise applied to a non-substituted statement" in {
      val theoremBuilder = IntroduceForall
        .parser(TheoremBuilder().addStep(Conjunction(Equals(x, x), ElementOf(y, x))))
        .parseAndDiscard("1 ∧ = z z ∈ y z x z")
      theoremBuilder.steps(1).statement mustEqual ForAll(z, Conjunction(Equals(z, z), ElementOf(y, z)))
    }

    "handle rule with a substitution in the premise applied to a substituted statement" in {
      val theoremBuilder = IntroduceForall
        .parser(TheoremBuilder().addStep(StatementVariableWithReplacement(φ, y, x)))
        .parseAndDiscard("1")
      theoremBuilder.steps(1).statement mustEqual ForAll(x, φ)
    }

    "add a rule's arbitrary variable to the theorem if it appears in a theorem premise" in {
      val theoremBuilder = IntroduceForall
        .parser(TheoremBuilder().addPremise(Equals(x, y)))
        .parseAndDiscard("p1 = x z y z")
      theoremBuilder.arbitraryVariables mustEqual Seq(y)
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy assumption" in {
      IntroduceForall
        .parser(TheoremBuilder().addFantasy(Equals(x, y)))
        .parseAndDiscard("f.a = x z y z") must throwAn[ArbitraryVariableException]
    }

    "disallow a rule if distinct variable requirements are not followed" in {
      IntroduceForall
        .parser(TheoremBuilder().addStep(Equals(z, z)))
        .parseAndDiscard("1 = y z z y") must throwA[DistinctVariableViolationException]
    }

    "apply a rule by adding distinct variables if necessary" in {
      val updatedTheoremBuilder = IntroduceForall
        .parser(TheoremBuilder().addStep(Equivalence(StatementVariableWithReplacement(φ, y, x), Equals(y, z))))
        .parseAndDiscard("1 ↔ φ = x z y x")
      updatedTheoremBuilder.steps(1).statement mustEqual ForAll(x, Equivalence(φ, Equals(x, z)))
    }
  }
}
