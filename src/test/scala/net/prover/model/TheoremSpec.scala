package net.prover.model

class TheoremSpec extends ProverSpec {
  def parseTheorem(
    firstLine: String,
    lines: Seq[String],
    additionalTheorems: Seq[Theorem] = Nil
  ): Theorem = {
    Theorem.parser(
      lines,
      defaultContext.copy(
        theoremLineParsers = defaultContext.theoremLineParsers ++
          Seq(IntroduceImplication, EliminateImplication, IntroduceForall, EliminateForall) ++
          additionalTheorems)
    ).parseAndDiscard(firstLine)._1
  }

  "theorem parse" should {
    "parse a theorem with a fantasy" in {
      val theorem = parseTheorem(
        "imp-refl Implication is Reflexive",
        Seq(
          "assume φ",
          "introduceImplication f.a",
          "qed"))
      theorem.conclusion mustEqual Implication(φ, φ)
    }

    "parse a theorem with a nested fantasy" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "premise → φ → ψ χ",
          "assume → φ ψ",
          "assume φ",
          "eliminateImplication p1 f.f.a",
          "eliminateImplication f.a f.f.a",
          "eliminateImplication f.f.1 f.f.2",
          "introduceImplication f.f.3",
          "introduceImplication f.1",
          "qed"))
      theorem.conclusion mustEqual Implication(Implication(φ, ψ), Implication(φ, χ))
    }

    "parse a theorem with a definition application" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "premise ∧ φ ψ",
          "unapply-∧ p1",
          "qed"))
      theorem.conclusion mustEqual Negation(Implication(φ, Negation(ψ)))
    }

    "parse a theorem with a previous theorem application" in {
      val previousTheorem = Theorem(
        "false-imp-any",
        "A False Statement Implies Anything",
        Seq(φ),
        Nil,
        Implication(Negation(φ), ψ),
        Nil,
        DistinctVariables.empty)

      val theorem = parseTheorem(
        "or-left Disjunction from Left",
        Seq(
          "premise φ",
          "false-imp-any p1 ψ",
          "apply-∨ 1",
          "qed"),
        additionalTheorems = Seq(previousTheorem))
      theorem.conclusion mustEqual Disjunction(φ, ψ)
    }

    "parse a theorem with arbitrary variables" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise = x y",
          "introduceForall p1 = x z y z",
          "qed"))
      theorem.arbitraryVariables mustEqual Seq(y)
    }

    "parse a theorem with distinct variables" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise sub y x φ",
          "introduceForall p1",
          "qed"))
      theorem.distinctVariables mustEqual
        DistinctVariables(Map(y -> Variables(Seq(φ), Nil)))
    }

    "not include distinct variables if they don't appear in the hypotheses or conclusion" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise ∀ x → φ ψ",
          "eliminateForall p1 y",
          "assume ∀ x φ",
          "eliminateForall f.a y",
          "eliminateImplication 1 f.1",
          "introduceForall f.2",
          "introduceImplication f.3",
          "qed"))
      theorem.distinctVariables mustEqual DistinctVariables.empty
    }

    "not include arbitrary variables if they don't appear in the hypotheses" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise ∀ x φ",
          "eliminateForall p1 y",
          "introduceForall 1",
          "qed"))
      theorem.arbitraryVariables mustEqual Nil
    }
  }

  "theorem" should {
    "apply to a theorem that matches its hypotheses" in {
      val theorem = Theorem(
        "contra",
        "Contrapositive",
        Seq(Implication(φ, ψ)),
        Nil,
        Implication(Negation(ψ), Negation(φ)),
        Nil,
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addStep(Implication(φ, ψ))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "1", defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual Implication(Negation(ψ), Negation(φ))
    }

    "apply to a theorem that matches its hypotheses with a free statement variable" in {
      val theorem = Theorem(
        "false-imp-all",
        "A False Statement Implies Everything",
        Seq(Negation(φ)),
        Nil,
        Implication(φ, ψ),
        Nil,
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addStep(Negation(φ))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "1 ¬ ψ", defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual Implication(φ, Negation(ψ))
    }

    "add arbitrary variables to applied theorem" in {
      val theorem = Theorem(
        "id",
        "Title",
        Seq(Equals(x, y)),
        Nil,
        Equals(y, z),
        Seq(x),
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addPremise(Equals(y, x))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "p1 z", defaultContext)
      updatedTheoremBuilder.arbitraryVariables mustEqual Seq(y)
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy assumption" in {
      val theorem = Theorem(
        "id",
        "Title",
        Seq(Equals(y, x)),
        Nil,
        Equals(y, z),
        Seq(x),
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addFantasy(Equals(x, y))
      theorem.readAndUpdateTheoremBuilder(theoremBuilder, "f.a z", defaultContext) must throwAn[ArbitraryVariableException]

    }
  }

}
