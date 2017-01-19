package net.prover.model

class TheoremSpec extends ProverSpec {
  def parseTheorem(
    firstLine: String,
    lines: Seq[String],
    additionalTheorems: Seq[Theorem] = Nil
  ): Theorem = {
    Theorem.parse(
      firstLine,
      lines,
      defaultContext.copy(
        theoremLineParsers = defaultContext.theoremLineParsers ++
          Seq(IntroduceImplication, EliminateImplication, IntroduceForall, EliminateForall) ++
          additionalTheorems)
    )._1
  }

  "theorem parse" should {
    "parse a theorem with a fantasy" in {
      val theorem = parseTheorem(
        "imp-refl Implication is Reflexive",
        Seq(
          "assume 1",
          "introduceImplication f.a",
          "qed"))
      theorem.conclusionTemplate mustEqual Implication(1, 1)
    }

    "parse a theorem with a nested fantasy" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "premise → 1 → 2 3",
          "assume → 1 2",
          "assume 1",
          "eliminateImplication p1 f.f.a",
          "eliminateImplication f.a f.f.a",
          "eliminateImplication f.f.1 f.f.2",
          "introduceImplication f.f.3",
          "introduceImplication f.1",
          "qed"))
      theorem.conclusionTemplate mustEqual Implication(Implication(1, 2), Implication(1, 3))
    }

    "parse a theorem with a definition application" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "premise ∧ 1 2",
          "unapply-∧ p1",
          "qed"))
      theorem.conclusionTemplate mustEqual Negation(Implication(1, Negation(2)))
    }

    "parse a theorem with a previous theorem application" in {
      val previousTheorem = Theorem(
        "false-imp-any",
        "A False Statement Implies Anything",
        Seq(1),
        Nil,
        Implication(Negation(1), 2),
        Nil,
        DistinctVariables.empty)

      val theorem = parseTheorem(
        "or-left Disjunction from Left",
        Seq(
          "premise 1",
          "false-imp-any p1 2",
          "apply-∨ 1",
          "qed"),
        additionalTheorems = Seq(previousTheorem))
      theorem.conclusionTemplate mustEqual Disjunction(1, 2)
    }

    "parse a theorem with arbitrary variables" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise = 1 2",
          "introduceForall p1 = 1 3 2 3",
          "qed"))
      theorem.arbitraryVariables mustEqual Seq(TermVariable("y"))
    }

    "parse a theorem with distinct variables" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise sub 2 1 1",
          "introduceForall p1",
          "qed"))
      theorem.distinctVariables mustEqual
        DistinctVariables(Map(TermVariable("y") -> Variables(Seq(1), Nil)))
    }

    "not include distinct variables if they don't appear in the hypotheses or conclusion" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "premise ∀ 1 → 1 2",
          "eliminateForall p1 2",
          "assume ∀ 1 1",
          "eliminateForall f.a 2",
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
          "premise ∀ 1 1",
          "eliminateForall p1 2",
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
        Seq(Implication(1, 2)),
        Nil,
        Implication(Negation(2), Negation(1)),
        Nil,
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addStep(Implication(1, 2))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "1", defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual Implication(Negation(2), Negation(1))
    }

    "apply to a theorem that matches its hypotheses with a free statement variable" in {
      val theorem = Theorem(
        "false-imp-all",
        "A False Statement Implies Everything",
        Seq(Negation(1)),
        Nil,
        Implication(1, 2),
        Nil,
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addStep(Negation(1))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "1 ¬ 2", defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual Implication(1, Negation(2))
    }

    "add arbitrary variables to applied theorem" in {
      val theorem = Theorem(
        "id",
        "Title",
        Seq(Equals("z", "y")),
        Nil,
        Equals("y", "x"),
        Seq("z"),
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addPremise(Equals("y", "z"))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "p1 4", defaultContext)
      updatedTheoremBuilder.arbitraryVariables mustEqual Seq(TermVariable("y"))
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy assumption" in {
      val theorem = Theorem(
        "id",
        "Title",
        Seq(Equals("z", "y")),
        Nil,
        Equals("y", "x"),
        Seq("z"),
        DistinctVariables.empty)
      val theoremBuilder = TheoremBuilder().addFantasy(Equals("y", "z"))
      theorem.readAndUpdateTheoremBuilder(theoremBuilder, "f.a 4", defaultContext) must throwAn[ArbitraryVariableException]

    }
  }

}
