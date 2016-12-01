package net.prover.model

class TheoremSpec extends ProverSpec {
  val introduceImplication = FantasyRule("introduceImplication", 1, Seq(2), Implication(1, 2))
  val eliminateImplication = DirectRule("eliminateImplication", Seq(Implication(1, 2), 1), 2, Nil)
  val introduceForall = DirectRule(
    "introduceForall",
    Seq(StatementVariableWithReplacement(1, 2, 1)),
    ForAll(1, 1),
    Seq(2))
  val eliminateForAll = DirectRule(
    "eliminateForAll",
    Seq(ForAll(1, 1)),
    StatementVariableWithReplacement(1, 2, 1),
    Nil)

  def parseTheorem(
    firstLine: String,
    lines: Seq[String],
    additionalTheorems: Seq[Theorem] = Nil
  ): Theorem = {
    Theorem.parse(
      firstLine,
      lines,
      defaultContext.copy(
        rules = defaultContext.rules ++ Seq(introduceImplication, eliminateImplication, introduceForall, eliminateForAll),
        theorems = defaultContext.theorems ++ additionalTheorems)
    )._1
  }

  "theorem parse" should {
    "parse a theorem with a fantasy" in {
      val theorem = parseTheorem(
        "imp-refl Implication is Reflexive",
        Seq(
          "assume 1",
          "introduceImplication f.h",
          "qed"))
      theorem.result mustEqual Implication(1, 1)
    }

    "parse a theorem with a nested fantasy" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "hypothesis → 1 → 2 3",
          "assume → 1 2",
          "assume 1",
          "eliminateImplication h1 f.f.h",
          "eliminateImplication f.h f.f.h",
          "eliminateImplication f.f.1 f.f.2",
          "introduceImplication f.f.3",
          "introduceImplication f.1",
          "qed"))
      theorem.result mustEqual Implication(Implication(1, 2), Implication(1, 3))
    }

    "parse a theorem with a definition application" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "hypothesis ∧ 1 2",
          "definition-∧ h1",
          "qed"))
      theorem.result mustEqual Negation(Implication(1, Negation(2)))
    }

    "parse a theorem with a previous theorem application" in {
      val previousTheorem = Theorem(
        "false-imp-any",
        "A False Statement Implies Anything",
        Seq(1),
        Nil,
        Implication(Negation(1), 2),
        Nil)

      val theorem = parseTheorem(
        "or-left Disjunction from Left",
        Seq(
          "hypothesis 1",
          "false-imp-any h1 2",
          "definition-∨ 1",
          "qed"),
        additionalTheorems = Seq(previousTheorem))
      theorem.result mustEqual Disjunction(1, 2)
    }

    "parse a theorem with arbitrary variables" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "hypothesis = 1 2",
          "introduceForall h1 = 1 3 2 3",
          "qed"))
      theorem.arbitraryVariables mustEqual Seq(TermVariable(2))
    }

    "not include arbitrary variables if they don't appear in the hypotheses" in {
      val theorem = parseTheorem(
        "id Title",
        Seq(
          "hypothesis ∀ 1 1",
          "eliminateForAll h1 2",
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
        Nil)
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
        Nil)
      val theoremBuilder = TheoremBuilder().addStep(Negation(1))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "1 ¬ 2", defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual Implication(1, Negation(2))
    }

    "add arbitrary variables to applied theorem" in {
      val theorem = Theorem(
        "id",
        "Title",
        Seq(Equals(1, 2)),
        Nil,
        Equals(2, 3),
        Seq(1))
      val theoremBuilder = TheoremBuilder().addHypothesis(Equals(2, 1))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "h1 4", defaultContext)
      updatedTheoremBuilder.arbitraryVariables mustEqual Seq(TermVariable(2))
    }

    "fail a rule with arbitrary variables if an arbitrary variable appears in a fantasy hypothesis" in {
      val theorem = Theorem(
        "id",
        "Title",
        Seq(Equals(1, 2)),
        Nil,
        Equals(2, 3),
        Seq(1))
      val theoremBuilder = TheoremBuilder().addFantasy(Equals(2, 1))
      theorem.readAndUpdateTheoremBuilder(theoremBuilder, "f.h 4", defaultContext) must throwAn[ArbitraryVariableException]

    }
  }

}
