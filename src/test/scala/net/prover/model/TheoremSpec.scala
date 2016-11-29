package net.prover.model

class TheoremSpec extends ProverSpec {
  val restate = DirectRule("restate", Seq(1), 1)
  val introduceImplication = FantasyRule("introduceImplication", 1, Seq(2), Implication(1, 2))
  val eliminateImplication = DirectRule("eliminateImplication", Seq(Implication(1, 2), 1), 2)

  def parseTheorem(
    firstLine: String,
    lines: Seq[String],
    rules: Seq[Rule] = Nil,
    connectives: Seq[Connective] = Nil,
    definitions: Seq[Definition] = Nil,
    theorems: Seq[Theorem] = Nil
  ): Theorem = {
    Theorem.parse(
      firstLine,
      lines,
      defaultContext.copy(rules = rules, connectives = connectives, definitions = definitions, theorems = theorems)
    )._1
  }

  "theorem parse" should {
    "parse a theorem with a fantasy" in {
      val theorem = parseTheorem(
        "imp-refl Implication is Reflexive",
        Seq(
          "assume 1",
          "restate f.h",
          "introduceImplication f.1",
          "qed"),
        Seq(restate, introduceImplication),
        Seq(Implication))
      theorem.result mustEqual Implication(1, 1)
    }

    "parse a theorem with a nested fantasy" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "hypothesis implies 1 implies 2 3",
          "assume implies 1 2",
          "assume 1",
          "eliminateImplication h1 f.f.h",
          "eliminateImplication f.h f.f.h",
          "eliminateImplication f.f.1 f.f.2",
          "introduceImplication f.f.3",
          "introduceImplication f.1",
          "qed"),
        Seq(restate, introduceImplication, eliminateImplication),
        Seq(Implication))
      theorem.result mustEqual Implication(Implication(1, 2), Implication(1, 3))
    }

    "parse a theorem with a definition application" in {
      val theorem = parseTheorem(
        "imp-distr Implication Distributes over Itself",
        Seq(
          "hypothesis and 1 2",
          "definition-and h1",
          "qed"),
        Seq(restate, introduceImplication, eliminateImplication),
        Seq(Negation, Implication, Conjunction),
        Seq(Definition(Conjunction, Negation(Implication(1, Negation(2))))))
      theorem.result mustEqual Negation(Implication(1, Negation(2)))
    }

    "parse a theorem with a previous theorem application" in {
      val previousTheorem = Theorem(
        "false-imp-any",
        "A False Statement Implies Anything",
        Seq(1),
        Nil,
        Implication(Negation(1), 2))

      val theorem = parseTheorem(
        "or-left Disjunction from Left",
        Seq(
          "hypothesis 1",
          "false-imp-any h1 2",
          "definition-or 1",
          "qed"),
        Nil,
        Seq(Negation, Implication, Disjunction),
        Seq(Definition(Disjunction, Implication(Negation(1), 2))),
        Seq(previousTheorem))
      theorem.result mustEqual Disjunction(1, 2)

    }
  }

  "theorem" should {
    "apply to a theorem that matches its hypotheses" in {
      val theorem = Theorem("contra", "Contrapositive", Seq(Implication(1, 2)), Nil, Implication(Negation(2), Negation(1)))
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
        Implication(1, 2))
      val theoremBuilder = TheoremBuilder().addStep(Negation(1))
      val updatedTheoremBuilder = theorem.readAndUpdateTheoremBuilder(theoremBuilder, "1 not 2", defaultContext)
      updatedTheoremBuilder.steps(1).statement mustEqual Implication(1, Negation(2))
    }
  }

}
