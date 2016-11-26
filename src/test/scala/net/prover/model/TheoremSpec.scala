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
    definitions: Seq[Definition] = Nil
  ): Theorem = {
    Theorem.parse(
      firstLine,
      lines,
      Book("", rules = rules, connectives = connectives, definitions = definitions)
    )._1
  }

  "theorem parse" should {
    "parse a theorem with a fantasy" in {
      val theorem = parseTheorem(
        "imp-refl Implication is Reflexive",
        Seq(
          "assume 1",
          "restate f.h",
          "introduceImplication 1",
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
          "introduceImplication 3",
          "introduceImplication 1",
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
  }

}
