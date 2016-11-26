package net.prover.model

class DefinitionSpec extends ProverSpec {
  "definition parser" should {
    "parse a definition" in {
      Definition.parse(
        "and not implies 1 not 2",
        Book("", connectives = Seq(Negation, Implication, Conjunction))
      ) mustEqual Definition(Conjunction, Negation(Implication(1, Negation(2))))
    }
  }

  "definition" should {
    "apply to a theorem with the defined connective" in {
      val definition = Definition(Conjunction, Negation(Implication(1, Negation(2))))
      val theorem = TheoremBuilder().addStep(Conjunction(Implication(1, 2), 3))
      val updatedTheorem = definition.applyToTheorem(theorem, "1", Book(""))
      updatedTheorem.steps(1).statement mustEqual Negation(Implication(Implication(1, 2), Negation(3)))
    }
    "apply to a theorem with the defining statement connective" in {
      val definition = Definition(Conjunction, Negation(Implication(1, Negation(2))))
      val theorem = TheoremBuilder().addStep(Negation(Implication(Implication(1, 2), Negation(3))))
      val updatedTheorem = definition.applyToTheorem(theorem, "1", Book(""))
      updatedTheorem.steps(1).statement mustEqual Conjunction(Implication(1, 2), 3)
    }
  }
}
