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
}
