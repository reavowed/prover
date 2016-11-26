package net.prover.model

import org.specs2.mutable.Specification

trait ProverSpec extends Specification {
  val Implication = Connective("implies", "→", 2)
  val Negation = Connective("not", "¬", 1)
  val Conjunction = Connective("and", "∧", 2)

  implicit def intToAtom(i: Int): Atom = Atom(i)
}
