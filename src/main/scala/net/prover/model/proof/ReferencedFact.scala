package net.prover.model.proof

case class ReferencedFact(fact: Fact, reference: Reference.ToFact) {
  def increaseDepth(additionalDepth: Int) = ReferencedFact(fact.increaseDepth(additionalDepth), reference)
}
