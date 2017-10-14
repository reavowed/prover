package net.prover.model.proof

case class ReferencedFact(fact: Fact, reference: Reference.ToFact) {
  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    ReferencedFact(fact.increaseDepth(additionalDepth, insertionPoint), reference)
  }
  def childDetails: Option[(ReferencedFact, Int, ReferencedFact => ReferencedFact)] = {
    for {
      (childFact, level, childUpdater) <- fact.childDetails
    } yield (copy(fact = childFact), level, { f: ReferencedFact => f.copy(fact = childUpdater(f.fact)) })
  }
}
