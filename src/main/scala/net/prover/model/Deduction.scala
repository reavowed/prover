package net.prover.model

trait Deduction extends TheoremLineParser {
  def premiseTemplates: Seq[Statement]
  def conclusionTemplate: Statement
  def arbitraryVariables: Seq[TermVariable]

  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (premisesAndTemplates, lineAfterPremises) = premiseTemplates.mapFold(line) { (premiseTemplate, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder).mapLeft((_, premiseTemplate))
    }
    val (matcher, _) = matchPremises(premisesAndTemplates, conclusionTemplate, lineAfterPremises, context)
    val updatedArbitraryVariables = arbitraryVariables.flatMap(matcher.terms.get).map(Term.asVariable)
    theoremBuilder
      .addStep(Step(conclusionTemplate.applyMatch(matcher)))
      .withArbitraryVariables(updatedArbitraryVariables)
  }

}
