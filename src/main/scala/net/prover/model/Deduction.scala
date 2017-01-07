package net.prover.model

trait Deduction extends TheoremLineParser {
  def premiseTemplates: Seq[Statement]
  def conclusionTemplate: Statement
  def arbitraryVariables: Seq[TermVariable]
  def distinctVariables: DistinctVariables

  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (premisesAndTemplates, lineAfterPremises) = premiseTemplates.mapFold(line) { (premiseTemplate, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder).mapLeft((_, premiseTemplate))
    }
    val (matcher, _) = matchPremises(
      premisesAndTemplates,
      conclusionTemplate,
      theoremBuilder.distinctVariables,
      lineAfterPremises,
      context)
    val updatedArbitraryVariables = arbitraryVariables.flatMap(matcher.terms.get).map(Term.asVariable)
    val updatedDistinctVariables = distinctVariables.applyMatch(matcher, theoremBuilder.distinctVariables)
    theoremBuilder
      .addStep(Step(conclusionTemplate.applyMatch(matcher, theoremBuilder.distinctVariables)))
      .withArbitraryVariables(updatedArbitraryVariables)
      .withDistinctVariables(updatedDistinctVariables)
  }

}
