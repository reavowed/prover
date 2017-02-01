package net.prover.model

trait Inference extends TheoremLineParser {
  def premises: Seq[Statement]
  def conclusion: Statement
  def arbitraryVariables: Seq[TermVariable]
  def distinctVariables: DistinctVariables

  def getSubstitutions(
    targetPremises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): Substitutions = {
    val premiseSubstitutionAttempts = premises.zip(targetPremises).map { case (premise, targetPremise) =>
      premise.calculateSubstitutions(targetPremise)
    }
    val initialSubstitutions = Substitutions.mergeAttempts(premiseSubstitutionAttempts)
      .getOrElse(throw ParseException.withMessage(
        s"Could not match premises\n$targetPremises\n$premises",
        line.fullLine))
    val requiredVariables = (premises.map(_.variables) :+ conclusion.variables).reduce(_ ++ _)
    initialSubstitutions.expand(requiredVariables, line, context)._1
  }

  def makeSubstitutions(
    substitutions: Substitutions
  ): Inference = new Inference {
    override val id = Inference.this.id
    override val premises = Inference.this.premises.map(_.applySubstitutions(substitutions).asInstanceOf[Statement])
    override val conclusion = Inference.this.conclusion.applySubstitutions(substitutions).asInstanceOf[Statement]
    override val arbitraryVariables = Inference.this.arbitraryVariables
      .map(_.applySubstitutions(substitutions))
      .map(Term.asVariable)
    override val distinctVariables = Inference.this.distinctVariables.applySubstitutions(substitutions)
  }

  def simplify(targetPremises: Seq[Statement], distinctVariables: DistinctVariables): Inference = {
    val additionalDistinctVariables = premises.zip(targetPremises)
      .map {
        case (premise, targetPremise) =>
          premise.attemptSimplification(targetPremise)
      }
      .traverseOption
      .getOrElse(throw new Exception(s"Could not match premises\n$targetPremises\n$premises"))
      .foldLeft(distinctVariables)(_ ++ _)
    new Inference {
      override val id = Inference.this.id
      override val premises = Inference.this.premises
        .map(_.makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement])
      override val conclusion = Inference.this.conclusion
        .makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement]
      override val arbitraryVariables = Inference.this.arbitraryVariables
      override val distinctVariables = Inference.this.distinctVariables ++ additionalDistinctVariables
    }
  }

  def matchPremises(targetPremises: Seq[Statement], line: PartialLine, context: Context, distinctVariables: DistinctVariables): Inference = {
    val substitutions = getSubstitutions(targetPremises, line, context)
    val substitutedInference = makeSubstitutions(substitutions)
    val simplifiedInference = substitutedInference.simplify(targetPremises, distinctVariables)
    simplifiedInference
  }

  override def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val (targetPremises, lineAfterPremises) = premises.mapFold(line) { (premise, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder)
    }
    val updatedInference = matchPremises(targetPremises, lineAfterPremises, context, theoremBuilder.distinctVariables)
    theoremBuilder
      .addStep(Step(updatedInference.conclusion, id))
      .withArbitraryVariables(updatedInference.arbitraryVariables)
      .withDistinctVariables(updatedInference.distinctVariables)
  }
}
