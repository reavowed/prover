package net.prover.model

trait Inference extends TheoremLineParser {
  def premiseTemplates: Seq[Statement]
  def conclusionTemplate: Statement
  def arbitraryVariables: Seq[TermVariable]
  def distinctVariables: DistinctVariables

  def getSubstitutions(
    premises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): Substitutions = {
    val premiseSubstitutionAttempts = premises.zip(premiseTemplates).map { case (premise, premiseTemplate) =>
      premiseTemplate.calculateSubstitutions(premise)
    }
    val initialSubstitutions = Substitutions.mergeAttempts(premiseSubstitutionAttempts)
      .getOrElse(throw ParseException.withMessage(
        s"Could not match premises\n$premises\n$premiseTemplates",
        line.fullLine))
    val requiredVariables = (premiseTemplates.map(_.variables) :+ conclusionTemplate.variables).reduce(_ ++ _)
    initialSubstitutions.expand(requiredVariables, line, context)._1
  }

  def makeSubstitutions(
    substitutions: Substitutions): Inference = new Inference {
    override val id = Inference.this.id
    override val premiseTemplates = Inference.this.premiseTemplates.map(_.applySubstitutions(substitutions).asInstanceOf[Statement])
    override val conclusionTemplate = Inference.this.conclusionTemplate.applySubstitutions(substitutions).asInstanceOf[Statement]
    override val arbitraryVariables = Inference.this.arbitraryVariables
      .map(_.applySubstitutions(substitutions))
      .map(Term.asVariable)
    override val distinctVariables = Inference.this.distinctVariables.applySubstitutions(substitutions)
  }

  def simplify(premises: Seq[Statement], distinctVariables: DistinctVariables): Inference = {
    val additionalDistinctVariables = premises.zip(premiseTemplates).map {
      case (premise, premiseTemplate) =>
        premiseTemplate.attemptSimplification(premise)
    }
      .traverseOption
      .getOrElse(throw new Exception(s"Could not match premises\n$premises\n$premiseTemplates"))
      .foldLeft(distinctVariables)(_ ++ _)
    new Inference {
      override val id = Inference.this.id
      override val premiseTemplates = Inference.this.premiseTemplates
        .map(_.makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement])
      override val conclusionTemplate = Inference.this.conclusionTemplate
        .makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement]
      override val arbitraryVariables = Inference.this.arbitraryVariables
      override val distinctVariables = Inference.this.distinctVariables ++ additionalDistinctVariables
    }
  }

  def matchPremises(premises: Seq[Statement], line: PartialLine, context: Context, distinctVariables: DistinctVariables): Inference = {
    val substitutions = getSubstitutions(premises, line, context)
    val substitutedInference = makeSubstitutions(substitutions)
    val simplifiedInference = substitutedInference.simplify(premises, distinctVariables)
    simplifiedInference
  }

  override def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val (premises, lineAfterPremises) = premiseTemplates.mapFold(line) { (premiseTemplate, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder)
    }
    val updatedInference = matchPremises(premises, lineAfterPremises, context, theoremBuilder.distinctVariables)
    theoremBuilder
      .addStep(Step(updatedInference.conclusionTemplate, id))
      .withArbitraryVariables(updatedInference.arbitraryVariables)
      .withDistinctVariables(updatedInference.distinctVariables)
  }
}
