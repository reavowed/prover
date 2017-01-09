package net.prover.model

trait Deduction extends TheoremLineParser {
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
    substitutions: Substitutions): Deduction = new Deduction {
    override val id = Deduction.this.id
    override val premiseTemplates = Deduction.this.premiseTemplates.map(_.applySubstitutions(substitutions))
    override val conclusionTemplate = Deduction.this.conclusionTemplate.applySubstitutions(substitutions)
    override val arbitraryVariables = Deduction.this.arbitraryVariables
      .map(_.applySubstitutions(substitutions))
      .map(Term.asVariable)
    override val distinctVariables = Deduction.this.distinctVariables.applySubstitutions(substitutions)
  }

  def simplify(premises: Seq[Statement]): Deduction = {
    val additionalDistinctVariables = premises.zip(premiseTemplates).map {
      case (premise, premiseTemplate) =>
        premiseTemplate.attemptSimplification(premise)
    }
      .traverseOption
      .getOrElse(throw new Exception(s"Could not match premises\n$premises\n$premiseTemplates"))
      .foldLeft(DistinctVariables.empty)(_ ++ _)
    new Deduction {
      override val id = Deduction.this.id
      override val premiseTemplates = Deduction.this.premiseTemplates
        .map(_.makeSimplifications(additionalDistinctVariables))
      override val conclusionTemplate = Deduction.this.conclusionTemplate
        .makeSimplifications(additionalDistinctVariables)
      override val arbitraryVariables = Deduction.this.arbitraryVariables
      override val distinctVariables = Deduction.this.distinctVariables ++ additionalDistinctVariables
    }
  }

  def matchPremises(premises: Seq[Statement], line: PartialLine, context: Context): Deduction = {
    val substitutions = getSubstitutions(premises, line, context)
    val substitutedDeduction = makeSubstitutions(substitutions)
    val simplifiedDeduction = substitutedDeduction.simplify(premises)
    simplifiedDeduction
  }

  override def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val (premises, lineAfterPremises) = premiseTemplates.mapFold(line) { (premiseTemplate, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder)
    }
    val updatedDeduction = matchPremises(premises, lineAfterPremises, context)
    theoremBuilder
      .addStep(Step(updatedDeduction.conclusionTemplate))
      .withArbitraryVariables(updatedDeduction.arbitraryVariables)
      .withDistinctVariables(updatedDeduction.distinctVariables)
  }
}