package net.prover.model

trait Inference extends TheoremLineParser {
  def assumption: Option[Statement]
  def premises: Seq[Statement]
  def conclusion: Statement
  def arbitraryVariables: Seq[TermVariable]
  def distinctVariables: DistinctVariables

  private def getInitialSubstitutions(
    targetAssumption: Option[Statement],
    targetPremises: Seq[Statement],
    context: Context
  ): Substitutions = {
    val assumptionSubstitutionAttempt = assumption match {
      case Some(actualAssumption) =>
        actualAssumption.calculateSubstitutions(
          targetAssumption.getOrElse(throw new Exception("No assumption provided for inference")))
      case None =>
        Some(Substitutions.empty)
    }
    val premiseSubstitutionAttempts = premises.zip(targetPremises).map { case (premise, targetPremise) =>
      premise.calculateSubstitutions(targetPremise)
    }
    Substitutions.mergeAttempts(assumptionSubstitutionAttempt +: premiseSubstitutionAttempts)
      .getOrElse(throw new Exception(
        s"Could not match premises\n${targetAssumption.toSeq ++ targetPremises}\n${assumption.toSeq ++ premises}"))
  }

  def substitutionsParser(
    targetAssumption: Option[Statement],
    targetPremises: Seq[Statement],
    context: Context
  ): Parser[Substitutions] = {
    val initialSubstitutions = getInitialSubstitutions(targetAssumption, targetPremises, context)
    val requiredVariables = (premises.map(_.variables) :+ conclusion.variables).reduce(_ ++ _)
    initialSubstitutions.expandParser(requiredVariables, context)
  }

  def makeSubstitutions(
    substitutions: Substitutions
  ): Inference = new Inference {
    override val id = Inference.this.id
    override val assumption = Inference.this.assumption.map(_.applySubstitutions(substitutions).asInstanceOf[Statement])
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
      override val assumption = Inference.this.assumption
        .map(_.makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement])
      override val premises = Inference.this.premises
        .map(_.makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement])
      override val conclusion = Inference.this.conclusion
        .makeSimplifications(additionalDistinctVariables).asInstanceOf[Statement]
      override val arbitraryVariables = Inference.this.arbitraryVariables
      override val distinctVariables = Inference.this.distinctVariables ++ additionalDistinctVariables
    }
  }

  private def matchPremises(
    targetAssumption: Option[Statement],
    targetPremises: Seq[Statement],
    context: Context,
    distinctVariables: DistinctVariables
  ): Parser[Inference] = {
    for {
      substitutions <- substitutionsParser(targetAssumption, targetPremises, context)
    } yield {
      val substitutedInference = makeSubstitutions(substitutions)
      val simplifiedInference = substitutedInference.simplify(targetPremises, distinctVariables)
      simplifiedInference
    }
  }

  private def applyWithInference(
    targetInference: Inference,
    theoremBuilder: TheoremBuilder,
    context: Context
  ): Parser[TheoremBuilder] = {
    if (targetInference.assumption.nonEmpty)
        throw new Exception(
          "Cannot apply assumption-discharging inference to another assumption-discharging inference")
    if (targetInference.premises.length != 1)
        throw new Exception(
          "Can only apply assumption-discharging inference to an inference with a single premise")
    if (premises.length != 1)
        throw new Exception(
          "Can only apply assumption-discharging inference with a single premise to an inference")

    for {
      targetInferencePremise <- Statement.parser(context)
      updatedTargetInference <- targetInference.matchPremises(
        None,
        Seq(targetInferencePremise),
        context,
        theoremBuilder.distinctVariables)
      updatedInference <- matchPremises(
        Some(updatedTargetInference.premises.head),
        Seq(updatedTargetInference.conclusion),
        context,
        theoremBuilder.distinctVariables)
    } yield {
      theoremBuilder.addStep(Step(updatedInference.conclusion, s"$id with ${targetInference.id}"))
    }
  }

  private def premisesParser(theoremBuilder: TheoremBuilder): Parser[Seq[Statement]] = {
    premises.map(_ => theoremBuilder.referenceParser).traverseParser
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, context: Context): Parser[TheoremBuilder] = {
    theoremBuilder.replaceFantasy { fantasy =>
      for {
        targetPremises <- premisesParser(theoremBuilder)
        updatedRule <- matchPremises(
          Some(fantasy.assumption),
          targetPremises,
          context,
          theoremBuilder.distinctVariables)
      } yield {
        Step(updatedRule.conclusion, id, Some(Step.Fantasy(fantasy.assumption, fantasy.steps)))
      }
    }
  }

  override def parser(
    theoremBuilder: TheoremBuilder,
    context: Context
  ): Parser[TheoremBuilder] = {
    assumption match {
      case Some(_) =>
        def withInferenceParser = {
          Parser.singleWord.flatMapOption { id =>
            context.inferences
              .find(_.id == id)
              .map(applyWithInference(_, theoremBuilder, context))
          }
        }
        withInferenceParser.orElse(applyWithFantasy(theoremBuilder, context))
      case None =>
        for {
          targetPremises <- premisesParser(theoremBuilder)
          updatedInference <- matchPremises(None, targetPremises, context, theoremBuilder.distinctVariables)
        } yield {
          theoremBuilder
            .addStep(Step(updatedInference.conclusion, id))
            .withArbitraryVariables(updatedInference.arbitraryVariables)
            .withDistinctVariables(updatedInference.distinctVariables)
        }
    }
  }
}
