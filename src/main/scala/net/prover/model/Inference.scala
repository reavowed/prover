package net.prover.model

trait Inference extends TheoremLineParser {
  def assumption: Option[Statement]
  def premises: Seq[Statement]
  def conclusion: Statement
  def arbitraryVariables: Seq[TermVariable]
  def distinctVariables: DistinctVariables

  def getSubstitutions(
    targetAssumption: Option[Statement],
    targetPremises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): Substitutions = {
    val assumptionSubstitutionAttempt = assumption match {
      case Some(actualAssumption) =>
        actualAssumption.calculateSubstitutions(targetAssumption.getOrElse(line.throwParseException("No assumption provided for inference")))
      case None =>
        Some(Substitutions.empty)
    }
    val premiseSubstitutionAttempts = premises.zip(targetPremises).map { case (premise, targetPremise) =>
      premise.calculateSubstitutions(targetPremise)
    }
    val initialSubstitutions = Substitutions.mergeAttempts(assumptionSubstitutionAttempt +: premiseSubstitutionAttempts)
      .getOrElse(throw ParseException.withMessage(
        s"Could not match premises\n${targetAssumption.toSeq ++ targetPremises}\n${assumption.toSeq ++ premises}",
        line.fullLine))
    val requiredVariables = (premises.map(_.variables) :+ conclusion.variables).reduce(_ ++ _)
    initialSubstitutions.expand(requiredVariables, line, context)._1
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

  def matchPremises(
    targetAssumption: Option[Statement],
    targetPremises: Seq[Statement],
    line: PartialLine,
    context: Context,
    distinctVariables: DistinctVariables
  ): Inference = {
    val substitutions = getSubstitutions(targetAssumption, targetPremises, line, context)
    val substitutedInference = makeSubstitutions(substitutions)
    val simplifiedInference = substitutedInference.simplify(targetPremises, distinctVariables)
    simplifiedInference
  }

  private def applyWithInference(
    targetInference: Inference,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    targetInference.assumption match {
      case None =>
        ()
      case _ =>
        throw ParseException.withMessage("Cannot apply assumption-discharging inference to another assumption-discharging inference", line.fullLine)
    }
    targetInference.premises match {
      case Seq(_) =>
        ()
      case _ =>
        throw ParseException.withMessage("Can only apply assumption-discharging inference to an inference with a single premise", line.fullLine)
    }
    premises match {
      case Seq(_) =>
        ()
      case _ =>
        throw ParseException.withMessage("Can only apply assumption-discharging inference with a single premise to an inference", line.fullLine)
    }
    val (targetInferencePremise, lineAfterInferencePremise) = Statement.parse(line, context)
    val updatedTargetInference = targetInference.matchPremises(
      None,
      Seq(targetInferencePremise),
      lineAfterInferencePremise,
      context,
      theoremBuilder.distinctVariables)
    val updatedInference = matchPremises(
      Some(updatedTargetInference.premises.head),
      Seq(updatedTargetInference.conclusion),
      lineAfterInferencePremise,
      context,
      theoremBuilder.distinctVariables)
    theoremBuilder.addStep(Step(updatedInference.conclusion, s"$id with ${targetInference.id}"))
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val (targetPremises, lineAfterPremises) = premises.mapFold(line) { (_, lineSoFar) =>
        readReference(lineSoFar, theoremBuilder)
      }
      val updatedRule = matchPremises(
        Some(fantasy.assumption),
        targetPremises,
        lineAfterPremises,
        context,
        theoremBuilder.distinctVariables)
      Step(updatedRule.conclusion, id, Some(Step.Fantasy(fantasy.assumption, fantasy.steps)))
    }
  }

  override def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    assumption match {
      case Some(_) =>
        def withInference = {
          line.splitFirstWord.optionMapLeft(n => context.inferences.find(_.id == n)) map {
            case (inference, restOfLine) =>
              applyWithInference(inference, theoremBuilder, restOfLine, context)
          }
        }
        withInference.getOrElse(applyWithFantasy(theoremBuilder, line, context))
      case None =>
        val (targetPremises, lineAfterPremises) = premises.mapFold(line) { (_, lineSoFar) =>
          readReference(lineSoFar, theoremBuilder)
        }
        val updatedInference = matchPremises(None, targetPremises, lineAfterPremises, context, theoremBuilder.distinctVariables)
        theoremBuilder
          .addStep(Step(updatedInference.conclusion, id))
          .withArbitraryVariables(updatedInference.arbitraryVariables)
          .withDistinctVariables(updatedInference.distinctVariables)
    }
  }
}
