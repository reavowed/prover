package net.prover.model

abstract class Rule extends ChapterEntry(Rule) with TheoremLineParser

case class ArbitraryVariableException(message: String) extends Exception(message)

case class DirectRule(
    id: String,
    premises: Seq[Statement],
    conclusion: Statement,
    arbitraryVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables)
  extends Rule with Inference

case class FantasyRule(
    id: String,
    assumption: Statement,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {

  def getSubstitutions(
    targetAssumption: Statement,
    targetPremises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): Substitutions = {
    val assumptionSubstitutionAttempt = assumption.calculateSubstitutions(targetAssumption)
    val premiseSubstitutionAttempts = premises.zip(targetPremises).map { case (premise, targetPremise) =>
      premise.calculateSubstitutions(targetPremise)
    }
    val initialSubstitutions = Substitutions.mergeAttempts(assumptionSubstitutionAttempt +: premiseSubstitutionAttempts)
      .getOrElse(throw ParseException.withMessage(
        s"Could not match premises\n$targetPremises\n$premises",
        line.fullLine))
    val requiredVariables = (premises.map(_.variables) :+ conclusion.variables).reduce(_ ++ _)
    initialSubstitutions.expand(requiredVariables, line, context)._1
  }

  def makeSubstitutions(substitutions: Substitutions): FantasyRule = FantasyRule(
    id,
    assumption.applySubstitutions(substitutions).asInstanceOf[Statement],
    premises.map(_.applySubstitutions(substitutions).asInstanceOf[Statement]),
    conclusion.applySubstitutions(substitutions).asInstanceOf[Statement])

  def simplify(targetAssumption: Statement, targetPremises: Seq[Statement]): FantasyRule = {
    val distinctVariableAttempts = assumption.attemptSimplification(targetAssumption) +:
      premises.zip(targetPremises).map {
        case (premise, targetPremise) =>
          premise.attemptSimplification(targetPremise)
      }
    val distinctVariables = distinctVariableAttempts.traverseOption
      .getOrElse(throw new Exception("Unexpected error resolving simplifications")).reduce(_ ++ _)
    FantasyRule(
      id,
      targetAssumption.makeSimplifications(distinctVariables).asInstanceOf[Statement],
      premises.map(_.makeSimplifications(distinctVariables).asInstanceOf[Statement]),
      conclusion.makeSimplifications(distinctVariables).asInstanceOf[Statement])
  }

  def matchAssumptionAndPremises(
    targetAssumption: Statement,
    targetPremises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): FantasyRule = {
    val substitutions = getSubstitutions(targetAssumption, targetPremises, line, context)
    makeSubstitutions(substitutions).simplify(targetAssumption, targetPremises)
  }

  private def applyWithInference(
    inference: Inference,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    inference.premises match {
      case Seq(_) =>
        ()
      case _ =>
        throw ParseException.withMessage("Can only apply rule to a inference with a single premise", line.fullLine)
    }
    premises match {
      case Seq(_) =>
        ()
      case _ =>
        throw ParseException.withMessage("Can only apply a rule with a single premise to an inference", line.fullLine)
    }
    val (targetInferencePremise, lineAfterInferencePremise) = Statement.parse(line, context)
    val updatedInference = inference.matchPremises(
      Seq(targetInferencePremise),
      lineAfterInferencePremise,
      context,
      theoremBuilder.distinctVariables)
    val updatedRule = matchAssumptionAndPremises(
      updatedInference.premises.head,
      Seq(updatedInference.conclusion),
      lineAfterInferencePremise,
      context)
    theoremBuilder.addStep(Step(updatedRule.conclusion, s"$id with ${inference.id}"))
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val (targetPremises, lineAfterPremises) = premises.mapFold(line) { (_, lineSoFar) =>
        readReference(lineSoFar, theoremBuilder)
      }
      val updatedRule = matchAssumptionAndPremises(
        fantasy.assumption,
        targetPremises,
        lineAfterPremises,
        context
      )
      Step(updatedRule.conclusion, id, Some(Step.Fantasy(fantasy.assumption, fantasy.steps)))
    }
  }

  override def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    def withInference = {
      line.splitFirstWord.optionMapLeft(n => context.inferences.find(_.id == n)) map {
        case (inference, restOfLine) =>
          applyWithInference(inference, theoremBuilder, restOfLine, context)
      }
    }
    withInference.getOrElse(applyWithFantasy(theoremBuilder, line, context))
  }
}

object Rule extends SingleLineChapterEntryParser[Rule] {
  override val name: String = "rule"

  override def parse(line: PartialLine, context: Context): Rule = {
    val (id, lineAfterName) = line.splitFirstWord
    val (assumptionOrPremises, lineAfterAssumptionOrPremises) = Statement.parseList(lineAfterName, context)
    val (firstSymbol, lineAfterFirstSymbol) = lineAfterAssumptionOrPremises.splitFirstWord
    firstSymbol match {
      case "⇒" =>
        val premises = assumptionOrPremises
        val (conclusion, lineAfterConclusion) = Statement.parse(lineAfterFirstSymbol, context)
        val (arbitraryVariables, lineAfterArbitraryVariables) = lineAfterConclusion match {
          case WordAndRemainingText("|", lineAfterPipe) =>
            Term.parseList(lineAfterPipe, context).mapLeft(_.map(Term.asVariable))
          case _ =>
            (Nil, lineAfterConclusion)
        }
        val (distinctVariables, _) = lineAfterArbitraryVariables match {
          case WordAndRemainingText("x", lineAfterCross) =>
            val (distinctVariable, lineAfterDistinctVariable) = Term.parse(lineAfterCross, context)
              .mapLeft(Term.asVariable)
            val (distinctStatement, lineAfterDistinctStatement) = Statement.parseStatementVariable(lineAfterDistinctVariable, context)
            (
              DistinctVariables(
                Map(distinctVariable -> Variables(Seq(distinctStatement), Nil))),
              lineAfterDistinctStatement)
          case _ =>
            (DistinctVariables.empty, lineAfterArbitraryVariables)
        }
        DirectRule(id, premises, conclusion, arbitraryVariables, distinctVariables)
      case "⊢" =>
        val assumption = assumptionOrPremises match {
          case Seq(singleAssumption) =>
            singleAssumption
          case _ =>
            throw ParseException.withMessage("A rule cannot discharge more than one assumption", line.fullLine)
        }
        val (premises, textAfterPremises) = Statement.parseList(lineAfterFirstSymbol, context)
        val (secondSymbol, conclusionText) = textAfterPremises.splitFirstWord
        if (secondSymbol != "⇒") {
          throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
        }
        val (conclusion, _) = Statement.parse(conclusionText, context)
        FantasyRule(id, assumption, premises, conclusion)
      case _ =>
        throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
    }
  }
  override def addToContext(rule: Rule, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ rule)
  }
}
