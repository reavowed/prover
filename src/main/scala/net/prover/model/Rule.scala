package net.prover.model

abstract class Rule extends ChapterEntry(Rule) with TheoremLineParser

case class ArbitraryVariableException(message: String) extends Exception(message)

case class DirectRule(
    id: String,
    premiseTemplates: Seq[Statement],
    conclusionTemplate: Statement,
    arbitraryVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables)
  extends Rule with Deduction

case class FantasyRule(
    id: String,
    assumptionTemplate: Statement,
    premiseTemplates: Seq[Statement],
    conclusionTemplate: Statement)
  extends Rule {

  def getSubstitutions(
    assumption: Statement,
    premises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): Substitutions = {
    val assumptionSubstitutionAttempt = assumptionTemplate.calculateSubstitutions(assumption)
    val premiseSubstitutionAttempts = premises.zip(premiseTemplates).map { case (premise, premiseTemplate) =>
      premiseTemplate.calculateSubstitutions(premise)
    }
    val initialSubstitutions = Substitutions.mergeAttempts(assumptionSubstitutionAttempt +: premiseSubstitutionAttempts)
      .getOrElse(throw ParseException.withMessage(
        s"Could not match premises\n$premises\n$premiseTemplates",
        line.fullLine))
    val requiredVariables = (premiseTemplates.map(_.variables) :+ conclusionTemplate.variables).reduce(_ ++ _)
    initialSubstitutions.expand(requiredVariables, line, context)._1
  }

  def makeSubstitutions(substitutions: Substitutions): FantasyRule = FantasyRule(
    id,
    assumptionTemplate.applySubstitutions(substitutions),
    premiseTemplates.map(_.applySubstitutions(substitutions)),
    conclusionTemplate.applySubstitutions(substitutions))

  def simplify(assumption: Statement, premises: Seq[Statement]): FantasyRule = {
    val distinctVariableAttempts = assumptionTemplate.attemptSimplification(assumption) +:
      premises.zip(premiseTemplates).map {
        case (premise, premiseTemplate) =>
          premiseTemplate.attemptSimplification(premise)
      }
    val distinctVariables = distinctVariableAttempts.traverseOption
      .getOrElse(throw new Exception("Unexpected error resolving simplifications")).reduce(_ ++ _)
    FantasyRule(
      id,
      assumptionTemplate.makeSimplifications(distinctVariables),
      premiseTemplates.map(_.makeSimplifications(distinctVariables)),
      conclusionTemplate.makeSimplifications(distinctVariables))
  }

  def matchAssumptionAndPremises(
    assumption: Statement,
    premises: Seq[Statement],
    line: PartialLine,
    context: Context
  ): FantasyRule = {
    val substitutions = getSubstitutions(assumption, premises, line, context)
    makeSubstitutions(substitutions).simplify(assumption, premises)
  }

  private def applyWithDeduction(
    deduction: Deduction,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val deductionPremiseTemplate = deduction.premiseTemplates match {
      case Seq(singlePremiseTemplate) =>
        singlePremiseTemplate
      case _ =>
        throw ParseException.withMessage("Can only apply rule to a deduction with a single premise", line.fullLine)
    }
    val premiseTemplate = premiseTemplates match {
      case Seq(singlePremise) =>
        singlePremise
      case _ =>
        throw ParseException.withMessage("Can only apply a rule with a single premise to a theorem", line.fullLine)
    }
    val (deductionPremise, lineAfterDeductionPremise) = Statement.parse(line, context)
    val updatedDeduction = deduction.matchPremises(
      Seq(deductionPremise),
      lineAfterDeductionPremise,
      context)
    val updatedRule = matchAssumptionAndPremises(
      updatedDeduction.premiseTemplates.head,
      Seq(updatedDeduction.conclusionTemplate),
      lineAfterDeductionPremise,
      context)
    theoremBuilder.addStep(Step(updatedRule.conclusionTemplate))
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val (premises, lineAfterPremises) = premiseTemplates.mapFold(line) { (premiseTemplate, lineSoFar) =>
        readReference(lineSoFar, theoremBuilder)
      }
      val updatedRule = matchAssumptionAndPremises(
        fantasy.assumption,
        premises,
        lineAfterPremises,
        context
      )
      Step(updatedRule.conclusionTemplate, Some(Step.Fantasy(fantasy.assumption, fantasy.steps)))
    }
  }

  override def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    def withDeduction = line.splitFirstWord.optionMapLeft(n => context.deductions.find(_.id == n)) map {
      case (deduction, restOfLine) =>
        applyWithDeduction(deduction, theoremBuilder, restOfLine, context)
    }
    withDeduction.getOrElse(applyWithFantasy(theoremBuilder, line, context))
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
    context.copy(rules = context.rules :+ rule)
  }
}
