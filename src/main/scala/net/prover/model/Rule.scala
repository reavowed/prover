package net.prover.model

abstract class Rule extends ChapterEntry(Rule) with TheoremLineParser

case class ArbitraryVariableException(message: String) extends Exception(message)

case class DirectRule(
    id: String,
    premiseTemplates: Seq[Statement],
    conclusionTemplate: Statement,
    arbitraryVariables: Seq[TermVariable],
    distinctVariableRequirements: DistinctVariableRequirements)
  extends Rule with Deduction

case class FantasyRule(
    id: String,
    assumptionTemplate: Statement,
    premiseTemplates: Seq[Statement],
    conclusionTemplate: Statement)
  extends Rule {
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
    val (matcher, lineAfterVariables) = deduction.matchPremises(
      Seq((deductionPremise, deductionPremiseTemplate)),
      deduction.conclusionTemplate,
      lineAfterDeductionPremise,
      context)
    val theoremPremise = deductionPremiseTemplate.applyMatch(matcher)
    val theoremConclusion = deduction.conclusionTemplate.applyMatch(matcher)
    val conclusion = matchPremisesToConclusion(
      Seq((theoremPremise, assumptionTemplate), (theoremConclusion, premiseTemplate)),
      conclusionTemplate,
      lineAfterVariables,
      context)._1
    theoremBuilder.addStep(Step(conclusion))
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val (premisesAndTemplates, lineAfterPremises) = premiseTemplates.mapFold(line) { (premiseTemplate, lineSoFar) =>
        readReference(lineSoFar, theoremBuilder).mapLeft((_, premiseTemplate))
      }
      val conclusion = matchPremisesToConclusion(
        (fantasy.assumption, assumptionTemplate) +: premisesAndTemplates,
        conclusionTemplate,
        lineAfterPremises,
        context
      )._1
      Step(conclusion, Some(Step.Fantasy(fantasy.assumption, fantasy.steps)))
    }
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
        val (distinctVariableRequirements, _) = lineAfterArbitraryVariables match {
          case WordAndRemainingText("x", lineAfterCross) =>
            val (distinctVariable, lineAfterDistinctVariable) = Term.parse(lineAfterCross, context)
              .mapLeft(Term.asVariable)
            val (distinctStatement, lineAfterDistinctStatement) = Statement.parseStatementVariable(lineAfterDistinctVariable, context)
            (
              DistinctVariableRequirements(
                Map(distinctVariable -> Variables(Seq(distinctStatement), Nil))),
              lineAfterDistinctStatement)
          case _ =>
            (DistinctVariableRequirements.empty, lineAfterArbitraryVariables)
        }
        DirectRule(id, premises, conclusion, arbitraryVariables, distinctVariableRequirements)
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
