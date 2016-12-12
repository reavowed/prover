package net.prover.model

abstract class Rule extends ChapterEntry(Rule) with TheoremLineParser

case class ArbitraryVariableException(message: String) extends Exception(message)

case class DirectRule(
    id: String,
    premiseTemplates: Seq[Statement],
    conclusionTemplate: Statement,
    arbitraryVariables: Seq[TermVariable])
  extends Rule with Deduction

case class FantasyRule(
    id: String,
    assumptionTemplate: Statement,
    premiseTemplates: Seq[Statement],
    conclusionTemplate: Statement)
  extends Rule {
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    def withTheorem = line.splitFirstWord.optionMapLeft(n => context.theorems.find(_.id == n)) map {
      case (theorem, restOfLine) =>
        applyWithPreviousTheorem(theorem, theoremBuilder, restOfLine, context)
    }
    def withDefinition = line.splitFirstWord.optionMapLeft(n => context.definitions.find(_.id == n)) map {
      case (definition, restOfLine) =>
        applyWithDefinition(definition, theoremBuilder, restOfLine, context)
    }
    withTheorem.orElse(withDefinition).getOrElse(applyWithFantasy(theoremBuilder, line, context))
  }


  private def applyWithPreviousTheorem(
    theorem: Theorem,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val theoremPremiseTemplate = theorem.premiseTemplates match {
      case Seq(singlePremiseTemplate) =>
        singlePremiseTemplate
      case _ =>
        throw ParseException.withMessage("Can only apply rule to a theorem with a single premise", line.fullLine)
    }
    val premiseTemplate = premiseTemplates match {
      case Seq(singlePremise) =>
        singlePremise
      case _ =>
        throw ParseException.withMessage("Can only apply a rule with a single premise to a theorem", line.fullLine)
    }
    val requiredVariables = theoremPremiseTemplate.variables ++ theorem.conclusionTemplate.variables
    val (matcher, lineAfterVariables) = Match.empty.expand(requiredVariables, line, context)
    val theoremPremise = theoremPremiseTemplate.applyMatch(matcher)
    val theoremConclusion = theorem.conclusionTemplate.applyMatch(matcher)
    val conclusion = matchPremisesToConclusion(
      Seq((theoremPremise, assumptionTemplate), (theoremConclusion, premiseTemplate)),
      conclusionTemplate,
      lineAfterVariables,
      context)._1
    theoremBuilder.addStep(Step(conclusion))
  }

  private def applyWithDefinition(
    definition: Definition,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val premiseTemplate = premiseTemplates match {
      case Seq(singlePremise) =>
        singlePremise
      case _ =>
        throw ParseException.withMessage("Can only apply rule to definition if it has a single premise", line.fullLine)
    }

    val (definitionPremise, lineAfterPremise) = Statement.parse(line, context)
    val (definitionConclusion, _) = definition.applyToStatement(definitionPremise, lineAfterPremise, theoremBuilder, context)

    val conclusion = matchPremisesToConclusion(
      Seq((definitionPremise, assumptionTemplate), (definitionConclusion, premiseTemplate)),
      conclusionTemplate,
      line,
      context
    )._1
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
        val freeVariables = lineAfterConclusion match {
          case WordAndRemainingText("|", lineAfterPipe) =>
            Term.parseList(lineAfterPipe, context)._1.map(Term.asVariable)
          case _ =>
            Nil
        }
        DirectRule(id, premises, conclusion, freeVariables)
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
