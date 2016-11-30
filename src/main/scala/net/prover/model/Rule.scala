package net.prover.model

sealed trait Rule extends ChapterEntry with TheoremLineParser {
  val `type` = "rule"
}

case class DirectRule(
    id: String,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule with DirectStepParser {
  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (premiseMatchAttempts, lineAfterPremises) = premises.mapFold(line) { (premise, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder).mapLeft(premise.attemptMatch)
    }
    val premisesMatch = Match.mergeAttempts(premiseMatchAttempts)
      .getOrElse(throw ParseException.withMessage("Could not match rule premises", line.fullLine))
    val (expandedMatch, remainingLine) = premisesMatch.expand(conclusion.variables, lineAfterPremises, context)
    val statement = conclusion.applyMatch(expandedMatch)
    val step = Step(statement)
    (step, remainingLine)
  }
}

case class FantasyRule(
    id: String,
    hypothesis: Statement,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    def withTheorem = line.splitFirstWord.optionMapLeft(n => context.theorems.find(_.id == n)) map {
      case (theorem, restOfLine) =>
        applyWithPreviousTheorem(theorem, theoremBuilder, restOfLine, context)
    }
    def withDefinition = line.splitFirstWord.optionMapLeft(n => context.connectives.flatMap(_.definition).find(_.id == n)) map {
      case (definition, restOfLine) =>
        applyWithDefinition(definition, theoremBuilder, restOfLine, context)
    }
    withTheorem.orElse(withDefinition).getOrElse(applyWithFantasy(theoremBuilder, line))
  }

  private def applyWithPreviousTheorem(
    theorem: Theorem,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val theoremHypothesis = theorem.hypotheses match {
      case Seq(singleHypothesis) =>
        singleHypothesis
      case _ =>
        throw ParseException.withMessage("Can only apply rule to theorem with a single hypothesis", line.fullLine)
    }
    val premise = premises match {
      case Seq(singlePremise) =>
        singlePremise
      case _ =>
        throw ParseException.withMessage("Can only apply rule to theorem if it has a single premise", line.fullLine)
    }
    val requiredVariables = theoremHypothesis.variables ++ theorem.result.variables
    val matcher = Match.empty.expand(requiredVariables, line, context)._1
    val updatedTheoremHypothesis = theoremHypothesis.applyMatch(matcher)
    val updatedTheoremResult = theorem.result.applyMatch(matcher)
    val fullMatch = Match.mergeAttempts(Seq(
      hypothesis.attemptMatch(updatedTheoremHypothesis),
      premise.attemptMatch(updatedTheoremResult))
    ).getOrElse(throw ParseException.withMessage(
      s"Theorem $updatedTheoremHypothesis ⊢ $updatedTheoremResult did not match rule requirement $hypothesis ⊢ $premise",
      line.fullLine))
    val statement = conclusion.applyMatch(fullMatch)
    theoremBuilder.addStep(Step(statement))
  }

  private def applyWithDefinition(
    definition: Definition,
    theoremBuilder: TheoremBuilder,
    line: PartialLine,
    context: Context
  ): TheoremBuilder = {
    val premise = premises match {
      case Seq(singlePremise) =>
        singlePremise
      case _ =>
        throw ParseException.withMessage("Can only apply rule to definition if it has a single premise", line.fullLine)
    }

    val (definitionHypothesis, lineAfterHypothesis) = readStatement(line, theoremBuilder, context)
    val (definitionResult, _) = definition.applyToStatement(definitionHypothesis, lineAfterHypothesis, theoremBuilder, context)

    val hypothesisMatch = hypothesis.attemptMatch(definitionHypothesis)
    val premiseMatch = premise.attemptMatch(definitionResult)
    val fullMatch = Match.mergeAttempts(Seq(hypothesisMatch, premiseMatch))
      .getOrElse(throw ParseException.withMessage(
        s"Definition $definitionHypothesis ⇒ $definitionResult did not match rule $hypothesis ⊢ $premise",
        line.fullLine))
    val statement = conclusion.applyMatch(fullMatch)
    theoremBuilder.addStep(Step(statement))
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, line: PartialLine): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val matchAttempts = hypothesis.attemptMatch(fantasy.hypothesis) +: premises.mapFold(line) { (premise, lineSoFar) =>
        readReference(lineSoFar, theoremBuilder).mapLeft(premise.attemptMatch)
      }._1
      val matchResult = Match.mergeAttempts(matchAttempts)
        .getOrElse(throw ParseException.withMessage("Could not match rule premises", line.fullLine))
      val statement = conclusion.applyMatch(matchResult)
      Step(statement, Some(Step.Fantasy(fantasy.hypothesis, fantasy.steps)))
    }
  }
}

object Rule extends SingleLineChapterEntryParser[Rule] {
  override val name: String = "rule"
  override def parse(line: PartialLine, context: Context): Rule = {
    val (id, lineAfterName) = line.splitFirstWord
    val (hypothesisOrPremises, lineAfterHypothesisOrPremises) = Statement.parseList(lineAfterName, context)
    val (firstSymbol, lineAfterFirstSymbol) = lineAfterHypothesisOrPremises.splitFirstWord
    firstSymbol match {
      case "⇒" =>
        val (conclusion, _) = Statement.parse(lineAfterFirstSymbol, context)
        DirectRule(id, hypothesisOrPremises, conclusion)
      case "⊢" =>
        val hypothesis = hypothesisOrPremises match {
          case Seq(singleHypothesis) =>
            singleHypothesis
          case _ =>
            throw ParseException.withMessage("A rule cannot discharge more than one assumption", line.fullLine)
        }
        val (premises, textAfterPremises) = Statement.parseList(lineAfterFirstSymbol, context)
        val (secondSymbol, conclusionText) = textAfterPremises.splitFirstWord
        if (secondSymbol != "⇒") {
          throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
        }
        val (conclusion, _) = Statement.parse(conclusionText, context)
        FantasyRule(id, hypothesis, premises, conclusion)
      case _ =>
        throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
    }
  }
  override def addToContext(rule: Rule, context: Context): Context = {
    context.copy(rules = context.rules :+ rule)
  }
}
