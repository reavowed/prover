package net.prover.model

sealed trait Rule extends ChapterEntry with TheoremLineParser {
  val `type` = "rule"
}

case class DirectRule(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule with DirectStepParser {
  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (matchAttempts, lineAfterPremises) = premises.mapFold(line) { (premise, lineSoFar) =>
      lineSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(theoremBuilder.resolveReference(r)))
    }
    val matchResult = Statement.mergeMatchAttempts(matchAttempts)
      .getOrElse(throw ParseException.withMessage("Could not match rule premises", line.fullLine))
    val statement = conclusion.replace(matchResult)
    val step = Step(statement)
    (step, lineAfterPremises)
  }
}

case class FantasyRule(
    name: String,
    hypothesis: Statement,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    def withTheorem = line.splitFirstWord.optionMapLeft(n => context.theorems.find(_.name == n)) map {
      case (theorem, restOfLine) =>
        applyWithPreviousTheorem(theorem, theoremBuilder, restOfLine, context)
    }
    def withDefinition = line.splitFirstWord.optionMapLeft(n => context.definitions.find(_.name == n)) map {
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
    val atoms = Seq(theoremHypothesis, theorem.result).flatMap(_.atoms).distinct
    val atomMatch = readMissingAtoms(atoms, line, context)._1
    val updatedTheoremHypothesis = theoremHypothesis.replace(atomMatch)
    val updatedTheoremResult = theorem.result.replace(atomMatch)
    val fullMatch = Statement.mergeMatchAttempts(Seq(
      hypothesis.attemptMatch(updatedTheoremHypothesis),
      premise.attemptMatch(updatedTheoremResult))
    ).getOrElse(throw ParseException.withMessage(
      s"Theorem $updatedTheoremHypothesis ⊢ $updatedTheoremResult did not match rule requirement $hypothesis ⊢ $premise",
      line.fullLine))
    val statement = conclusion.replace(fullMatch)
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
        throw ParseException.withMessage("Can only apply rule to theorem if it has a single premise", line.fullLine)
    }

    val (definitionHypothesis, _) = Statement.parse(line, context)
    val definitionResult = definition.applyToStatement(definitionHypothesis)

    val hypothesisMatch = hypothesis.attemptMatch(definitionHypothesis)
    val premiseMatch = premise.attemptMatch(definitionResult)
    val fullMatch = Statement.mergeMatchAttempts(Seq(hypothesisMatch, premiseMatch))
      .getOrElse(throw ParseException.withMessage(
        s"Definition $definitionHypothesis ⇒ $definitionResult did not match rule $hypothesis ⊢ $premise",
        line.fullLine))
    val statement = conclusion.replace(fullMatch)
    theoremBuilder.addStep(Step(statement))
  }

  private def applyWithFantasy(theoremBuilder: TheoremBuilder, line: PartialLine): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val matchAttempts = hypothesis.attemptMatch(fantasy.hypothesis) +: premises.mapFold(line) { (premise, lineSoFar) =>
        lineSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(theoremBuilder.resolveReference(r)))
      }._1
      val matchResult = Statement.mergeMatchAttempts(matchAttempts)
        .getOrElse(throw ParseException.withMessage("Could not match rule premises", line.fullLine))
      val statement = conclusion.replace(matchResult)
      Step(statement, Some(Step.Fantasy(fantasy.hypothesis, fantasy.steps)))
    }
  }
}

object Rule extends SingleLineChapterEntryParser[Rule] {
  override val name: String = "rule"
  override def parse(line: PartialLine, context: Context): Rule = {
    val (name, lineAfterName) = line.splitFirstWord
    val (hypothesisOrPremises, lineAfterHypothesisOrPremises) = Statement.parseList(lineAfterName, context)
    val (firstSymbol, lineAfterFirstSymbol) = lineAfterHypothesisOrPremises.splitFirstWord
    firstSymbol match {
      case "⇒" =>
        val (conclusion, _) = Statement.parse(lineAfterFirstSymbol, context)
        DirectRule(name, hypothesisOrPremises, conclusion)
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
        FantasyRule(name, hypothesis, premises, conclusion)
      case _ =>
        throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
    }
  }
  override def addToContext(rule: Rule, context: Context): Context = {
    context.copy(rules = context.rules :+ rule)
  }
}
