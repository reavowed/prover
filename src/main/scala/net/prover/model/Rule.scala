package net.prover.model

trait Rule extends TheoremLineParser

case class DirectRule(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {
  override def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder = {
    val matchAttempts = premises.mapFold(line) { (lineSoFar, premise) =>
      lineSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(theoremBuilder.resolveReference(r))).swap
    }._2
    val matchResult = Statement.mergeMatchAttempts(matchAttempts)
      .getOrElse(throw ParseException.withMessage("Could not match rule premises", line.fullLine))
    val statement = conclusion.replace(matchResult)
    theoremBuilder.addStep(Step(statement))
  }
}

case class FantasyRule(
    name: String,
    hypothesis: Statement,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {
  override def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val matchAttempts = hypothesis.attemptMatch(fantasy.hypothesis) +: premises.mapFold(line) { (lineSoFar, premise) =>
        lineSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(fantasy.resolveReference(r))).swap
      }._2
      val matchResult = Statement.mergeMatchAttempts(matchAttempts)
        .getOrElse(throw ParseException.withMessage("Could not match rule premises", line.fullLine))
      val statement = conclusion.replace(matchResult)
      Step(statement, Some(Step.Fantasy(fantasy.hypothesis, fantasy.steps)))
    }
  }
}

object Rule extends SingleLineBookEntryParser[Rule] {
  override val name: String = "rule"
  override def parse(line: PartialLine, book: Book): Rule = {
    val connectives = book.connectives
    val (name, lineAfterName) = line.splitFirstWord
    val (hypothesisOrPremises, lineAfterHypothesisOrPremises) = Statement.parseList(lineAfterName, connectives)
    val (firstSymbol, lineAfterFirstSymbol) = lineAfterHypothesisOrPremises.splitFirstWord
    firstSymbol match {
      case "⇒" =>
        val (conclusion, _) = Statement.parse(lineAfterFirstSymbol, connectives)
        DirectRule(name, hypothesisOrPremises, conclusion)
      case "⊢" =>
        val hypothesis = hypothesisOrPremises match {
          case Seq(singleHypothesis) =>
            singleHypothesis
          case _ =>
            throw ParseException.withMessage("A rule cannot discharge more than one assumption", line.fullLine)
        }
        val (premises, textAfterPremises) = Statement.parseList(lineAfterFirstSymbol, connectives)
        val (secondSymbol, conclusionText) = textAfterPremises.splitFirstWord
        if (secondSymbol != "⇒") {
          throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
        }
        val (conclusion, _) = Statement.parse(conclusionText, connectives)
        FantasyRule(name, hypothesis, premises, conclusion)
      case _ =>
        throw ParseException.withMessage("Rule did not have a conclusion", line.fullLine)
    }
  }
  override def addToBook(rule: Rule, book: Book): Book = {
    book.copy(rules = book.rules :+ rule)
  }
}
