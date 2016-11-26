package net.prover.model

trait Rule extends TheoremLineParser

case class DirectRule(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {
  override def applyToTheorem(theoremBuilder: TheoremBuilder, applicationText: String, book: Book): TheoremBuilder = {
    val matchAttempts = premises.mapFold(applicationText) { (textSoFar, premise) =>
      textSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(theoremBuilder.resolveReference(r))).swap
    }
    val matchResult = Statement.mergeMatchAttempts(matchAttempts)
      .getOrElse(throw new Exception(s"Could not match rule premises\n$applicationText"))
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
  override def applyToTheorem(theoremBuilder: TheoremBuilder, applicationText: String, book: Book): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val matchAttempts = hypothesis.attemptMatch(fantasy.hypothesis) +: premises.mapFold(applicationText) { (textSoFar, premise) =>
        textSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(fantasy.resolveReference(r))).swap
      }
      val matchResult = Statement.mergeMatchAttempts(matchAttempts)
        .getOrElse(throw new Exception(s"Could not match rule premises\n$name $applicationText"))
      val statement = conclusion.replace(matchResult)
      Step(statement, Some(Step.Fantasy(fantasy.hypothesis, fantasy.steps)))
    }
  }
}

object Rule extends SingleLineBookEntryParser[Rule] {
  override val name: String = "rule"
  override def parse(text: String, book: Book): Rule = {
    val connectives = book.connectives
    val (name, textAfterName) = text.splitFirstWord
    val (hypothesisOrPremises, textAfterHypothesisOrPremises) = Statement.parseList(textAfterName, connectives)
    val (firstSymbol, textAfterFirstSymbol) = textAfterHypothesisOrPremises.splitFirstWord
    firstSymbol match {
      case "⇒" =>
        val (conclusion, _) = Statement.parse(textAfterFirstSymbol, connectives)
        DirectRule(name, hypothesisOrPremises, conclusion)
      case "⊢" =>
        val hypothesis = hypothesisOrPremises match {
          case Seq(singleHypothesis) =>
            singleHypothesis
          case _ =>
            throw new Exception("A rule cannot discharge more than one assumption\n" + text)
        }
        val (premises, textAfterPremises) = Statement.parseList(textAfterFirstSymbol, connectives)
        val (secondSymbol, conclusionText) = textAfterPremises.splitFirstWord
        if (secondSymbol != "⇒") {
            throw new Exception("Rule did not have a conclusion\n" + text)
        }
        val (conclusion, _) = Statement.parse(conclusionText, connectives)
        FantasyRule(name, hypothesis, premises, conclusion)
      case _ =>
        throw new Exception("Rule did not have a conclusion\n" + text)
    }
  }
  override def addToBook(rule: Rule, book: Book): Book = {
    book.copy(rules = book.rules :+ rule)
  }
}
