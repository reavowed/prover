package net.prover.model

trait Rule {
  def name: String
  def applyToTheorem(theoremBuilder: TheoremBuilder, text: String): TheoremBuilder
}

case class DirectRule(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Rule {
  override def applyToTheorem(theoremBuilder: TheoremBuilder, applicationText: String): TheoremBuilder = {
    val matchAttempts = premises.mapFold(applicationText) { case (textSoFar, premise) =>
      textSoFar match {
        case SingleWord(reference, remainingText) =>
          (remainingText, premise.attemptMatch(theoremBuilder.resolveReference(reference)))
      }
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
  override def applyToTheorem(theoremBuilder: TheoremBuilder, applicationText: String): TheoremBuilder = {
    theoremBuilder.replaceFantasy { fantasy =>
      val matchAttempts = hypothesis.attemptMatch(fantasy.hypothesis) +: premises.mapFold(applicationText) { case (textSoFar, premise) =>
        textSoFar match {
          case SingleWord(reference, remainingText) =>
            (remainingText, premise.attemptMatch(fantasy.resolveReference(reference)))
        }
      }
      val matchResult = Statement.mergeMatchAttempts(matchAttempts)
        .getOrElse(throw new Exception(s"Could not match rule premises\n$applicationText"))
      val statement = conclusion.replace(matchResult)
      Step(statement, Some(Step.Fantasy(fantasy.hypothesis, fantasy.steps)))
    }
  }
}

object Rule {
  def parse(ruleText: String, connectives: Seq[Connective]): Rule = {
    ruleText match {
      case SingleWord(name, textAfterName) =>
        val (hypothesisOrPremises, remainingText) = Statement.parseList(textAfterName, connectives)
        remainingText match {
          case SingleWord("⇒", conclusionText) =>
            val (conclusion, _) = Statement.parse(conclusionText, connectives)
            DirectRule(name, hypothesisOrPremises, conclusion)
          case SingleWord("⊢", premisesText) =>
            hypothesisOrPremises match {
              case Seq(hypothesis) =>
                val (premises, textAfterPremises) = Statement.parseList(premisesText, connectives)
                textAfterPremises match {
                  case SingleWord("⇒", conclusionText) =>
                    val (conclusion, _) = Statement.parse(conclusionText, connectives)
                    FantasyRule(name, hypothesis, premises, conclusion)
                  case _ =>
                    throw new Exception("Rule did not have a conclusion\n" + textAfterPremises)
                }
              case _ =>
                throw new Exception("A rule cannot discharge more than one assumption")
            }
        }
      case _ =>
        throw new Exception("Rule was not a name followed by a definition\n" + ruleText)
    }
  }

}
