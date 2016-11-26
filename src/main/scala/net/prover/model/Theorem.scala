package net.prover.model

import net.prover.model.TheoremBuilder.Fantasy

case class Theorem(name: String, title: String, hypotheses: Seq[Statement], steps: Seq[Step])

case class TheoremBuilder(
  hypotheses: Seq[Statement] = Nil,
  steps: Seq[Step] = Nil,
  fantasyOption: Option[Fantasy] = None) {
  def addHypothesis(hypothesis: Statement) = copy(hypotheses = hypotheses :+ hypothesis)
  def addStep(step: Step) = {
    fantasyOption match {
      case Some(fantasy) =>
        copy(fantasyOption = Some(fantasy.addStep(step)))
      case None =>
        copy(steps = steps :+ step)
    }
  }
  def addFantasy(fantasyHypothesis: Statement) = copy(fantasyOption = Some(Fantasy(fantasyHypothesis)))
  def resolveReference(reference: String): Statement = {
    val hypothesisReference = "h(\\d+)".r
    val fantasyReference = "f\\.(.+)".r
    reference match {
      case hypothesisReference(IntParser(number)) =>
        hypotheses(number - 1)
      case fantasyReference(innerReference) =>
        fantasyOption.getOrElse(throw new Exception("No fantasy to reference")).resolveReference(innerReference)
    }
  }
}
object TheoremBuilder {
  case class Fantasy(
    hypothesis: Statement,
    steps: Seq[Step] = Nil) {
    def addStep(step: Step) = copy(steps = steps :+ step)
    def resolveReference(reference: String): Statement = {
      val stepReference = "(\\d+)".r
      reference match {
        case "h" =>
          hypothesis
        case stepReference(IntParser(number)) =>
          steps(number - 1).statement
      }
    }
  }
}

object Theorem {

  def parse(
    name: String,
    title: String,
    lines: Seq[String],
    rules: Seq[Rule],
    connectives: Seq[Connective]
  ): (Theorem, Seq[String]) = {
    def parseLine(
      line: String,
      theoremBuilder: TheoremBuilder
    ): TheoremBuilder = {
      line.splitKeyword match {
        case Seq("hypothesis", hypothesisText) =>
          val hypothesis = Statement.parse(hypothesisText, connectives)._1
          theoremBuilder.addHypothesis(hypothesis)
        case Seq("assume", hypothesisText) =>
          val hypothesis = Statement.parse(hypothesisText, connectives)._1
          theoremBuilder.addFantasy(hypothesis)
        case Seq(ruleName, ruleApplicationText) =>
          val rule = rules.find(_.name == ruleName).getOrElse(throw new Exception(s"Did not understand rule $ruleName\n" + line))
          rule.applyToTheorem(theoremBuilder, ruleApplicationText)
      }
    }

    def parseHelper(linesRemaining: Seq[String], theoremBuilder: TheoremBuilder): (Theorem, Seq[String]) = {
      linesRemaining match {
        case "qed" +: nonTheoremLines =>
          if (theoremBuilder.fantasyOption.isDefined) throw new Exception("Cannot finish theorem with open assumption")
          (Theorem(name, title, theoremBuilder.hypotheses, theoremBuilder.steps), nonTheoremLines)
        case definitionLine +: otherLines =>
          parseHelper(otherLines, parseLine(definitionLine, theoremBuilder))
        case Nil =>
          throw new Exception("Book ended in middle of theorem")
      }
    }
    parseHelper(lines, TheoremBuilder())
  }
}
