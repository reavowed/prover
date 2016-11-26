package net.prover.model

import net.prover.model.TheoremBuilder.Fantasy

case class Theorem(name: String, title: String, hypotheses: Seq[Statement], steps: Seq[Step], result: Statement)

case class TheoremBuilder(
  hypotheses: Seq[Statement] = Nil,
  steps: Seq[Step] = Nil,
  fantasyOption: Option[Fantasy] = None) {
  def addHypothesis(hypothesis: Statement) = copy(hypotheses = hypotheses :+ hypothesis)
  def addStep(step: Step): TheoremBuilder = {
    fantasyOption match {
      case Some(fantasy) =>
        copy(fantasyOption = Some(fantasy.addStep(step)))
      case None =>
        copy(steps = steps :+ step)
    }
  }
  def addFantasy(fantasyHypothesis: Statement): TheoremBuilder = {
    val newFantasy = fantasyOption match {
      case Some(fantasy) =>
        fantasy.addFantasy(fantasyHypothesis)
      case None =>
        Fantasy(fantasyHypothesis)
    }
    copy(fantasyOption = Some(newFantasy))
  }
  def replaceFantasy(f: Fantasy => Step): TheoremBuilder = {
    fantasyOption match {
      case Some(fantasy) =>
        fantasy.fantasyOption match {
          case None =>
            copy(fantasyOption = None, steps = steps :+ f(fantasy))
          case Some(_) =>
            copy(fantasyOption = Some(fantasy.replaceFantasy(f)))
        }
      case None =>
        throw new Exception("No fantasy to replace")
    }
  }
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
    steps: Seq[Step] = Nil,
    fantasyOption: Option[Fantasy] = None) {
    def addStep(step: Step): Fantasy = {
      fantasyOption match {
        case Some(fantasy) =>
          copy(fantasyOption = Some(fantasy.addStep(step)))
        case None =>
          copy(steps = steps :+ step)
      }
    }
    def addFantasy(fantasyHypothesis: Statement): Fantasy = {
      val newFantasy = fantasyOption match {
        case Some(fantasy) =>
          fantasy.addFantasy(fantasyHypothesis)
        case None =>
          Fantasy(fantasyHypothesis)
      }
      copy(fantasyOption = Some(newFantasy))
    }
    def resolveReference(reference: String): Statement = {
      val stepReference = "(\\d+)".r
      val fantasyReference = "f\\.(.+)".r
      reference match {
        case "h" =>
          hypothesis
        case fantasyReference(innerReference) =>
          fantasyOption.getOrElse(throw new Exception("No fantasy to reference")).resolveReference(innerReference)
        case stepReference(IntParser(number)) =>
          steps(number - 1).statement
      }
    }
    def replaceFantasy(f: Fantasy => Step): Fantasy = {
      fantasyOption match {
        case Some(fantasy) =>
          fantasy.fantasyOption match {
            case None =>
              copy(fantasyOption = None, steps = steps :+ f(fantasy))
            case Some(_) =>
              copy(fantasyOption = Some(fantasy.replaceFantasy(f)))
          }
        case None =>
          throw new Exception("No fantasy to replace")
      }
    }
  }
}

object Theorem extends BookEntryParser[Theorem] {
  override val name: String = "theorem"
  override def parse(firstLine: String, lines: Seq[String], book: Book): (Theorem, Seq[String]) = {
    val (name, title) = firstLine.splitFirstWord
    def parseLine(
      line: String,
      theoremBuilder: TheoremBuilder
    ): TheoremBuilder = {
      import book.connectives, book.rules
      line.splitFirstWord match {
        case ("hypothesis", hypothesisText) =>
          val hypothesis = Statement.parse(hypothesisText, connectives)._1
          theoremBuilder.addHypothesis(hypothesis)
        case ("assume", hypothesisText) =>
          val hypothesis = Statement.parse(hypothesisText, connectives)._1
          theoremBuilder.addFantasy(hypothesis)
        case (ruleName, ruleApplicationText) =>
          val rule = rules.find(_.name == ruleName).getOrElse(throw new Exception(s"Did not understand rule $ruleName\n" + line))
          rule.applyToTheorem(theoremBuilder, ruleApplicationText)
      }
    }

    def parseHelper(linesRemaining: Seq[String], theoremBuilder: TheoremBuilder): (Theorem, Seq[String]) = {
      linesRemaining match {
        case "qed" +: nonTheoremLines =>
          import theoremBuilder._
          if (fantasyOption.isDefined) throw new Exception("Cannot finish theorem with open assumption")
          (Theorem(name, title, hypotheses, steps, steps.last.statement), nonTheoremLines)
        case definitionLine +: otherLines =>
          parseHelper(otherLines, parseLine(definitionLine, theoremBuilder))
        case Nil =>
          throw new Exception("Book ended in middle of theorem")
      }
    }
    parseHelper(lines, TheoremBuilder())
  }
  override def addToBook(theorem: Theorem, book: Book): Book = book.copy(theorems = book.theorems :+ theorem)
}
