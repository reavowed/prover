package net.prover.model

import scala.util.control.NonFatal

case class Theorem(name: String, title: String, hypotheses: Seq[Statement], steps: Seq[Step], result: Statement)

trait TheoremLineParser {
  def name: String
  def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder
}

object HypothesisParser extends TheoremLineParser {
  override val name: String = "hypothesis"
  override def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder = {
    import book.connectives
    val (hypothesis, _) = Statement.parse(line, connectives)
    theoremBuilder.addHypothesis(hypothesis)
  }
}

object FantasyHypothesisParser extends TheoremLineParser {
  override val name: String = "assume"
  override def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder = {
    import book.connectives
    val (hypothesis, _) = Statement.parse(line, connectives)
    theoremBuilder.addFantasy(hypothesis)
  }
}

object Theorem extends BookEntryParser[Theorem] {
  override val name: String = "theorem"
  override def parse(firstLine: PartialLine, lines: Seq[BookLine], book: Book): (Theorem, Seq[BookLine]) = {
    val (name, title) = firstLine.splitFirstWord.mapRight(_.remainingText)
    def parseLine(
      line: BookLine,
      theoremBuilder: TheoremBuilder
    ): TheoremBuilder = {
      try {
        val parsers = Seq(HypothesisParser, FantasyHypothesisParser) ++ book.rules ++ book.definitions
        val (lineType, restOfLine) = line.splitFirstWord
        val parser = parsers.find(_.name == lineType).getOrElse(throw new Exception(s"Unrecognised theorem line '$lineType'"))
        parser.applyToTheorem(theoremBuilder, restOfLine, book)
      } catch {
        case NonFatal(ex) =>
          throw ParseException.fromCause(ex, line)
      }
    }

    def parseHelper(linesRemaining: Seq[BookLine], theoremBuilder: TheoremBuilder): (Theorem, Seq[BookLine]) = {
      linesRemaining match {
        case BookLine("qed", _) +: nonTheoremLines =>
          import theoremBuilder._
          if (fantasyOption.isDefined)
            throw new Exception("Cannot finish theorem with open assumption")
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
