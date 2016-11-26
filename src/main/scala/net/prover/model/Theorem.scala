package net.prover.model

case class Theorem(name: String, title: String, hypotheses: Seq[Statement], steps: Seq[Step], result: Statement)

trait TheoremLineParser {
  def name: String
  def applyToTheorem(theoremBuilder: TheoremBuilder, text: String, book: Book): TheoremBuilder
}

object HypothesisParser extends TheoremLineParser {
  override val name: String = "hypothesis"
  override def applyToTheorem(theoremBuilder: TheoremBuilder, text: String, book: Book): TheoremBuilder = {
    import book.connectives
    val (hypothesis, _) = Statement.parse(text, connectives)
    theoremBuilder.addHypothesis(hypothesis)
  }
}

object FantasyHypothesisParser extends TheoremLineParser {
  override val name: String = "assume"
  override def applyToTheorem(theoremBuilder: TheoremBuilder, text: String, book: Book): TheoremBuilder = {
    import book.connectives
    val (hypothesis, _) = Statement.parse(text, connectives)
    theoremBuilder.addFantasy(hypothesis)
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
      val parsers = Seq(HypothesisParser, FantasyHypothesisParser) ++ book.rules ++ book.definitions
      val (lineType, restOfLine) = line.splitFirstWord
      val parser = parsers.find(_.name == lineType).getOrElse(throw new Exception(s"Unrecognised theorem line '$lineType'"))
      parser.applyToTheorem(theoremBuilder, restOfLine, book)
    }

    def parseHelper(linesRemaining: Seq[String], theoremBuilder: TheoremBuilder): (Theorem, Seq[String]) = {
      linesRemaining match {
        case "qed" +: nonTheoremLines =>
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
