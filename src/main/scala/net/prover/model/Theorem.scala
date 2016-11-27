package net.prover.model

import scala.util.control.NonFatal

case class Theorem(
    name: String,
    title: String,
    hypotheses: Seq[Statement],
    steps: Seq[Step],
    result: Statement)
  extends ChapterEntry with TheoremLineParser {

  val `type` = "theorem"

  override def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder = {
    val (remainingLine, matchAttempts) = hypotheses.mapFold(line) { (lineSoFar, premise) =>
      lineSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(theoremBuilder.resolveReference(r))).swap
    }
    val matchResult = Statement.mergeMatchAttempts(matchAttempts)
      .getOrElse(throw ParseException.withMessage(s"Could not match theorem hypotheses", line.fullLine))
    val missingAtoms = result.atoms.diff(matchResult.keys.toSeq)
    val expandedMatch = readMissingAtoms(missingAtoms, remainingLine, book)._1 ++ matchResult
    val statement = result.replace(expandedMatch)
    theoremBuilder.addStep(Step(statement))
  }
}

trait TheoremLineParser {
  def name: String
  def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder

  def readMissingAtoms(atoms: Seq[Int], line: PartialLine, book: Book): (Map[Int, Statement], PartialLine) = {
    atoms.mapFold(line) { (lineSoFar, missingAtom) =>
      Statement.parse(lineSoFar, book.connectives).swap.mapRight(missingAtom -> _)
    }.swap.mapLeft(_.toMap)
  }
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

object Theorem extends ChapterEntryParser[Theorem] {
  override val name: String = "theorem"
  override def parse(firstLine: PartialLine, lines: Seq[BookLine], book: Book): (Theorem, Seq[BookLine]) = {
    val (name, title) = firstLine.splitFirstWord.mapRight(_.remainingText)
    def parseLine(
      line: BookLine,
      theoremBuilder: TheoremBuilder
    ): TheoremBuilder = {
      try {
        val parsers = Seq(HypothesisParser, FantasyHypothesisParser) ++ book.rules ++ book.definitions ++ book.theorems
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
