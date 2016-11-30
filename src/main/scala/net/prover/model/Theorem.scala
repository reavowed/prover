package net.prover.model

import scala.util.control.NonFatal

case class Theorem(
    id: String,
    title: String,
    hypotheses: Seq[Statement],
    steps: Seq[Step],
    result: Statement)
  extends ChapterEntry with DirectStepParser {

  val `type` = "theorem"

  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (matchAttempts, lineAfterHypotheses) = hypotheses.mapFold(line) { (hypothesis, lineSoFar) =>
      readReference(lineSoFar, theoremBuilder).mapLeft(hypothesis.attemptMatch)
    }
    val matchResult = Match.mergeAttempts(matchAttempts)
      .getOrElse(throw ParseException.withMessage(s"Could not match theorem hypotheses", line.fullLine))
    val (expandedMatch, lineAfterMatch) = matchResult.expand(result.variables, lineAfterHypotheses, context)
    val step = Step(result.applyMatch(expandedMatch))
    (step, lineAfterMatch)
  }
}

trait TheoremLineParser {
  def id: String
  def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder

  protected def readReference(line: PartialLine, theoremBuilder: TheoremBuilder): (Statement, PartialLine) = {
    line.splitFirstWord.mapLeft(theoremBuilder.resolveReference)
  }

  protected def readStatement(line: PartialLine, theoremBuilder: TheoremBuilder, context: Context): (Statement, PartialLine) = {
    val ReferenceMatcher = "r\\.(.*)".r
    line match {
      case WordAndRemainingText(ReferenceMatcher(reference), lineAfterReference) =>
        (theoremBuilder.resolveReference(reference), lineAfterReference)
      case _ =>
        Statement.parse(line, context)
    }
  }
}

object HypothesisParser extends TheoremLineParser {
  override val id: String = "hypothesis"
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (hypothesis, _) = Statement.parse(line, context)
    theoremBuilder.addHypothesis(hypothesis)
  }
}

object FantasyHypothesisParser extends TheoremLineParser {
  override val id: String = "assume"
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (hypothesis, _) = Statement.parse(line, context)
    theoremBuilder.addFantasy(hypothesis)
  }
}

trait DirectStepParser extends TheoremLineParser {
  def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine)

  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (step, _) = readStep(theoremBuilder, line, context)
    theoremBuilder.addStep(step)
  }
}

object Theorem extends ChapterEntryParser[Theorem] {
  override val name: String = "theorem"
  override def parse(firstLine: PartialLine, lines: Seq[BookLine], context: Context): (Theorem, Seq[BookLine]) = {
    val (id, title) = firstLine.splitFirstWord.mapRight(_.remainingText)
    def parseLine(
      line: BookLine,
      theoremBuilder: TheoremBuilder
    ): TheoremBuilder = {
      try {
        val parsers = Seq(HypothesisParser, FantasyHypothesisParser) ++
          context.rules ++
          context.connectives.flatMap(_.definition) ++
          context.predicates.flatMap(_.definition) ++
          context.theorems ++
          context.axioms
        val (lineType, restOfLine) = line.splitFirstWord
        val parser = parsers.find(_.id == lineType).getOrElse(throw new Exception(s"Unrecognised theorem line '$lineType'"))
        parser.readAndUpdateTheoremBuilder(theoremBuilder, restOfLine, context)
      } catch {
        case e: ParseException =>
          throw e
        case NonFatal(ex) =>
          throw ParseException.fromCause(ex, line)
      }
    }

    def parseHelper(linesRemaining: Seq[BookLine], theoremBuilder: TheoremBuilder): (Theorem, Seq[BookLine]) = {
      linesRemaining match {
        case BookLine("qed", _, _) +: nonTheoremLines =>
          import theoremBuilder._
          if (fantasyOption.isDefined)
            throw new Exception("Cannot finish theorem with open assumption")
          (Theorem(id, title, hypotheses, steps, steps.last.statement), nonTheoremLines)
        case definitionLine +: otherLines =>
          parseHelper(otherLines, parseLine(definitionLine, theoremBuilder))
        case Nil =>
          throw new Exception("Book ended in middle of theorem")
      }
    }
    parseHelper(lines, TheoremBuilder())
  }
  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(theorems = context.theorems :+ theorem)
  }
}
