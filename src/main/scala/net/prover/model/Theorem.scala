package net.prover.model

import scala.util.control.NonFatal

case class Theorem(
    name: String,
    title: String,
    hypotheses: Seq[Statement],
    steps: Seq[Step],
    result: Statement)
  extends ChapterEntry with DirectStepParser {

  val `type` = "theorem"

  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (matchAttempts, lineAfterHypotheses) = hypotheses.mapFold(line) { (premise, lineSoFar) =>
      lineSoFar.splitFirstWord.mapLeft(r => premise.attemptMatch(theoremBuilder.resolveReference(r)))
    }
    val matchResult = Statement.mergeMatchAttempts(matchAttempts)
      .getOrElse(throw ParseException.withMessage(s"Could not match theorem hypotheses", line.fullLine))
    val missingStatementVariables = result.statementVariables.diff(matchResult.keys.toSeq)
    val (missingStatementMatch, lineAfterStatements) = readMissingStatements(missingStatementVariables, lineAfterHypotheses, context)
    val expandedMatch = missingStatementMatch ++ matchResult
    val step = Step(result.replace(expandedMatch))
    (step, lineAfterStatements)
  }
}

trait TheoremLineParser {
  def name: String
  def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder

  def readMissingStatements(statementVariables: Seq[Int], line: PartialLine, context: Context): (Map[Int, Statement], PartialLine) = {
    statementVariables.mapFold(line) { (statementVariable, lineSoFar) =>
      Statement.parse(lineSoFar, context).mapLeft(statementVariable -> _)
    }.mapLeft(_.toMap)
  }
}

object HypothesisParser extends TheoremLineParser {
  override val name: String = "hypothesis"
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (hypothesis, _) = Statement.parse(line, context)
    theoremBuilder.addHypothesis(hypothesis)
  }
}

object FantasyHypothesisParser extends TheoremLineParser {
  override val name: String = "assume"
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
    val (name, title) = firstLine.splitFirstWord.mapRight(_.remainingText)
    def parseLine(
      line: BookLine,
      theoremBuilder: TheoremBuilder
    ): TheoremBuilder = {
      try {
        val parsers = Seq(HypothesisParser, FantasyHypothesisParser) ++
          context.rules ++
          context.connectives.flatMap(_.definition) ++
          context.theorems
        val (lineType, restOfLine) = line.splitFirstWord
        val parser = parsers.find(_.name == lineType).getOrElse(throw new Exception(s"Unrecognised theorem line '$lineType'"))
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
  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(theorems = context.theorems :+ theorem)
  }
}
