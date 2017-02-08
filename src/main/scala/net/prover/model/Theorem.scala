package net.prover.model

import scala.util.control.NonFatal

case class Theorem(
    id: String,
    title: String,
    premises: Seq[Statement],
    steps: Seq[Step],
    conclusion: Statement,
    arbitraryVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables)
  extends ChapterEntry(Theorem) with Inference
{
  val assumption = None
}

trait TheoremLineParser {
  def id: String
  def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder

  protected def readReference(line: PartialLine, theoremBuilder: TheoremBuilder): (Statement, PartialLine) = {
    line.splitFirstWord.mapLeft(theoremBuilder.resolveReference)
  }
}

object PremiseParser extends TheoremLineParser {
  override val id: String = "premise"
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (premise, _) = Statement.parse(line, context)
    theoremBuilder.addPremise(premise)
  }
}

object FantasyAssumptionParser extends TheoremLineParser {
  override val id: String = "assume"
  override def readAndUpdateTheoremBuilder(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): TheoremBuilder = {
    val (assumption, _) = Statement.parse(line, context)
    theoremBuilder.addFantasy(assumption)
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
        val parsers = Seq(PremiseParser, FantasyAssumptionParser) ++ context.theoremLineParsers
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
        case BookLine("qed", _, _, _) +: nonTheoremLines =>
          import theoremBuilder._
          if (fantasyOption.isDefined)
            throw new Exception("Cannot finish theorem with open assumption")
          val conclusion = steps.last.statement
          val variables = (premises.map(_.variables) :+ conclusion.variables).reduce(_ ++ _)
          val theorem = Theorem(
            id,
            title,
            premises,
            steps,
            conclusion,
            arbitraryVariables.intersect(variables.termVariables),
            distinctVariables.filter(variables.statementVariables.contains, variables.termVariables.contains))
          (theorem, nonTheoremLines)
        case definitionLine +: otherLines =>
          parseHelper(otherLines, parseLine(definitionLine, theoremBuilder))
        case Nil =>
          throw new Exception("Book ended in middle of theorem")
      }
    }
    parseHelper(lines, TheoremBuilder())
  }
  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ theorem)
  }
}
