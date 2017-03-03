package net.prover.model

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
  def parser(theoremBuilder: TheoremBuilder)(implicit context: Context): Parser[TheoremBuilder]
  def readAndUpdateTheoremBuilder(
    theoremBuilder: TheoremBuilder,
    line: PartialLine)(
    implicit context: Context
  ): TheoremBuilder = {
    parser(theoremBuilder).parseAndDiscard(line)
  }

  protected def referenceParser(theoremBuilder: TheoremBuilder): Parser[Statement] = {
    Parser.singleWord.map(theoremBuilder.resolveReference)
  }
}

object PremiseParser extends TheoremLineParser {
  override val id: String = "premise"

  override def parser(theoremBuilder: TheoremBuilder)(implicit context: Context): Parser[TheoremBuilder] = {
    for {
      premise <- Statement.parser
    } yield {
      theoremBuilder.addPremise(premise)
    }
  }
}

object FantasyAssumptionParser extends TheoremLineParser {
  override val id: String = "assume"
  override def parser(theoremBuilder: TheoremBuilder)(implicit context: Context): Parser[TheoremBuilder] = {
    for {
      assumption <- Statement.parser
    } yield {
      theoremBuilder.addFantasy(assumption)
    }
  }
}

object Theorem extends ChapterEntryParser[Theorem] {
  override val name: String = "theorem"
  override def parser(lines: Seq[BookLine])(implicit context: Context): Parser[(Theorem, Seq[BookLine])] = {
    for {
      id <- Parser.singleWord
      title <- Parser.allRemaining
    } yield {
      parseLines(id, title, lines, TheoremBuilder())
    }
  }

  def lineParser(
    theoremBuilder: TheoremBuilder)(
    implicit context: Context
  ): Parser[TheoremBuilder] = {
    val lineParsers = Seq(PremiseParser, FantasyAssumptionParser) ++ context.theoremLineParsers
    for {
      lineType <- Parser.singleWord
      lineParser = lineParsers.find(_.id == lineType).getOrElse(throw new Exception(s"Unrecognised theorem line '$lineType'"))
      updatedTheoremBuilder <- lineParser.parser(theoremBuilder)
    } yield {
      updatedTheoremBuilder
    }
  }

  def parseLines(
    id: String,
    title: String,
    linesRemaining: Seq[BookLine],
    theoremBuilder: TheoremBuilder)(
    implicit context: Context
  ): (Theorem, Seq[BookLine]) = {
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
        val updatedTheoremBuilder = lineParser(theoremBuilder).parseAndDiscard(definitionLine)
        parseLines(id, title, otherLines, updatedTheoremBuilder)
      case Nil =>
        throw new Exception("Book ended in middle of theorem")
    }
  }

  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ theorem)
  }
}
