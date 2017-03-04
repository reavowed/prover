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
  override def parser(implicit context: Context): Parser[Theorem] = {
    for {
      id <- Parser.singleWord
      title <- Parser.toEndOfLine
      theorem <- parseLines(id, title, TheoremBuilder())
    } yield {
      theorem
    }
  }

  def parseLines(
    id: String,
    title: String,
    theoremBuilder: TheoremBuilder)(
    implicit context: Context
  ): Parser[Theorem] = {
    val lineParsers = Seq(PremiseParser, FantasyAssumptionParser) ++ context.theoremLineParsers
    Parser.singleWord flatMap {
      case "qed" =>
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
        Parser.constant(theorem)
      case lineType =>
        val lineParser = lineParsers.find(_.id == lineType)
          .getOrElse(t(lineType))
        lineParser.parser(theoremBuilder).flatMap(parseLines(id, title, _))
    }
  }

  def t(lineType: String) = throw new Exception(s"Unrecognised theorem line '$lineType'")

  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ theorem)
  }
}
