package net.prover.model

case class ProofOutline(steps: Seq[ProofOutline.Step])

object ProofOutline {
  sealed trait Step
  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step])
    extends Step
  case class AssertionStep(
      assertion: Statement)
    extends Step

  private def assumptionStepParser(implicit context: Context): Parser[AssumptionStep] = {
    for {
      assumption <- Statement.parser
      steps <- stepsParser.inBraces
    } yield {
      AssumptionStep(assumption, steps)
    }
  }

  private def assertionStepParser(implicit context: Context): Parser[AssertionStep] = {
    Statement.parser.map(AssertionStep)
  }

  private def stepParser(implicit context: Context): Parser[Option[Step]] = {
    Parser.singleWord.flatMap {
      case "assume" =>
        assumptionStepParser.map(Some.apply)
      case "prove" =>
        assertionStepParser.map(Some.apply)
      case _ =>
        Parser.constant(None)
    }
  }

  private def stepsParser(implicit context: Context): Parser[Seq[Step]] = {
    stepParser.collectWhileDefined
  }

  def parser(implicit context: Context): Parser[ProofOutline] = {
    stepsParser.map(ProofOutline.apply)
  }
}