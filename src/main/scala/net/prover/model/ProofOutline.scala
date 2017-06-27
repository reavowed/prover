package net.prover.model

case class ProofOutline(steps: Seq[ProofOutline.Step])

object ProofOutline {
  sealed trait Step
  sealed trait StepWithAssertion extends Step {
    def assertion: Statement
  }
  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step])
    extends Step
  case class NamingStep(
      termVariable: TermVariable,
      statement: Statement,
      steps: Seq[Step])
    extends StepWithAssertion {
    override def assertion: Statement = steps.ofType[StepWithAssertion].lastOption
      .getOrElse(throw new Exception("Naming step must contain a step with an assertion"))
      .assertion
  }
  case class AssertionStep(
      assertion: Statement,
      nonArbitraryVariables: Set[TermVariable],
      nonDistinctVariables: Set[(TermVariable, Variable)],
      debug: Boolean = false)
    extends StepWithAssertion

  private def assumptionStepParser(implicit context: Context): Parser[AssumptionStep] = {
    for {
      assumption <- Statement.parser
      steps <- stepsParser.inBraces
    } yield {
      AssumptionStep(assumption, steps)
    }
  }

  private def namingStepParser(implicit context: Context): Parser[NamingStep] = {
    for {
      termVariable <- Term.variableParser
      statement <- Statement.parser
      steps <- stepsParser.inBraces
    } yield {
      NamingStep(termVariable, statement, steps)
    }
  }

  private def assertionStepParser(implicit context: Context): Parser[AssertionStep] = {
    for {
      assertion <- Statement.parser
      nonArbitraryVariables <- Parser.optional("non-arbitrary", Term.variableParser.listInParens(None).map(_.toSet), Set.empty[TermVariable])
      nonDistinctVariables <- Parser.optional("non-distinct", Conditions.variablePairListParser.map(_.toSet), Set.empty[(TermVariable, Variable)])
      debug <- Parser.optional("debug", Parser.constant(true), false)
    } yield AssertionStep(assertion, nonArbitraryVariables, nonDistinctVariables, debug)
  }

  private def stepParser(implicit context: Context): Parser[Option[Step]] = {
    Parser.singleWord.flatMap {
      case "assume" =>
        assumptionStepParser.map(Some.apply)
      case "let" =>
        namingStepParser.map(Some.apply)
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
