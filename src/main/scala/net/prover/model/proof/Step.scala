package net.prover.model.proof

import net.prover.model._
import net.prover.model.components.{BoundVariable, Statement, Term, TermVariable}

sealed trait Step {
  def reference: Reference.Direct
  def fact: Option[Fact]
  def referencedFact: Option[ReferencedFact] = fact.map { f => ReferencedFact(f, reference)}
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def matchesOutline(stepOutline: StepOutline): Boolean
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
  def serialized: String = serializedLines.mkString("\n")
  def serializedLines: Seq[String]
}

object Step {

  sealed trait WithProvenStatement extends Step {
    def statement: Statement
    override def fact = Some(Fact.Direct(statement))
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[Step],
      reference: Reference.Direct)
    extends Step
  {
    def assumptionReference = reference.withSuffix("a")
    override val fact = steps.ofType[Step.WithProvenStatement].lastOption.map(lastSubstep => Fact.Deduced(assumption, lastSubstep.statement))
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Assumption(`assumption`, stepOutlines) =>
        Step.matchOutlines(steps, stepOutlines)
      case _ =>
        false
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      steps.flatMap(_.getAssertionHints(availableInferences))
    }
    override def serializedLines = s"assume ${assumption.serialized} {" +: steps.flatMap(_.serializedLines).indent :+ "}"
  }
  object Assumption {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.Assumption] = {
      for {
        assumption <- Statement.parser
        steps <- Step.listParser(Some(reference)).inBraces
      } yield Step.Assumption(assumption, steps, reference)
    }
  }

  case class Assertion(
      statement: Statement,
      inferenceApplication: InferenceApplication,
      reference: Reference.Direct,
      isRearrangement: Boolean)
    extends Step.WithProvenStatement
  {
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
    override def referenceMap: ReferenceMap = ReferenceMap(reference.value -> inferenceApplication.directReferences)
    override def matchesOutline(stepOutline: StepOutline): Boolean = Step.matchAssertionOutline(statement, stepOutline)
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      inferenceApplication.getAssertionHints(availableInferences)
    }
    override def serializedLines: Seq[String] = {
      s"assert ${if (isRearrangement) "rearranging " else ""}${statement.serialized}" +: inferenceApplication.serializedLines.indent
    }
  }
  object Assertion {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.Assertion] = {
      for {
        isRearrangement <- Parser.optionalWord("rearranging").isDefined
        assertion <- Statement.parser
        inferenceApplication <- InferenceApplication.parser
      } yield {
        Step.Assertion(assertion, inferenceApplication, reference, isRearrangement)
      }
    }
  }

  case class Naming(
      variable: TermVariable,
      assumptionStep: Step.Assumption,
      assertionStep: Step.WithProvenStatement,
      reference: Reference.Direct)
    extends Step.WithProvenStatement
  {
    override def statement: Statement = assertionStep.statement
    override def referencedInferenceIds: Set[String] = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def referenceMap: ReferenceMap = assertionStep.referenceMap
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.Naming(`variable`, statement, stepOutlines) =>
        assumptionStep.assumption == statement && Step.matchOutlines(assumptionStep.steps, stepOutlines)
      case _ =>
        false
    }
    def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      assumptionStep.getAssertionHints(availableInferences) ++ assertionStep.getAssertionHints(availableInferences)
    }
    override def serializedLines = {
      Seq(s"name ${variable.text} ${assumptionStep.assumption.serialized} {") ++
        assumptionStep.steps.flatMap(_.serializedLines).indent ++
        Seq("}") ++
        assertionStep.serializedLines
    }
  }
  object Naming {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.Naming] = {
      for {
        variable <- Term.variableParser
        assumptionStep <- Assumption.parser(reference)
        _ <- Parser.requiredWord("assert")
        assertionStep <- Assertion.parser(reference)
      } yield Step.Naming(variable, assumptionStep, assertionStep, reference)
    }
  }

  case class ScopedVariable(boundVariableName: String, substeps: Seq[Step], reference: Reference.Direct) extends Step {
    override def fact: Option[Fact] = {
      substeps.ofType[Step.WithProvenStatement].lastOption
        .map(_.statement)
        .map(Fact.Bound(_)(boundVariableName))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def matchesOutline(stepOutline: StepOutline): Boolean = stepOutline match {
      case StepOutline.ScopedVariable(`boundVariableName`, substepOutlines) =>
        Step.matchOutlines(substeps, substepOutlines)
      case _ =>
        false
    }
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      substeps.flatMap(_.getAssertionHints(availableInferences))
    }
    def serializedLines: Seq[String] = Seq(s"take $boundVariableName {") ++ substeps.flatMap(_.serializedLines).indent ++ Seq("}")
  }
  object ScopedVariable {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Step.ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addBoundVariable(variableName)
        steps <- listParser(Some(reference))(updatedContext).inBraces
      } yield Step.ScopedVariable(variableName, steps, reference)
    }
  }

  def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext): Parser[Option[Step]] = {
    Parser.selectOptionalWord {
      case "assume" => Assumption.parser(reference)
      case "assert" => Assertion.parser(reference)
      case "name" => Naming.parser(reference)
      case "take" => ScopedVariable.parser(reference)
    }
  }
  def listParser(baseReference: Option[Reference.Direct])(implicit parsingContext: ParsingContext): Parser[Seq[Step]] = {
    Parser.iterateWhileDefined((Seq.empty[Step], 0)) { case (steps, index) =>
      parser(Reference.nextReference(baseReference, index.toString)).mapMap {step =>
        (steps :+ step, index + 1)
      }
    }.map(_._1)
  }

  def matchOutlines(steps: Seq[Step], stepOutlines: Seq[StepOutline]): Boolean = {
    steps.zipStrict(stepOutlines).exists(_.forall { case (step, stepOutline) =>
      step.matchesOutline(stepOutline)
    })
  }
  def matchAssertionOutline(provenStatement: Statement, stepOutline: StepOutline): Boolean = stepOutline match {
    case StepOutline.Assertion(`provenStatement`, _, _) =>
      true
    case _ =>
      false
  }
}
