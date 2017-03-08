package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

case class Theorem(
    name: String,
    premises: Seq[Premise],
    steps: Seq[Step],
    conclusion: ProvenStatement)
  extends ChapterEntry(Theorem)
    with Inference
{
  val id = calculateHash()
}

sealed trait Step

case class AssumptionStep(
    assumption: Statement,
    steps: Seq[Step])
  extends Step

case class AssertionStep(
    provenStatement: ProvenStatement,
    inference: Inference,
    references: Seq[Reference],
    substitutions: Substitutions)
  extends Step

sealed trait Reference
case class DirectReference(index: Int) extends Reference
case class DeducedReference(antecedentIndex: Int, consequentIndex: Int) extends Reference

case class ReferencedAssertion(provenStatement: ProvenStatement, reference: DirectReference)
case class ReferencedDeduction(assumption: Statement, deduction: ProvenStatement, reference: Reference)

sealed trait MatchedPremise {
  def provenStatement: ProvenStatement
  def reference: Reference
}
case class MatchedDirectPremise(
    premise: DirectPremise,
    provenStatement: ProvenStatement,
    reference: DirectReference)
  extends MatchedPremise
case class MatchedDeducedPremise(
    premise: DeducedPremise,
    assumption: Statement,
    provenStatement: ProvenStatement,
    reference: Reference)
  extends MatchedPremise

case class Prover(
    theoremPremises: Seq[Premise],
    activeAssumptions: Seq[Statement],
    previousSteps: Seq[Step])(
    implicit context: Context)
{
  def availableInferences = context.inferences

  def proveAssertion(assertion: Statement): AssertionStep = {
    availableInferences.mapCollect { inference =>
      inference.conclusion.statement.calculateSubstitutions(assertion, Substitutions.empty).map(inference -> _)
    }.mapCollect { case (inference, substitutions) =>
      matchInferencePremises(inference, substitutions).map(inference -> _)
    }.map { case (inference, (matchedPremises, substitutions)) =>
      makeAssertionStep(assertion, inference, matchedPremises, substitutions)
    }.headOption.getOrElse(throw new Exception(s"Could not prove statement $assertion"))
  }

  private def getProvenStatements: Seq[ReferencedAssertion] = {
    theoremPremises.zipWithIndex.collect {
      case (DirectPremise(statement), index) =>
        (ProvenStatement.withNoConditions(statement), index)
    } ++ activeAssumptions.zipWithIndex.map {
      case (statement, index) =>
        (ProvenStatement.withNoConditions(statement), index + theoremPremises.length)
    } ++ previousSteps.zipWithIndex.collect {
      case (AssertionStep(provenStatement, _, _, _), index) =>
        (provenStatement, index + theoremPremises.length + activeAssumptions.length)
    } map { case (provenStatement, index) =>
      ReferencedAssertion(provenStatement, DirectReference(index))
    }
  }

  private def getProvenDeductions: Seq[ReferencedDeduction] = {
    theoremPremises.zipWithIndex.collect {
      case (DeducedPremise(assumption, deduction), index) =>
        ReferencedDeduction(assumption, ProvenStatement.withNoConditions(deduction), DirectReference(index))
    } ++ previousSteps.zipWithIndex.collectMany {
      case (AssumptionStep(assumption, substeps), assumptionIndex) =>
        substeps.zipWithIndex.collect {
          case (AssertionStep(deduction, _ , _, _), assertionIndex) =>
            val reference = DeducedReference(theoremPremises.length + activeAssumptions.length + assumptionIndex, assertionIndex)
            ReferencedDeduction(assumption, deduction, reference)
        }
    }
  }

  private def matchInferencePremises(
    inference: Inference,
    substitutions: Substitutions
  ): Option[(Seq[MatchedPremise], Substitutions)] = {
    val initial = Seq((Seq.empty[MatchedPremise], substitutions))
    inference.premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
        matchPremise(premise, substitutionsSoFar).map { case (matchedPremise, newSubstitutions) =>
          (matchedPremisesSoFar :+ matchedPremise, newSubstitutions)
        }
      }
    }.headOption
  }

  private def matchPremise(
    inferencePremise: Premise,
    substitutionsSoFar: Substitutions
  ): Seq[(MatchedPremise, Substitutions)] = {
    inferencePremise match {
      case directPremise @ DirectPremise(inferencePremiseStatement) =>
        getProvenStatements.map { case ReferencedAssertion(provenStatement, reference) =>
          inferencePremiseStatement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
            .map((MatchedDirectPremise(directPremise, provenStatement, reference), _))
        } collectDefined
      case deducedPremise @ DeducedPremise(antecedent, consequent) =>
        getProvenDeductions.map { case ReferencedDeduction(assumption, deduction, reference) =>
          antecedent.calculateSubstitutions(assumption, substitutionsSoFar)
            .flatMap(consequent.calculateSubstitutions(deduction.statement, _))
            .map((MatchedDeducedPremise(deducedPremise, assumption, deduction, reference), _))
        } collectDefined
    }
  }

  private def makeAssertionStep(
    assertion: Statement,
    inference: Inference,
    matchedPremises: Seq[MatchedPremise],
    substitutions: Substitutions
  ): AssertionStep = {
    val substitutedInference = inference.applySubstitutions(substitutions)
    val arbitraryVariables = matchedPremises.map(_.provenStatement.arbitraryVariables)
      .foldLeft(substitutedInference.conclusion.arbitraryVariables)(_ ++ _)
    val distinctVariables = matchedPremises.map(_.provenStatement.distinctVariables)
      .foldLeft(substitutedInference.conclusion.distinctVariables)(_ ++ _)
    val provenStatement = ProvenStatement(assertion, arbitraryVariables, distinctVariables)
    AssertionStep(provenStatement, inference, matchedPremises.map(_.reference), substitutions)
  }
}

object Theorem extends ChapterEntryParser[Theorem] with InferenceParser {
  override val name: String = "theorem"

  private def assumptionParser(
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    previousSteps: Seq[Step])(
    implicit context: Context
  ): Parser[AssumptionStep] = {
    for {
      assumption <- Statement.parser
      steps <- stepsParser(premises, assumptions :+ assumption, previousSteps).inBraces
    } yield {
      AssumptionStep(assumption, steps)
    }
  }

  private def assertionParser(
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    previousSteps: Seq[Step])(
    implicit context: Context
  ): Parser[AssertionStep] = {
    for {
      assertion <- Statement.parser
    } yield {
      Prover(premises, assumptions, previousSteps).proveAssertion(assertion)
    }
  }

  private def stepParser(
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    previousSteps: Seq[Step],
    stepsSoFar: Seq[Step])(
    implicit context: Context
  ): Parser[Option[Step]] = {
    Parser.singleWord.flatMap[Option[Step]] {
      case "assume" =>
        assumptionParser(premises, assumptions, previousSteps ++ stepsSoFar).map(Some.apply)
      case "prove" =>
        assertionParser(premises, assumptions, previousSteps ++ stepsSoFar).map(Some.apply)
      case _ =>
        Parser.constant(None)
    }
  }

  private def stepsParser(
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    previousSteps: Seq[Step])(
    implicit context: Context
  ): Parser[Seq[Step]] = {
    Parser.collectWhileDefined[Step] { stepsSoFar =>
      stepParser(premises, assumptions, previousSteps, stepsSoFar)
    }
  }

  override def parser(implicit context: Context): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      premises <- premisesParser
      steps <- stepsParser(premises, Nil, Nil)
      _ <- Parser.singleWord.onlyIf(_ == "qed").throwIfUndefined("Expected step or qed")
    } yield {
      val lastProvenStatement = steps.ofType[AssertionStep].lastOption
        .getOrElse(throw new Exception("Theorem must contain at least one assertion"))
        .provenStatement
      Theorem(
        name,
        premises,
        steps,
        lastProvenStatement)
    }
  }

  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(inferences = context.inferences :+ theorem)
  }
}
