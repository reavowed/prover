package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.books.reading.ProofFileReader
import net.prover.entries.{EntryWithContext, TheoremWithContext}
import net.prover.model._
import net.prover.model.definitions.{Definitions, ExpressionDefinition}
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.theorems.{IsComplete, ReplaceDefinitions}

import scala.reflect.ClassTag

@JsonIgnoreProperties(Array("rearrangementType"))
case class Theorem(
    name: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement,
    proofs: Seq[Proof])
  extends Inference.Entry
{
  override def withName(newName: String): Theorem = copy(name = newName)
  override def referencedInferenceIds: Set[String] = proofs.flatMap(_.referencedInferenceIds).toSet
  override def referencedEntries: Set[ChapterEntry] =  ((premises :+ conclusion).flatMap(_.referencedDefinitions).toSet ++ proofs.flatMap(_.referencedDefinitions).toSet).map(_.associatedChapterEntry)
  override def inferences: Seq[Inference.FromEntry] = Seq(this)

  def isComplete(entryWithContext: EntryWithContext): Boolean = IsComplete(entryWithContext.asInstanceOf[TheoremWithContext])
  def initialStepContext: StepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)

  def findSteps[T <: Step : ClassTag]: Seq[(T, StepContext)] = {
    def forStep(step: Step, context: StepContext): Seq[(T, StepContext)] = {
      step match {
        case assertion: T =>
          Seq((assertion, context))
        case stepWithSubsteps: Step.WithSubsteps =>
          forSteps(stepWithSubsteps.substeps, stepWithSubsteps.specifyStepContext(context))
        case _ =>
          Nil
      }
    }
    def forSteps(steps: Seq[Step], context: StepContext): Seq[(T, StepContext)] = {
      steps.zipWithIndex.flatMap { case (step, index) => forStep(step, context.atIndex(index))}
    }
    proofs.flatMap(proof => forSteps(proof.steps, initialStepContext))
  }

  override def serializedLines: Seq[String] = Seq(s"theorem $name") ++
    variableDefinitions.serializedLines ++
    premises.map("premise " + _.serialized) ++
    Seq("conclusion " + conclusion.serialized)

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): Theorem = {
    ReplaceDefinitions(expressionDefinitionReplacements, entryWithContext.availableEntries)(entryWithContext.copy(entry = this))
  }
}

object Theorem extends Inference.EntryParser {
  override val name: String = "theorem"

  case class Proof(steps: Seq[Step]) {
    def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    def referencedDefinitions: Set[ExpressionDefinition] = steps.flatMap(_.referencedDefinitions).toSet
    def isComplete(definitions: Definitions): Boolean = steps.forall(_.isComplete(definitions))
    def serialized: String = steps.flatMap(_.serializedLines).mkString("\n") + "\n"
  }

  def proofParser(
    theoremName: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement)(
    implicit availableEntries: AvailableEntries
  ): Parser[Proof] = {
    val initialStepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)
    for {
      steps <- Step.listParser(availableEntries, initialStepContext)
      _ = if (!steps.mapCollect(_.provenStatement).lastOption.contains(conclusion)) throw new Exception(s"Proof of theorem '$theoremName' did not prove $conclusion")
    } yield Proof(steps)
  }

  override def parser(implicit availableEntries: AvailableEntries, proofFileReader: ProofFileReader): Parser[Theorem] = {

    for {
      name <- Parser.toEndOfLine
      variableDefinitions <- VariableDefinitions.parser
      expressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
      premises <- premisesParser(expressionParsingContext)
      conclusion <- conclusionParser(expressionParsingContext)
      serializedProofs = proofFileReader.getSerializedProofs(name)
      proofs = serializedProofs.mapWithIndex((proof, i) => proofParser(name, variableDefinitions, premises, conclusion).parseFromString(proof, s"proof ${i + 1}"))
    } yield Theorem(name, variableDefinitions, premises, conclusion, proofs)
  }
}
