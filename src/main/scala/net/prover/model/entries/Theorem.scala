package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.books.model.EntryParsingContext
import net.prover.controllers.models.StepWithReferenceChange
import net.prover.model._
import net.prover.model.definitions.{Definitions, ExpressionDefinition}
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.theorems.ReplaceDefinitions

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

  def isComplete(definitions: Definitions): Boolean = proofs.exists(_.isComplete(definitions))
  def initialStepContext: StepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)

  def recalculateReferences(provingContext: ProvingContext): (Theorem, Seq[Seq[StepWithReferenceChange]]) = {
    proofs.map(_.recalculateReferences(initialStepContext, provingContext, conclusion)).split.mapLeft(newProofs => copy(proofs = newProofs))
  }

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

  def clearInference(inference: Inference): Theorem = {
    copy(proofs = proofs.map(_.clearInference(inference)))
  }
  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): Theorem = {
    ReplaceDefinitions(this, expressionDefinitionReplacements)(entryContext)
  }
}

object Theorem extends Inference.EntryParser {
  override val name: String = "theorem"

  case class Proof(steps: Seq[Step]) {
    def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    def referencedDefinitions: Set[ExpressionDefinition] = steps.flatMap(_.referencedDefinitions).toSet
    def isComplete(definitions: Definitions): Boolean = steps.forall(_.isComplete(definitions))

    def recalculateReferences(initialStepContext: StepContext, provingContext: ProvingContext, expectedConclusion: Statement): (Proof, Seq[StepWithReferenceChange]) = {
      val (newSteps, changedSteps) = steps.recalculateReferences(initialStepContext, provingContext)
      val newStepsWithTarget = if (newSteps.mapCollect(_.provenStatement).lastOption.contains(expectedConclusion)) newSteps else newSteps :+ Step.Target(expectedConclusion)
      (Proof(newStepsWithTarget), changedSteps)
    }

    def clearInference(inference: Inference): Proof = {
      Proof(steps.clearInference(inference))
    }

    def serialized: String = steps.flatMap(_.serializedLines).mkString("\n") + "\n"
  }

  def proofParser(
    theoremName: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement)(
    implicit entryContext: EntryContext
  ): Parser[Proof] = {
    val initialStepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)
    for {
      steps <- Step.listParser(entryContext, initialStepContext)
      _ = if (!steps.mapCollect(_.provenStatement).lastOption.contains(conclusion)) throw new Exception(s"Proof of theorem '$theoremName' did not prove $conclusion")
    } yield Proof(steps)
  }

  override def parser(implicit context: EntryParsingContext): Parser[Theorem] = {

    for {
      name <- Parser.toEndOfLine
      variableDefinitions <- VariableDefinitions.parser
      expressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
      premises <- premisesParser(expressionParsingContext)
      conclusion <- conclusionParser(expressionParsingContext)
      serializedProofs = context.proofFileReader.getSerializedProofs(name)
      proofs = serializedProofs.mapWithIndex((proof, i) => proofParser(name, variableDefinitions, premises, conclusion).parseFromString(proof, s"proof ${i + 1}"))
    } yield Theorem(name, variableDefinitions, premises, conclusion, proofs)
  }
}
