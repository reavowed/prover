package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.books.reading.ProofFileReader
import net.prover.entries.{ChapterWithContext, EntryParsingContext, EntryWithContext, TheoremWithContext}
import net.prover.model._
import net.prover.model.definitions.{Definitions, ExpressionDefinition}
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.theorems.{GetReferencedDefinitions, IsComplete, ReplaceDefinitions}

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
  override def referencedEntries: Set[ChapterEntry] = {
    ((premises :+ conclusion).flatMap(_.referencedDefinitions).toSet ++
      proofs.flatMap(GetReferencedDefinitions(_)).toSet).map(_.associatedChapterEntry)
  }
  override def inferences: Seq[Inference.FromEntry] = Seq(this)

  def isComplete(entryWithContext: EntryWithContext): Boolean = IsComplete(entryWithContext.asInstanceOf[TheoremWithContext])
  def initialStepContext(implicit provingContext: ProvingContext): StepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)

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
    def serialized: String = steps.flatMap(_.serializedLines).mkString("\n") + "\n"
  }

  def proofParser(
    theoremName: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement)(
    implicit provingContext: ProvingContext
  ): Parser[Proof] = {
    val initialStepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)
    for {
      steps <- Step.listParser(initialStepContext, provingContext)
      _ = if (!steps.lastOption.exists(_.statement == conclusion)) throw new Exception(s"Proof of theorem '$theoremName' did not prove $conclusion")
    } yield Proof(steps)
  }

  override def parser(implicit entryParsingContext: EntryParsingContext): Parser[Theorem] = {
    import entryParsingContext._
    for {
      name <- Parser.toEndOfLine
      variableDefinitions <- VariableDefinitions.parser
      expressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
      premises <- premisesParser(expressionParsingContext)
      conclusion <- conclusionParser(expressionParsingContext)
      serializedProofs = proofFileReader.getSerializedProofs(name)
      proofs = serializedProofs.mapWithIndex((proof, i) => proofParser(name, variableDefinitions, premises, conclusion).parseFromString(proof, s"Book '${bookTitle}' chapter '${chapterTitle}' theorem ${name} proof ${i + 1}"))
    } yield Theorem(name, variableDefinitions, premises, conclusion, proofs)
  }
}
