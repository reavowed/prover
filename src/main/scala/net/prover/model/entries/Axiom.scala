package net.prover.model.entries

import net.prover.entries.{EntryParsingContext, EntryWithContext}
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.Statement
import net.prover.parsing.Parser

case class Axiom(
    name: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Inference.Entry
{
  override def withName(newName: String): Axiom = copy(name = newName)
  override def referencedEntries: Set[ChapterEntry] = (premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions).map(_.associatedChapterEntry)
  override def isComplete(entryWithContext: EntryWithContext): Boolean = true
  override def inferences: Seq[Inference.FromEntry] = Seq(this)
  override def serializedLines: Seq[String] = {
    Seq(s"axiom $name") ++
      variableDefinitions.serializedLines ++
      premises.map("premise " + _.serialized) ++
      Seq(s"conclusion ${conclusion.serialized}")
  }

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): Axiom = {
    Axiom(
      name,
      variableDefinitions,
      premises.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      conclusion.replaceDefinitions(expressionDefinitionReplacements))
  }
}

object Axiom extends Inference.EntryParser {
  override val name: String = "axiom"

  override def parser(implicit entryParsingContext: EntryParsingContext): Parser[Axiom] = {
    import entryParsingContext.availableEntries
    for {
      name <- Parser.toEndOfLine
      variableDefinitions <- VariableDefinitions.parser
      expressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
      premises <- premisesParser(expressionParsingContext)
      conclusion <- conclusionParser(expressionParsingContext)
    } yield Axiom(name, variableDefinitions, premises, conclusion)
  }
  override def toString = name
}
