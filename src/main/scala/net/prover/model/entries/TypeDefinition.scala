package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.{ComponentType, TermComponent}
import net.prover.model.expressions.Statement

case class TypeDefinition(
    name: String,
    defaultSymbol: String,
    otherComponentTypes: Seq[ComponentType],
    componentFormat: Format.Explicit,
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def title: String = s"Definition: ${name.capitalize}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry]

  override def serializedLines: Seq[String] = Seq("type", name, defaultSymbol, otherComponentTypes.map(_.serialized).mkString(" ").inParens).mkString(" ") +:
    Seq(
      Seq("format", componentFormat.serialized.value.inParens).mkString(" "),
      Seq("definition", definingStatement.serialized.inParens).mkString(" ")
    ).indent
}

object TypeDefinition extends ChapterEntryParser {
  override def name: String = "type"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      name <- Parser.singleWord
      defaultSymbol <- Parser.singleWord
      otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.inParens
      _ <- Parser.requiredWord("format")
      componentFormat <- Format.parser(otherComponentTypes.map(_.name))
      _ <- Parser.requiredWord("definition")
      definingStatement <- Statement.parser(ExpressionParsingContext.outsideProof(context, defaultSymbol +: otherComponentTypes.ofType[TermComponent].map(_.name))).inParens
    } yield TypeDefinition(name, defaultSymbol, otherComponentTypes , componentFormat, definingStatement)
  }
}
