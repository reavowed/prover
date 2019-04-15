package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.{ComponentType, TermComponent}
import net.prover.model.expressions.Statement

case class TypeDefinition(
    symbol: String,
    defaultTermName: String,
    otherComponentTypes: Seq[ComponentType],
    componentFormat: Format.Explicit,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry]

  override def serializedLines: Seq[String] = Seq("type", symbol, defaultTermName, otherComponentTypes.map(_.serialized).mkString(" ").inParens).mkString(" ") +:
    (Seq(Seq("format", componentFormat.serialized.value.inParens).mkString(" ")) ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  def statementDefinition = StatementDefinition(symbol, Nil, TermComponent(defaultTermName) +: otherComponentTypes, explicitName, componentFormat, Some(definingStatement), None, Nil)
}

object TypeDefinition extends ChapterEntryParser {
  override def name: String = "type"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      defaultTermName <- Parser.singleWord
      otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.inParens
      componentFormat <- Parser.required("format", Format.parser(otherComponentTypes.map(_.name)))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultTermName +: otherComponentTypes.ofType[TermComponent].map(_.name))).inParens)
    } yield TypeDefinition(symbol, defaultTermName, otherComponentTypes, componentFormat, explicitName, definingStatement)
  }
}
