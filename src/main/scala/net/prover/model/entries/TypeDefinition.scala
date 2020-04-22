package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions.{ExpressionDefinition, Qualifier, StatementDefinition}
import net.prover.model.expressions.Statement

case class TypeDefinition(
    symbol: String,
    defaultTermName: String,
    qualifier: Option[Qualifier],
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone with ChapterEntry.CanChangeOptionalName
{
  override val name: String = explicitName.getOrElse(symbol)
  override val title: String = s"Type Definition: ${name.capitalizeWords}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry)

  def withSymbol(newSymbol: String): TypeDefinition = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): TypeDefinition = copy(explicitName = newName)
  def withFormat(newFormat: Format.Explicit): TypeDefinition = copy(qualifier = qualifier.map(_.withFormat(newFormat)))

  @JsonSerialize
  val article: String = if (name.headOption.exists("aeiou".contains(_))) "an" else "a"
  def baseFormat = Format.Explicit(s"%1 is $article %0", s"$defaultTermName is $article $name", 2, true, true)
  def fullFormat = qualifier.prependFormat(baseFormat)

  val allTermNames: Seq[String] = defaultTermName +: qualifier.termNames
  val allComponents: Seq[TermComponent] = allTermNames.map(ComponentType.TermComponent(_, Nil))
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    symbol,
    allComponents,
    explicitName,
    fullFormat,
    Some(definingStatement),
    this)
  override def inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("type", symbol, defaultTermName).mkString(" ") +:
      (qualifier.map("qualifier " + _.serialized).toSeq ++
        explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
        Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
      ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): TypeDefinition = {
    TypeDefinition(
      symbol,
      defaultTermName,
      qualifier,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements))
  }
}

object TypeDefinition extends ChapterEntryParser {
  override def name: String = "type"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      defaultTermName <- Parser.singleWord
      qualifier <- Parser.optional("qualifier", Qualifier.parser)
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultTermName +: qualifier.termNames)).inParens)
    } yield TypeDefinition(symbol, defaultTermName, qualifier, explicitName, definingStatement)
  }
}
