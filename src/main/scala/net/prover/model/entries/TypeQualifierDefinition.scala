package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions.{ExpressionDefinition, Qualifier, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}

case class TypeQualifierDefinition(
    symbol: String,
    parentType: TypeDefinition,
    qualifier: Qualifier,
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: StatementDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.CanChangeOptionalName
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${parentType.name.capitalize} ${name.capitalize}"
  def qualifiedSymbol: String = parentType.symbol + symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.associatedChapterEntry + parentType

  def withName(newName: Option[String]): TypeQualifierDefinition = copy(explicitName = newName)

  def fullFormat = qualifier.prependFormat(Format.Explicit(s"%1 is", s"${parentType.defaultTermName} is", 2, true, true))
  def allTermNames = parentType.defaultTermName +: qualifier.termNames
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    allTermNames.map(ComponentType.TermComponent(_, Nil)),
    Some(name),
    fullFormat,
    Some(conjunctionDefinition(parentType.statementDefinition(parentType.allTermNames.map(TermVariable(_, Nil)): _*), definingStatement)),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("qualifier", symbol, "on", parentType.symbol, qualifier.serialized).mkString(" ") +:
    (explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): TypeQualifierDefinition = {
    TypeQualifierDefinition(
      symbol,
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      qualifier,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryContext.conjunctionDefinitionOption.get)
  }
}

object TypeQualifierDefinition extends ChapterEntryParser {
  override def name: String = "qualifier"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentType <- Parser.required("on", context.typeDefinitionParser)
      qualifier <- Qualifier.parser
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, parentType.defaultTermName +: qualifier.termNames)).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create type qualifier definition without conjunction"))
    } yield TypeQualifierDefinition(symbol, parentType, qualifier, explicitName, definingStatement, conjunctionDefinition)
  }
}
