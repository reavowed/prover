package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}

case class PropertyDefinitionOnType(
    symbol: String,
    parentType: TypeDefinition,
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: StatementDefinition)
  extends ChapterEntry.Standalone
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize} ${parentType.name.capitalize}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.associatedChapterEntry + parentType

  def baseFormat = Format.Explicit(s"%1 is %0", s"${parentType.defaultTermName} is $name", 2, true, true)
  def fullFormat = parentType.qualifier match {
    case Some(q) =>
      Format.Concatenated(baseFormat, q.format)
    case None =>
      baseFormat
  }
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    parentType.allComponents,
    Some(explicitName.getOrElse(symbol)),
    fullFormat,
    Some(conjunctionDefinition(parentType.statementDefinition(parentType.allTermNames.map(TermVariable(_, Nil)): _*), definingStatement)),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("property", symbol, "on", parentType.name).mkString(" ") +:
    (explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): PropertyDefinitionOnType = {
    PropertyDefinitionOnType(
      symbol,
      entryContext.typeDefinitions.find(_.symbol == parentType.symbol).get,
      explicitName,
      definingStatement.replaceDefinition(oldDefinition, newDefinition),
      conjunctionDefinition)
  }
}

object PropertyDefinitionOnType extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentTypeName <- Parser.required("on", Parser.singleWord)
      parentType = context.typeDefinitions.find(_.name == parentTypeName).getOrElse(throw new Exception(s"Unrecognised type '$parentTypeName'"))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, parentType.allTermNames)).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create property definition without conjunction"))
    } yield PropertyDefinitionOnType(symbol, parentType, explicitName, definingStatement, conjunctionDefinition)
  }
}
