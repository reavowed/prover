package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}

case class PropertyDefinitionOnType(
    symbol: String,
    parentType: TypeDefinition,
    requiredParentQualifier: Option[TypeQualifierDefinition],
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: StatementDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.CanChangeOptionalName
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize} ${parentType.name.capitalize}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.associatedChapterEntry + parentType

  def withName(newName: Option[String]): PropertyDefinitionOnType = copy(explicitName = newName)

  def baseFormat = Format.Explicit(s"%1 is %0", s"${parentType.defaultTermName} is $name", 2, true, true)
  def fullFormat = parentType.qualifier.prependFormat(baseFormat)

  def allTermNames = requiredParentQualifier.map(q => q.allTermNames).getOrElse(parentType.allTermNames)
  def definingStatementWithParentRequirement = {
    val parentStatement = parentType.statementDefinition(parentType.allTermNames.map(TermVariable(_, Nil)): _*)
    val parentStatementWithQualifier = requiredParentQualifier.map(q => conjunctionDefinition(parentStatement, q.statementDefinition(q.allTermNames.map(TermVariable(_, Nil)): _*))).getOrElse(parentStatement)
    conjunctionDefinition(parentStatementWithQualifier, definingStatement)
  }

  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    allTermNames.map(ComponentType.TermComponent(_, Nil)),
    Some(name),
    fullFormat,
    Some(definingStatementWithParentRequirement),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("property", symbol, "on", parentType.symbol).mkString(" ") +:
    (requiredParentQualifier.map(q => Seq("parentQualifier", q.symbol).mkString(" ")).toSeq ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): PropertyDefinitionOnType = {
    PropertyDefinitionOnType(
      symbol,
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      requiredParentQualifier.map(q => entryReplacements(q).asInstanceOf[TypeQualifierDefinition]),
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryContext.conjunctionDefinitionOption.get)
  }
}

object PropertyDefinitionOnType extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentType <- Parser.required("on", context.typeDefinitionParser)
      requiredParentQualifier <- Parser.optional("parentQualifier", Parser.singleWord.map(qualifierSymbol => context.qualifiersByType(parentType.symbol).find(_.symbol == qualifierSymbol).getOrElse(throw new Exception(s"Unrecognised qualifier '$qualifierSymbol'"))))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, requiredParentQualifier.map(_.allTermNames).getOrElse(parentType.allTermNames))).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create property definition without conjunction"))
    } yield PropertyDefinitionOnType(symbol, parentType, requiredParentQualifier, explicitName, definingStatement, conjunctionDefinition)
  }
}
