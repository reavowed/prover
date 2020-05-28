package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ConjunctionDefinition, ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}

case class RelatedObjectDefinition(
    symbol: String,
    parentType: TypeDefinition,
    defaultTermName: String,
    requiredParentQualifier: Option[TypeQualifierDefinition],
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: ConjunctionDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition with ChapterEntry.HasArticle
{
  override def title: String = s"Definition: ${name.capitalizeWords} for ${parentType.name.capitalizeWords}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.referencedEntry + parentType

  override def withSymbol(newSymbol: String): RelatedObjectDefinition = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): ChapterEntry = copy(explicitName = newName)

  def baseFormat = Format.Explicit(s"%1 is $article %0 for %2", s"$defaultTermName is $article $name for ${parentType.mainTermName}", 3, true, true)
  def fullFormat = parentType.qualifier.prependFormat(baseFormat)
  def parentTermNames = requiredParentQualifier.map(_.allTermNames) getOrElse parentType.allTermNames
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    (defaultTermName +: parentTermNames).map(ComponentType.TermComponent(_, Nil)),
    Some(name),
    fullFormat,
    Some(conjunctionDefinition(parentType.statementDefinition(parentType.allTermNames.indices.map(i => TermVariable(i + 1, Nil)): _*), definingStatement)),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): ChapterEntry = {
    RelatedObjectDefinition(
      symbol,
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      defaultTermName,
      requiredParentQualifier.map(q => entryReplacements(q).asInstanceOf[TypeQualifierDefinition]),
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryContext.conjunctionDefinitionOption.get)
  }

  override def serializedLines: Seq[String] = Seq(RelatedObjectDefinition.name, symbol, defaultTermName, "for", parentType.symbol).mkString(" ") +:
    (requiredParentQualifier.map(q => Seq("parentQualifier", q.symbol).mkString(" ")).toSeq ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
      ).indent
}

object RelatedObjectDefinition extends ChapterEntryParser {
  override def name: String = "relatedObject"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      defaultTermName <- Parser.singleWord
      parentType <- Parser.required("for", context.typeDefinitionParser)
      requiredParentQualifier <- parentType.parentQualifierParser
      explicitName <- Parser.optional("name", Parser.allInParens)
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(defaultTermName +: requiredParentQualifier.map(_.allTermNames).getOrElse(parentType.allTermNames))
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create related object definition without conjunction"))
    } yield RelatedObjectDefinition(symbol, parentType, defaultTermName, requiredParentQualifier, explicitName, definingStatement, conjunctionDefinition)
  }
}
