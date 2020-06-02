package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ConjunctionDefinition, ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}

case class RelatedObjectDefinition(
    symbol: String,
    parentType: TypeDefinition,
    mainVariableDefinition: SimpleVariableDefinition,
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

  def baseFormat: Format.Explicit = Format.Explicit(s"%1 is $article %0 for %2", s"${mainVariableDefinition.name} is $article $name for ${parentType.mainVariableDefinition}", 3, true, true)
  def fullFormat: Format = parentType.defaultQualifier.prependFormat(baseFormat)
  def parentVariableDefinitions: Seq[SimpleVariableDefinition] = requiredParentQualifier.map(_.allVariableDefinitions) getOrElse parentType.allVariableDefinitions
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    (mainVariableDefinition +: parentVariableDefinitions).map(_.name).map(ComponentType.TermComponent(_, Nil)),
    Some(name),
    fullFormat,
    Some(conjunctionDefinition(parentType.statementDefinition(parentType.allVariableNames.indices.map(i => TermVariable(i + 1, Nil)): _*), definingStatement)),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): ChapterEntry = {
    RelatedObjectDefinition(
      symbol,
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      mainVariableDefinition,
      requiredParentQualifier.map(q => entryReplacements(q).asInstanceOf[TypeQualifierDefinition]),
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryContext.conjunctionDefinitionOption.get)
  }

  override def serializedLines: Seq[String] = Seq(RelatedObjectDefinition.name, symbol, mainVariableDefinition.serialized, "for", parentType.symbol).mkString(" ") +:
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
      mainVariableDefinition <- SimpleVariableDefinition.parser
      parentType <- Parser.required("for", context.typeDefinitionParser)
      requiredParentQualifier <- parentType.parentQualifierParser
      explicitName <- Parser.optional("name", Parser.allInParens)
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(mainVariableDefinition +: requiredParentQualifier.map(_.allVariableDefinitions).getOrElse(parentType.allVariableDefinitions))
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create related object definition without conjunction"))
    } yield RelatedObjectDefinition(symbol, parentType, mainVariableDefinition, requiredParentQualifier, explicitName, definingStatement, conjunctionDefinition)
  }
}
