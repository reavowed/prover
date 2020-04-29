package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition, TermListAdapter}
import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.model.proof.SubstitutionContext

case class PropertyDefinitionOnType(
    symbol: String,
    parentType: TypeDefinition,
    requiredParentQualifier: Option[TypeQualifierDefinition],
    termListAdapter: Option[TermListAdapter],
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: StatementDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  override def title: String = s"Definition: ${name.capitalizeWords} ${parentType.name.capitalizeWords}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.associatedChapterEntry + parentType

  override def withSymbol(newSymbol: String): PropertyDefinitionOnType = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): PropertyDefinitionOnType = copy(explicitName = newName)

  def baseFormat = Format.Explicit(s"%1 is %0", s"${parentType.defaultTermName} is $name", 2, true, true)
  def fullFormat = parentType.qualifier.prependFormat(baseFormat)

  def allTermNames = termListAdapter.map(_.termNames) orElse requiredParentQualifier.map(q => q.allTermNames) getOrElse parentType.allTermNames
  def parentTypeCondition = {
    val mainTerm = TermVariable(parentType.defaultTermName, Nil)
    val qualifierTerms = termListAdapter match {
      case Some(adapter) =>
        val adapterTerms = adapter.termNames.map(TermVariable(_, Nil))
        adapter.templates.map(_.specify(adapterTerms)(SubstitutionContext.outsideProof).get)
      case None =>
        val termNames = requiredParentQualifier match {
          case Some(qualifier) =>
            qualifier.qualifier.termNames
          case None =>
            parentType.qualifier.termNames
        }
        termNames.map(TermVariable(_, Nil))
    }
    requiredParentQualifier match {
      case Some(qualifier) =>
        val parentStatement = parentType.statementDefinition(mainTerm)
        val qualifierStatement = qualifier.statementDefinition(mainTerm +: qualifierTerms: _*)
        conjunctionDefinition(parentStatement, qualifierStatement)
      case None =>
        parentType.statementDefinition(mainTerm +: qualifierTerms: _*)
    }
  }

  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    allTermNames.map(ComponentType.TermComponent(_, Nil)),
    Some(name),
    fullFormat,
    Some(conjunctionDefinition(parentTypeCondition, definingStatement)),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("property", symbol, "on", parentType.symbol).mkString(" ") +:
    (requiredParentQualifier.map(q => Seq("parentQualifier", q.symbol).mkString(" ")).toSeq ++
      termListAdapter.map(a => Seq("termListAdapter", a.serialized).mkString(" ")).toSeq ++
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
      termListAdapter.map(_.replaceDefinitions(expressionDefinitionReplacements)),
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
      requiredParentQualifier <- parentType.parentQualifierParser
      termListAdapter <- Parser.optional("termListAdapter", TermListAdapter.parser)
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, requiredParentQualifier.map(_.allTermNames).getOrElse(parentType.allTermNames))).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create property definition without conjunction"))
    } yield PropertyDefinitionOnType(symbol, parentType, requiredParentQualifier, termListAdapter, explicitName, definingStatement, conjunctionDefinition)
  }
}
