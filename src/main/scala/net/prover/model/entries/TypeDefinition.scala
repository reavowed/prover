package net.prover.model.entries

import net.prover.books.reading.ProofFileReader
import net.prover.entries.EntryWithContext
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions.{ExpressionDefinition, Qualifier, StatementDefinition}
import net.prover.model.expressions.Statement

case class TypeDefinition(
    symbol: String,
    mainVariableDefinition: SimpleVariableDefinition,
    defaultQualifier: Option[Qualifier],
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
    with ChapterEntry.HasOptionalExplicitName
    with ChapterEntry.HasStatementDefinition
    with ChapterEntry.HasArticle
    with ChapterEntry.HasMainVariable
    with ChapterEntry.HasDefiningStatement
{
  override val title: String = s"Definition: ${name.capitalizeWords}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry)

  def withSymbol(newSymbol: String): TypeDefinition = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): TypeDefinition = copy(explicitName = newName)
  def withFormat(newFormat: Format.Explicit): TypeDefinition = copy(defaultQualifier = defaultQualifier.map(_.withFormat(newFormat)))
  def withMainVariableDefinition(newMainVariableDefinition: SimpleVariableDefinition): TypeDefinition = copy(mainVariableDefinition = newMainVariableDefinition)
  override def withDefiningStatement(newDefiningStatement: Statement) = copy(definingStatement = newDefiningStatement)

  override def definingStatementParsingContext(implicit entryContext: EntryContext) = {
    ExpressionParsingContext.forTypeDefinition(allVariableDefinitions)
  }

  def baseFormat = Format.Explicit(s"%1 is $article %0", s"${mainVariableDefinition.name} is $article $name", 2, true, true)
  def fullFormat = defaultQualifier.prependFormat(baseFormat)

  val allVariableDefinitions: Seq[SimpleVariableDefinition] = mainVariableDefinition +: defaultQualifier.variableDefinitions
  val allVariableNames: Seq[String] = allVariableDefinitions.map(_.name)
  val allComponents: Seq[TermComponent] = allVariableNames.map(ComponentType.TermComponent(_, Nil))
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    symbol,
    allComponents,
    explicitName,
    fullFormat,
    Some(definingStatement),
    this)
  override def inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("type", symbol, mainVariableDefinition.serialized).mkString(" ") +:
      (defaultQualifier.map("qualifier " + _.serialized).toSeq ++
        explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
        Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
      ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): TypeDefinition = {
    TypeDefinition(
      symbol,
      mainVariableDefinition,
      defaultQualifier,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements))
  }

  def parentQualifierParser(implicit entryContext: EntryContext): Parser[Option[TypeQualifierDefinition]] = {
    Parser.optional("parentQualifier", Parser.singleWord.map(qualifierSymbol => entryContext.qualifiersByType(symbol).find(_.symbol == qualifierSymbol).getOrElse(throw new Exception(s"Unrecognised qualifier '$qualifierSymbol'"))))
  }
}

object TypeDefinition extends ChapterEntryParser {
  override def name: String = "type"
  override def parser(implicit entryContext: EntryContext, proofFileReader: ProofFileReader): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      mainVariableDefinition <- SimpleVariableDefinition.parser
      qualifier <- Parser.optional("qualifier", Qualifier.parser)
      explicitName <- Parser.optional("name", Parser.allInParens)
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(mainVariableDefinition +: qualifier.variableDefinitions)
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
    } yield TypeDefinition(symbol, mainVariableDefinition, qualifier, explicitName, definingStatement)
  }
}
