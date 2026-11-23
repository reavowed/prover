package net.prover.model.entries

import net.prover.entries.{EntryParsingContext, EntryWithContext}
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.parsing.Parser
import net.prover.proving.structure.definitions.ConjunctionDefinition

/**
  * A relation between two objects of (possibly different) types. If either type has a default qualifier, the relation
  * names the variables to use for that qualifier. A name may be shared between the two qualifiers, or may be the other
  * type's main variable, in which case they refer to the same variable. Any qualifier variables that are not one of the
  * main variables become additional components of the relation's statement, after the two main variables.
  */
case class TypeRelationDefinition(
    symbol: String,
    firstType: TypeDefinition,
    secondType: TypeDefinition,
    firstVariable: SimpleVariableDefinition,
    secondVariable: SimpleVariableDefinition,
    firstQualifierVariableNames: Seq[String],
    secondQualifierVariableNames: Seq[String],
    linkingPhrase: String,
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: ConjunctionDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  override val title: String = s"Definition: ${name.capitalizeWords}"

  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.referencedEntry + firstType + secondType

  override def withSymbol(newSymbol: String): ChapterEntry = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): ChapterEntry = copy(explicitName = newName)

  def allVariableDefinitions: Seq[SimpleVariableDefinition] = TypeRelationDefinition.variableDefinitions(firstVariable, secondVariable, firstQualifierVariableNames, secondQualifierVariableNames)
  val qualifierVariableDefinitions: Seq[SimpleVariableDefinition] = allVariableDefinitions.drop(2)
  def allVariableNames: Seq[String] = allVariableDefinitions.map(_.name)
  def allComponents: Seq[TermComponent] = allVariableNames.map(ComponentType.TermComponent(_, Nil))
  def firstQualifierIndices: Seq[Int] = firstQualifierVariableNames.map(allVariableNames.indexOf)
  def secondQualifierIndices: Seq[Int] = secondQualifierVariableNames.map(allVariableNames.indexOf)

  private def qualifierFormatString(typeDefinition: TypeDefinition, replacement: Int => String): Option[String] = {
    typeDefinition.defaultQualifier.map(q => "%(\\d+)".r.replaceAllIn(q.format.baseFormatString, m => java.util.regex.Matcher.quoteReplacement(replacement(m.group(1).toInt))))
  }
  /** The text of each qualifier in terms of the relation's variable names, e.g. "on A". */
  val firstQualifierText: Option[String] = qualifierFormatString(firstType, i => firstQualifierVariableNames(i))
  val secondQualifierText: Option[String] = qualifierFormatString(secondType, i => secondQualifierVariableNames(i))

  def format: Format.Explicit = {
    def build(firstQualifier: Option[String], secondQualifier: Option[String], firstName: String, secondName: String): String = {
      // If both qualifiers say the same thing, only state it once at the end
      val (q1, q2) = if (firstQualifier.isDefined && firstQualifier == secondQualifier) (None, secondQualifier) else (firstQualifier, secondQualifier)
      (Seq(firstName) ++ q1.toSeq ++ Seq(linkingPhrase, secondName) ++ q2.toSeq).mkString(" ")
    }
    val firstBaseQualifier = qualifierFormatString(firstType, i => s"%${firstQualifierIndices(i) + 1}")
    val secondBaseQualifier = qualifierFormatString(secondType, i => s"%${secondQualifierIndices(i) + 1}")
    Format.Explicit(
      build(firstBaseQualifier, secondBaseQualifier, "%1", "%2"),
      build(firstQualifierText, secondQualifierText, firstVariable.name, secondVariable.name),
      allVariableNames.length + 1,
      true,
      true)
  }

  override def validate(): Unit = {
    if (firstQualifierVariableNames.length != firstType.defaultQualifier.variableDefinitions.length)
      throw new Exception(s"Type relation $symbol must give ${firstType.defaultQualifier.variableDefinitions.length} qualifier variables for ${firstType.symbol}")
    if (secondQualifierVariableNames.length != secondType.defaultQualifier.variableDefinitions.length)
      throw new Exception(s"Type relation $symbol must give ${secondType.defaultQualifier.variableDefinitions.length} qualifier variables for ${secondType.symbol}")
  }

  private def firstTypeStatement: Statement = firstType.statementDefinition(TermVariable(0, Nil) +: firstQualifierIndices.map(TermVariable(_, Nil)): _*)
  private def secondTypeStatement: Statement = secondType.statementDefinition(TermVariable(1, Nil) +: secondQualifierIndices.map(TermVariable(_, Nil)): _*)

  override val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    symbol,
    allComponents,
    Some(name),
    format,
    Some(conjunctionDefinition(firstTypeStatement, conjunctionDefinition(secondTypeStatement, definingStatement))),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq(
    "typeRelation",
    symbol,
    linkingPhrase.inParens,
    firstType.symbol,
    TypeRelationDefinition.serializeVariable(firstVariable, firstQualifierVariableNames),
    secondType.symbol,
    TypeRelationDefinition.serializeVariable(secondVariable, secondQualifierVariableNames)
  ).mkString(" ") +:
    ( explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq :+
      Seq("definition", definingStatement.serialized.inParens).mkString(" ")).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): ChapterEntry = {
    TypeRelationDefinition(
      symbol,
      entryReplacements(firstType).asInstanceOf[TypeDefinition],
      entryReplacements(secondType).asInstanceOf[TypeDefinition],
      firstVariable,
      secondVariable,
      firstQualifierVariableNames,
      secondQualifierVariableNames,
      linkingPhrase,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryWithContext.availableEntries.conjunctionDefinitionOption.get)
  }
}


object TypeRelationDefinition extends ChapterEntryParser {
  override def name: String = "typeRelation"

  /** e.g. `∘ (infix-function) A` - the main variable definition, then any qualifier variable names */
  def serializeVariable(variableDefinition: SimpleVariableDefinition, qualifierVariableNames: Seq[String]): String = {
    (variableDefinition.serialized +: qualifierVariableNames).mkString(" ")
  }
  private def variableParser(typeDefinition: TypeDefinition): Parser[(SimpleVariableDefinition, Seq[String])] = {
    for {
      variableDefinition <- SimpleVariableDefinition.parser
      qualifierVariableNames <- Parser.nWords(typeDefinition.defaultQualifier.variableDefinitions.length)
    } yield (variableDefinition, qualifierVariableNames)
  }

  override def parser(implicit entryParsingContext: EntryParsingContext): Parser[TypeRelationDefinition] = {
    import entryParsingContext.availableEntries
    for {
      symbol <- Parser.singleWord
      linkingPhrase <- Parser.allInParens
      firstType <- availableEntries.typeDefinitionParser
      (firstVariableDefinition, firstQualifierVariableNames) <- variableParser(firstType)
      secondType <- availableEntries.typeDefinitionParser
      (secondVariableDefinition, secondQualifierVariableNames) <- variableParser(secondType)
      explicitName <- Parser.optional("name", Parser.allInParens)
      variableDefinitions = TypeRelationDefinition.variableDefinitions(firstVariableDefinition, secondVariableDefinition, firstQualifierVariableNames, secondQualifierVariableNames)
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(variableDefinitions)
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
      conjunctionDefinition = availableEntries.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create type qualifier definition without conjunction"))
    } yield TypeRelationDefinition(symbol, firstType, secondType, firstVariableDefinition, secondVariableDefinition, firstQualifierVariableNames, secondQualifierVariableNames, linkingPhrase, explicitName, definingStatement, conjunctionDefinition)
  }

  def variableDefinitions(
    firstVariable: SimpleVariableDefinition,
    secondVariable: SimpleVariableDefinition,
    firstQualifierVariableNames: Seq[String],
    secondQualifierVariableNames: Seq[String]
  ): Seq[SimpleVariableDefinition] = {
    Seq(firstVariable, secondVariable) ++ (firstQualifierVariableNames ++ secondQualifierVariableNames)
      .distinct
      .filter(n => n != firstVariable.name && n != secondVariable.name)
      .map(SimpleVariableDefinition(_, Nil))
  }
}
