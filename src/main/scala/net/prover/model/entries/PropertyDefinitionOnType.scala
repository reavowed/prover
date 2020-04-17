package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.expressions.{Statement, TermVariable}

case class PropertyDefinitionOnType(
    symbol: String,
    parentType: TypeDefinition,
    defaultTermName: String,
    parentTermNames: Seq[String],
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize} ${parentType.name.capitalize}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry] + parentType

  def fullFormat: Format = Format.Explicit(s"$defaultTermName is $name ${parentType.componentFormat.originalValue}", symbol +: defaultTermName +: parentTermNames, requiresBrackets = false, requiresComponentBrackets = true)
  val statementDefinition: StatementDefinition = StatementDefinition(
    qualifiedSymbol,
    Nil,
    TermComponent(defaultTermName, Nil) +: parentTermNames.map(ComponentType.TermComponent(_, Nil)),
    explicitName.orElse(Some(symbol)),
    fullFormat,
    Some(definingStatement),
    None,
    Nil)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = (Seq("property", symbol, "on", parentType.name, defaultTermName) ++ parentTermNames).mkString(" ") +:
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
      defaultTermName,
      parentTermNames,
      explicitName,
      definingStatement.replaceDefinition(oldDefinition, newDefinition))
  }
}

object PropertyDefinitionOnType extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentTypeName <- Parser.required("on", Parser.singleWord)
      parentType = context.typeDefinitions.find(_.name == parentTypeName).getOrElse(throw new Exception(s"Unrecognised type '$parentTypeName'"))
      defaultTermName <- Parser.singleWord
      parentTermNames <- parentType.childTermNamesParser
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultTermName +: parentComponentTypes.ofType[TermComponent].map(_.name))).inParens)
    } yield PropertyDefinitionOnType(symbol, parentType, defaultTermName, parentTermNames, explicitName, definingStatement)
  }
}
