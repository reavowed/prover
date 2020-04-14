package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.expressions.Statement

case class StandalonePropertyDefinition(
    symbol: String,
    defaultTermName: String,
    otherComponentTypes: Seq[ComponentType],
    componentFormat: Format.Explicit,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize}"
  def qualifiedSymbol: String = symbol

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry]

  def fullFormat: Format = Format.Explicit(s"$defaultTermName is $name", Seq(defaultTermName), requiresBrackets = false, requiresComponentBrackets = true)
  val statementDefinition: StatementDefinition = StatementDefinition(
    qualifiedSymbol,
    Nil,
    TermComponent(defaultTermName, Nil) +: otherComponentTypes,
    explicitName.orElse(Some(symbol)),
    fullFormat,
    Some(definingStatement),
    None,
    Nil)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = (Seq(StandalonePropertyDefinition.name, symbol, defaultTermName) :+ otherComponentTypes.map(_.serialized).mkString(" ").inParens).mkString(" ") +:
    (Seq(componentFormat.serialized.value) ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): StandalonePropertyDefinition = {
    StandalonePropertyDefinition(
      symbol,
      defaultTermName,
      otherComponentTypes,
      componentFormat,
      explicitName,
      definingStatement.replaceDefinition(oldDefinition, newDefinition))
  }
}

object StandalonePropertyDefinition extends ChapterEntryParser {
  override def name: String = "standaloneProperty"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      defaultTermName <- Parser.singleWord
      otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.inParens
      componentFormat <- Parser.required("format", Format.parserForTypeDefinition(otherComponentTypes))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultTermName +: otherComponentTypes.ofType[TermComponent].map(_.name))).inParens)
    } yield StandalonePropertyDefinition(symbol, defaultTermName, otherComponentTypes, componentFormat, explicitName, definingStatement)
  }
}


