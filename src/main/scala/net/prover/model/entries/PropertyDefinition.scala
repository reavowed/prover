package net.prover.model.entries

import net.prover.model.entries.ExpressionDefinition.{ComponentType, TermComponent}
import net.prover.model.expressions.Statement
import net.prover.model._

case class PropertyDefinition(
    name: String,
    parentType: TypeDefinition,
    defaultSymbol: String,
    parentComponentTypes: Seq[ComponentType],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def title: String = s"Definition: ${name.capitalize} ${parentType.name.capitalize}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry] + parentType

  override def serializedLines: Seq[String] = (Seq("property", name, "on", parentType.name, defaultSymbol) ++ parentComponentTypes.map(_.serialized)).mkString(" ") +:
    Seq(
      Seq("definition", definingStatement.serialized.inParens).mkString(" ")
    ).indent
}

object PropertyDefinition extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      name <- Parser.singleWord
      _ <- Parser.requiredWord("on")
      parentTypeName <- Parser.singleWord
      parentType = context.typeDefinitions.find(_.name == parentTypeName).getOrElse(throw new Exception(s"Unrecognised type '$parentTypeName'"))
      defaultSymbol <- Parser.singleWord
      parentComponentTypes <- parentType.otherComponentTypes.map(t => Parser.singleWord.map(t.withName)).traverseParser
      _ <- Parser.requiredWord("definition")
      definingStatement <- Statement.parser(ExpressionParsingContext.outsideProof(context, defaultSymbol +: parentComponentTypes.ofType[TermComponent].map(_.name))).inParens
    } yield PropertyDefinition(name, parentType, defaultSymbol, parentComponentTypes, definingStatement)
  }

}
