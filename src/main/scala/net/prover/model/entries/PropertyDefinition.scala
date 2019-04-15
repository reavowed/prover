package net.prover.model.entries

import net.prover.model.entries.ExpressionDefinition.{ComponentType, TermComponent}
import net.prover.model.expressions.Statement
import net.prover.model._

case class PropertyDefinition(
    name: String,
    parentType: TypeDefinition,
    defaultTermName: String,
    parentComponentTypes: Seq[ComponentType],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def title: String = s"Definition: ${name.capitalize} ${parentType.name.capitalize}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry] + parentType

  override def serializedLines: Seq[String] = (Seq("property", name, "on", parentType.name, defaultTermName) ++ parentComponentTypes.map(_.serialized)).mkString(" ") +:
    Seq(
      Seq("definition", definingStatement.serialized.inParens).mkString(" ")
    ).indent
}

object PropertyDefinition extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      name <- Parser.singleWord
      parentTypeName <- Parser.required("on", Parser.singleWord)
      parentType = context.typeDefinitions.find(_.name == parentTypeName).getOrElse(throw new Exception(s"Unrecognised type '$parentTypeName'"))
      defaultSymbol <- Parser.singleWord
      parentComponentTypes <- parentType.otherComponentTypes.map(t => Parser.singleWord.map(t.withName)).traverseParser
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultSymbol +: parentComponentTypes.ofType[TermComponent].map(_.name))).inParens)
    } yield PropertyDefinition(name, parentType, defaultSymbol, parentComponentTypes, definingStatement)
  }

}
