package net.prover.structure.model.entries

import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType

trait CompoundExpressionDefinitionEntry extends TypedExpressionDefinitionEntry[CompoundExpressionDefinitionEntry] with ChapterEntry.Standalone

trait TypedExpressionDefinitionEntry[+ExpressionDefinitionType <: CompoundExpressionDefinitionEntry] extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with CompoundExpressionDefinition { self: CompoundExpressionDefinitionEntry =>
  def typeName: String
  override def name: String = super[CompoundExpressionDefinition].name
  override def format: Format.Basic
  override def title: String = s"$typeName Definition: $name"
  override def referencedInferenceIds: Set[String] = Set.empty
  override def associatedChapterEntry: ChapterEntry = this

  def withSymbol(newSymbol: String): ExpressionDefinitionType
  def withName(newName: Option[String]): ExpressionDefinitionType
  def withShorthand(newShorthand: Option[String]): ExpressionDefinitionType
  def withAttributes(newAttributes: Seq[String]): ExpressionDefinitionType
  def withFormat(newFormat: Format.Basic): ExpressionDefinitionType

  protected def serializedComponents = "(" + (boundVariableNames.map("$" + _) ++ componentTypes.map(_.serialized)).mkString(" ") + ")"
}

object CompoundExpressionDefinitionEntry {
  private def boundVariablesParser: Parser[Seq[String]] = {
    val boundVariablePattern = "\\$(.*)".r
    Parser.selectOptionalWord {
      case boundVariablePattern(variableName) => variableName
    }.whileDefined
  }

  def rawBoundVariablesAndComponentTypesParser: Parser[(Seq[String], Seq[ComponentType])] = {
    for {
      boundVariables <- boundVariablesParser
      componentTypes <- ComponentType.listParser(boundVariables)
    } yield (boundVariables, componentTypes)
  }

  def boundVariablesAndComponentTypesParser: Parser[(Seq[String], Seq[ComponentType])] = {
    rawBoundVariablesAndComponentTypesParser.inParens
  }

  def shorthandParser: Parser[Option[String]] = Parser.optional("shorthand", Parser.allInParens)
}
