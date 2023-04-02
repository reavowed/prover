package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.expressions._

trait ExpressionDefinitionEntry extends TypedExpressionDefinitionEntry[ExpressionDefinitionEntry] with ChapterEntry.Standalone

trait TypedExpressionDefinitionEntry[+ExpressionDefinitionType <: ExpressionDefinitionEntry] extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ExpressionDefinition { self: ExpressionDefinitionEntry =>
  def typeName: String
  override def name: String = super[ExpressionDefinition].name
  override def format: Format.Basic
  override def title: String = s"$typeName Definition: $name"
  override def associatedChapterEntry: ChapterEntry = this

  def withSymbol(newSymbol: String): ExpressionDefinitionType
  def withName(newName: Option[String]): ExpressionDefinitionType
  def withShorthand(newShorthand: Option[String]): ExpressionDefinitionType
  def withAttributes(newAttributes: Seq[String]): ExpressionDefinitionType
  def withFormat(newFormat: Format.Basic): ExpressionDefinitionType

  protected def serializedComponents = "(" + (boundVariableNames.map("$" + _) ++ componentTypes.map(_.serialized)).mkString(" ") + ")"
}

object ExpressionDefinitionEntry {
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
