package net.prover.views

import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions._
import net.prover.model._

import scala.util.control.NonFatal
import scala.xml.{Elem, Node, NodeSeq}

object ExpressionView {
  def apply(expression: Expression)(implicit displayContext: DisplayContext): NodeSeq = {
    apply(expression, internalPath = Nil, safe = false)
  }

  private def apply(expression: Expression, internalPath: Seq[Int], safe: Boolean)(implicit displayContext: DisplayContext): NodeSeq = {
    displayContext.displayShorthands
      .mapFind(withShorthand(expression, internalPath, _, safe))
      .getOrElse(directly(expression, internalPath, safe))
  }

  private def withShorthand(
    expression: Expression,
    internalPath: Seq[Int],
    shorthand: DisplayShorthand,
    safe: Boolean)(
    implicit displayContext: DisplayContext
  ): Option[Elem] = {
    for {
      rawComponents <- shorthand.template.matchExpression(expression)
      components = rawComponents.map {
        case Template.Match.BoundVariable(name, index, variableInternalPath) =>
          boundVariable(name, index, variableInternalPath)
        case Template.Match.Component(e, boundVariableNames, componentInternalPath) =>
          apply(e, internalPath ++ componentInternalPath, safe = false)(displayContext.withBoundVariableLists(boundVariableNames))
      }
    } yield formatted(shorthand.format, components, internalPath, safe)
  }

  private def directly(
    expression: Expression,
    internalPath: Seq[Int],
    safe: Boolean)(
    implicit displayContext: DisplayContext
  ): NodeSeq = {
    expression match {
      case ExpressionVariable(text) => leaf(text)
      case DefinedExpression(definition, boundVariableNames, components) =>
        formatted(
          definition.format,
          boundVariableNames.mapWithIndex((name, index) => boundVariable(name, index, Nil)) ++ components.mapWithIndex { (component, index) => {
            ExpressionView(
              component,
              internalPath :+ index,
              safe = true)(
              displayContext.withBoundVariableList(boundVariableNames))
          }},
          internalPath,
          safe)
      case ExpressionApplication(text, arguments) => formatted(
        Format.Default(text + "(" + arguments.indices.map(i => s"%$i").mkString(", ") + ")", requiresBrackets = false),
        arguments.mapWithIndex((argument, index) => {
          ExpressionView(argument, internalPath :+ index, safe = true)
        }),
        internalPath,
        safe)
      case functionParameter: FunctionParameter =>
        try {
          leaf(displayContext.boundVariableNames(functionParameter.level)(functionParameter.index))
        } catch {
          case NonFatal(e) =>
            throw e
        }
    }
  }

  private def leaf(text: String): Seq[Node] = {
    HtmlHelper.format(text)
  }
  private def boundVariable(text: String, index: Int, furtherPath: Seq[Int]): Elem = {
    <span class="boundVariable" data-index={index.toString} data-further-path={if (furtherPath.nonEmpty) furtherPath.mkString(".") else null}>{HtmlHelper.format(text)}</span>
  }
  private def formatted(format: Format, components: Seq[NodeSeq], internalPath: Seq[Int], safe: Boolean): Elem = {
    <span data-path={internalPath.mkString(".")}>{format.formatHtml(components, safe)}</span>
  }
}
