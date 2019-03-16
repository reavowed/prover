package net.prover.views

import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions._
import net.prover.model._

import scala.xml.Elem

object ExpressionView {
  def apply(expression: Expression, referrers: Set[(String, Seq[Int])] = Set.empty, safe: Boolean = false)(implicit displayContext: DisplayContext): Elem = {
    val topLevelReferrers = referrers.filter(_._2.isEmpty).map(_._1)
    displayContext.displayShorthands
      .mapFind(withShorthand(expression, topLevelReferrers, _, safe))
      .getOrElse(directly(expression, referrers, topLevelReferrers, safe))
  }

  private def withShorthand(
    expression: Expression,
    referrers: Set[String],
    shorthand: DisplayShorthand,
    safe: Boolean)(
    implicit displayContext: DisplayContext
  ): Option[Elem] = {
    for {
      rawComponents <- shorthand.template.matchExpression(expression)
      components = rawComponents.map {
        case Left(name) => leaf(name, Set.empty)
        case Right(e) => apply(e, Set.empty)
      }
    } yield formatted(shorthand.format, components, referrers, safe)
  }

  private def directly(
    expression: Expression,
    referrers: Set[(String, Seq[Int])],
    topLevelReferrers: Set[String],
    safe: Boolean)(
    implicit displayContext: DisplayContext
  ): Elem = {
    expression match {
      case ExpressionVariable(text) => leaf(text, topLevelReferrers)
      case DefinedExpression(definition, boundVariableNames, components) =>
        formatted(
          definition.format,
          boundVariableNames.map(leaf(_, Set.empty)) ++ components.mapWithIndex { (component, index) => {
            apply(component, referrers.filter(_._2.headOption.contains(index)).map(_.mapRight(_.tail)), safe = true)
          }},
          topLevelReferrers,
          safe)
      case ExpressionApplication(text, arguments) => formatted(
        Format.Default(text + "(" + arguments.indices.map(i => s"%$i").mkString(", ") + ")", requiresBrackets = false),
        arguments.mapWithIndex((argument, index) => {
          apply(argument, referrers.filter(_._2.headOption.contains(index)).map(_.mapRight(_.tail)), safe = true)
        }),
        topLevelReferrers,
        safe)
      case functionParameter: FunctionParameter =>
        leaf(functionParameter.name.getOrElse(throw new Exception("Function parameter for display did not have a name")), topLevelReferrers)
    }
  }

  private def leaf(text: String, referrers: Set[String]): Elem = {
    <span class={referrers.map("highlight-" + _).mkString(" ")}>{HtmlHelper.format(text)}</span>
  }
  private def formatted(format: Format, components: Seq[Elem], referrers: Set[String], safe: Boolean): Elem = {
    <span class={referrers.map("highlight-" + _).mkString(" ")}>{format.formatHtml(components, safe)}</span>
  }
}
