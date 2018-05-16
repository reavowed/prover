package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Shorthand}
import net.prover.model.expressions._

case class ProofLine(
  prefix: String,
  expression: ProofLine.Expression,
  reference: Option[String],
  indentLevel: Int,
  inferenceLink: Option[ProofLine.EntryLink])

object ProofLine {
  trait Expression {
    def referrers: Set[String]
  }
  object Expression {
    case class Plain(text: String, referrers: Set[String]) extends Expression
    case class Nested(
        format: Format,
        components: Seq[Expression],
        referrers: Set[String])
      extends Expression
    def create(
      source: expressions.Expression,
      referrers: Set[(String, Seq[Int])])(
      implicit displayContext: DisplayContext
    ): Expression = {
      val topLevelReferrers = referrers.filter(_._2.isEmpty).map(_._1)
      displayContext.shorthands.mapFind(withShorthand(source, topLevelReferrers, _))
        .getOrElse(directly(source, referrers, topLevelReferrers))
    }

    def withShorthand(
      source: expressions.Expression,
      referrers: Set[String],
      shorthand: Shorthand)(
      implicit displayContext: DisplayContext
    ): Option[Expression] = {
      for {
        rawComponents <- shorthand.template.matchExpression(source)
        components = rawComponents.map {
          case Left(name) => Plain(name, Set.empty)
          case Right(expression) => fromRealExpression(expression)
        }
      } yield Nested(shorthand.format,components, referrers)
    }

    def directly(
      source: expressions.Expression,
      referrers: Set[(String, Seq[Int])],
      topLevelReferrers: Set[String])(
      implicit displayContext: DisplayContext
    ): Expression = {
      source match {
        case ExpressionVariable(text) => Plain(text, topLevelReferrers)
        case DefinedExpression(definition, boundVariableNames, components) =>
          Nested(
            definition.format,
            boundVariableNames.map(Plain(_, Set.empty)) ++ components.mapWithIndex { (component, index) => {
              create(component, referrers.filter(_._2.headOption.contains(index)).map(_.mapRight(_.tail)))
            }},
            topLevelReferrers)
        case ExpressionApplication(text, arguments) => Nested(
          Format.Default(text + "(" + arguments.indices.map(i => s"%$i").mkString(", ") + ")", requiresBrackets = false),
          arguments.mapWithIndex((argument, index) => {
            create(argument, referrers.filter(_._2.headOption.contains(index)).map(_.mapRight(_.tail)))
          }),
          topLevelReferrers)
        case functionParameter: FunctionParameter =>
          Plain(functionParameter.name.getOrElse(throw new Exception("Function parameter for display did not have a name")), topLevelReferrers)
      }
    }

    implicit def fromRealExpression(
      realExpression: expressions.Expression)(
      implicit displayContext: DisplayContext
    ): Expression = create(realExpression, Set.empty)
  }
  case class EntryLink(name: String, key: Option[ChapterEntry.Key])
  object EntryLink {
    def apply(inference: Inference): EntryLink = EntryLink(
      inference.name,
      inference.entryKeyOption)
  }
}
