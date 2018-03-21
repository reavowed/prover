package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.{DefinedExpression, ExpressionApplication, ExpressionVariable, FunctionParameter}

case class ProofLine(
  prefix: String,
  expression: ProofLine.Expression,
  reference: Option[String],
  indentLevel: Int,
  inferenceLink: Option[ProofLine.InferenceLink])

object ProofLine {
  trait Expression
  object Expression {
    case class Plain(text: String, referrers: Set[String]) extends Expression
    case class Nested(
        format: Format,
        boundVariableNames: Seq[String],
        components: Seq[Expression],
        referrers: Set[String])
      extends Expression

    def create(source: expressions.Expression, referrers: Set[(String, Seq[Int])]): Expression = {
      val topLevelReferrers = referrers.filter(_._2.isEmpty).map(_._1)
      source match {
        case ExpressionVariable(text) => Plain(text, topLevelReferrers)
        case DefinedExpression(definition, boundVariableNames, components) => Nested(
          definition.format,
          boundVariableNames,
          components.mapWithIndex { (component, index) => {
            create(component, referrers.filter(_._2.headOption.contains(index)).map(_.mapRight(_.tail)))
          }},
          topLevelReferrers)
        case ExpressionApplication(text, arguments) => Nested(
          Format(text + "(" + arguments.indices.map(i => s"%$i").mkString(", ") + ")", requiresBrackets = false),
          Nil,
          arguments.mapWithIndex((argument, index) => {
            create(argument, referrers.filter(_._2.headOption.contains(index)).map(_.mapRight(_.tail)))
          }),
          topLevelReferrers)
        case functionParameter: FunctionParameter =>
          Plain(functionParameter.toString, topLevelReferrers)
      }
    }
    implicit def fromRealExpression(realExpression: expressions.Expression): Expression = create(realExpression, Set.empty)
  }
  case class InferenceLink(name: String, key: Option[(String, String, String)])
  object InferenceLink {
    def apply(inference: Inference): InferenceLink = InferenceLink(
      inference.name,
      inference.keyOption.map(key => (inference.bookKey, inference.chapterKey, key)))
  }
}