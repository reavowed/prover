package net.prover.views

import net.prover.model.HtmlHelper
import net.prover.model.proof.ProofLine

import scala.xml.Elem

object ExpressionView {
  def apply(expression: ProofLine.Expression, safe: Boolean = false): Elem = {
    expression match {
      case ProofLine.Expression.Plain(text, referrers) =>
        <span class={referrers.map("highlight-" + _).mkString(" ")}>{HtmlHelper.format(text)}</span>
      case ProofLine.Expression.Nested(definitionFormat, components, referrers) =>
        <span class={referrers.map("highlight-" + _).mkString(" ")}>{definitionFormat.formatHtml(
          components.map(apply(_, safe = true)),
          safe)}</span>
    }
  }
}
