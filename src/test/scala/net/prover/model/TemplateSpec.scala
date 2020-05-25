package net.prover.model

import net.prover.model.expressions.{DefinedStatement, Template}
import org.specs2.mutable.Specification
import TestDefinitions._

class TemplateSpec extends Specification {

  implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext(
    defaultEntryContext,
    VariableDefinitions(Seq(VariableDefinition("ψ", 1, Nil)), Seq("A").map(VariableDefinition(_, 0, Nil))),
    Nil)

  "a template" should {
    "parse bound variables correctly" in {
      val template = Template.parser.parseFromString("∀ x → ∈ $0 X φ", "test template")
      val parsedStatement = template.expressionParser.parseFromString("a A with a ψ", "test statement")
      parsedStatement mustEqual ForAll("a")(Implication(ElementOf($, A), ψ($)))
      parsedStatement.asInstanceOf[DefinedStatement].boundVariableNames mustEqual Seq("a")
    }
  }
}
