package net.prover.model

import net.prover.model.expressions.{DefinedStatement, Template}
import org.specs2.mutable.Specification
import TestDefinitions._

class TemplateSpec extends Specification {

  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))
  given expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)

  "a template" should {
    "parse bound variables correctly" in {
      val template = Template.parser.parseFromString("∀ x → ∈ $0 X φ", "test template")
      val parsedStatement = template.expressionParser.parseFromString("x a with x ψ", "test statement")
      parsedStatement must beEqualTo(ForAll("a")(Implication(ElementOf($, a), ψ($))))
      parsedStatement.asInstanceOf[DefinedStatement].boundVariableNames must beEqualTo(Seq("x"))
    }
  }
}
