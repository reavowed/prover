package net.prover.model

import net.prover.model.expressions.{DefinedStatement, Template}
import org.specs2.mutable.Specification
import TestDefinitions._

class TemplateSpec extends Specification {

  "a template" should {
    "parse bound variables correctly" in {
      val template = Template.parser.parseFromString("∀ x → ∈ $0 X φ", "test template")
      val parsedStatement = template.expressionParser.parseFromString("a A with a ψ", "test statement")
      parsedStatement mustEqual ForAll("a")(Implication(ElementOf($, A), ψ($)))
      parsedStatement.asInstanceOf[DefinedStatement].scopedBoundVariableNames mustEqual Seq("a")
    }
  }
}
