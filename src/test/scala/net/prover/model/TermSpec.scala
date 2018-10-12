package net.prover.model

import net.prover.model.expressions._

class TermSpec extends ProverSpec {

  def parseTerm(line: String)(implicit parsingContext: ParsingContext): Term = {
    Term.parser(parsingContext).parseAndDiscard(line)
  }

  "term parser" should {
    "parse a lowercase term variable" in {
      parseTerm("x") mustEqual TermVariable("x")
    }
    "parse an uppercase term variable" in {
      parseTerm("Y") mustEqual TermVariable("Y")
    }
    "parse a term variable with a prime" in {
      parseTerm("f'") mustEqual TermVariable("f'")
    }
    "parse a term variable with a subscript" in {
      parseTerm("g_0") mustEqual TermVariable("g_0")
    }
    "not parse a random character as a term variable" in {
      parseTerm("∘") must throwAn[Exception]
    }
    "parse a named term variable" in {
      parseTerm("∘")(defaultContext.copy(termVariableNames = Set("∘"))) mustEqual TermVariable("∘")
    }
  }
}
