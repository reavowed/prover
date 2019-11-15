package net.prover.model

import net.prover.model.expressions._

class TermSpec extends ProverSpec {
  implicit val ec = entryContext

  def parseTerm(line: String)(implicit parsingContext: ExpressionParsingContext): Term = {
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
    "parse a single non-ASCII character as a term variable" in {
      parseTerm("∘") mustEqual TermVariable("∘")
    }
    "not parse a statement variable character as a term variable" in {
      parseTerm("φ") must throwAn[Exception]
    }
    "not parse multiple characters as a term variable" in {
      parseTerm("fg") must throwAn[Exception]
    }
  }
}
