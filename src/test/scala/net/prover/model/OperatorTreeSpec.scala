package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.definitions.OperatorTree
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class OperatorTreeSpec extends Specification {
  implicit val entryContext = defaultEntryContext
  val e = TermVariablePlaceholder("e", 4)
  val f = TermVariablePlaceholder("f", 5)

  "canonical form" should {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    def testCanonicalForm(base: Term, expected: Term): MatchResult[Any] = {
      OperatorTree.parse(base).canonicalForm.term mustEqual expected
    }

    "sort and stack terms" in {
      testCanonicalForm(add(add(a, c), add(b, d)), add(a, add(b, add(c, d))))
    }

    "sort and stack terms after distributing" in {
      testCanonicalForm(
        add(multiply(a, add(b, c)), multiply(d, e)),
        add(multiply(a, b), add(multiply(a, c), multiply(d, e))))
    }

    "distribute" in {
      // (a(cf + de))((bd)(bf))
      // a b b d f (cf + de)
      testCanonicalForm(
        mulZ(
          mulZ(a, addZ(mulZ(c, f), mulZ(d, e))),
          mulZ(mulZ(b, d), mulZ(b, f))),
        addZ(
          mulZ(a, mulZ(b, mulZ(b, mulZ(c, mulZ(d, mulZ(f, f)))))),
          mulZ(a, mulZ(b, mulZ(b, mulZ(d, mulZ(d, mulZ(e, f))))))))
    }

    "remove identities" in {
      testCanonicalForm(add(add(a, c), add(Zero, d)), add(a, add(c, d)))
    }

    "collapse absorbers" in {
      testCanonicalForm(multiply(multiply(c, a), multiply(Zero, d)), Zero)
    }

    "collapse absorbers as identity" in {
      testCanonicalForm(add(multiply(c, a), multiply(Zero, d)), multiply(a, c))
    }

    "eliminate inverses" in {
      testCanonicalForm(addZ(addZ(c, IntegerNegation(b)), addZ(b, a)), addZ(a, c))
    }

    "replace fully eliminated inverses with identity" in {
      testCanonicalForm(addZ(addZ(a, IntegerNegation(b)), addZ(b, IntegerNegation(a))), toZ(Zero))
    }

    "extract inverses" in {
      testCanonicalForm(mulZ(a, IntegerNegation(b)), IntegerNegation(mulZ(a, b)))
    }
  }
}
