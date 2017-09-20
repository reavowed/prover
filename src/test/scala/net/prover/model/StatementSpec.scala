package net.prover.model

import net.prover.model.expressions._

class StatementSpec extends ProverSpec {

  def parseStatement(line: String): Statement = {
    Statement.parser(defaultContext).parseAndDiscard(line)
  }

  def parseStatementList(line: String): Seq[Statement] = {
    Statement.listParser(defaultContext).parseAndDiscard(line)
  }

  "statement parser" should {
    "parse a statement variable" in {
      parseStatement("φ") mustEqual φ
    }

    "parse a binary connective" in {
      parseStatement("→ φ ψ") mustEqual Implication(φ, ψ)
    }

    "parse a nested binary connective" in {
      parseStatement("→ → φ ψ χ") mustEqual
        Implication(Implication(φ, ψ), χ)
    }

    "parse a quantified statement" in {
      parseStatement("∀ y χ") mustEqual ForAll("y")(χ)
    }

    "parse an empty list" in {
      parseStatementList("()") mustEqual Nil
    }

    "parse a list with a single statement" in {
      parseStatementList("(φ)") mustEqual Seq(φ)
    }

    "parse a list with multiple statements" in {
      parseStatementList("(φ, ψ, χ)") mustEqual Seq(φ, ψ, χ)
    }
  }

  "statement general substitution" should {
    "apply to a statement variable" in {
      val substitutions = Substitutions(Map(φ -> Implication(φ, ψ), ψ -> χ), Map.empty)
      φ.applySubstitutions(substitutions) must beSome(Implication(φ, ψ))
    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      φ.calculateSubstitutions(Implication(φ, ψ), Substitutions.empty, 0) mustEqual
        Seq(Substitutions(Map(φ -> Implication(φ, ψ)), Map.empty))
    }
    "not match a defined statement to a statement variable" in {
      Implication(φ, ψ).calculateSubstitutions(φ, Substitutions.empty, 0) must beEmpty
    }
    "not match two different defined statements" in {
      Implication(φ, ψ).calculateSubstitutions(Conjunction(φ, ψ), Substitutions.empty, 0) must beEmpty
    }
    "not match two defined statements of the same type whose components don't match" in {
      Implication(Implication(φ, ψ), χ)
        .calculateSubstitutions(Implication(φ, ψ), Substitutions.empty, 0)
        .must(beEmpty)
    }
    "match two defined statements of the same type whose components are different statement variables" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(φ, χ), Substitutions.empty, 0)
        .mustEqual(Seq(Substitutions(Map(φ -> φ, ψ -> χ), Map.empty)))
    }
    "match two defined statements of the same type whose components are different but match" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), Substitutions.empty, 0)
        .mustEqual(Seq(Substitutions(Map(φ -> Conjunction(φ, ψ), ψ -> χ), Map.empty)))
    }
    "match two connectives of the same type whose components merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)), Substitutions.empty, 0)
        .mustEqual(Seq(Substitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty)))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), Substitutions.empty, 0)
        .must(beEmpty)
    }
    "match a predicate twice with a constant bound variable" in {
      Equivalence(φ(x), φ(y))
        .calculateSubstitutions(
          Equivalence(ElementOf(BoundVariable(0)("z"), a), ElementOf(BoundVariable(0)("z"), b)),
          Substitutions.empty,
          0)
        .mustEqual(Seq(Substitutions(
          Map(x -> a, y -> b),
          Map(φ -> DefinedPredicate(ElementOf, Seq(BoundVariable(0)("z"), IdentityFunction))(Nil)))))
    }
    "match a predicate to a bound variable outside the current scope" in {
      φ(x)
        .calculateSubstitutions(ElementOf(BoundVariable(0)("y"), z), Substitutions.empty, 0)
        .must(contain(Substitutions(
          Map(x -> BoundVariable(0)("y")),
          Map(φ -> DefinedPredicate(ElementOf, Seq(IdentityFunction, ConstantFunction(z)))(Nil)))))
    }
    "not match a predicate to a bound variable outside the current scope" in {
      φ(x)
        .calculateSubstitutions(ElementOf(BoundVariable(0)("y"), z), Substitutions.empty, 1)
        .must(not(contain(Substitutions(
          Map(x -> BoundVariable(0)("y")),
          Map(φ -> DefinedPredicate(ElementOf, Seq(IdentityFunction, ConstantFunction(z)))(Nil))))))
    }
  }

//  "statement condensing" should {
//    "condense a statement variable with a known substitution to a matching compound statement" in {
//      val premise = φ
//      val premiseSubstitutions = Substitutions(
//        Map(φ -> Conjunction(φ, ψ)), Map.empty)
//      val conclusion = Conjunction(χ, φ)
//      val conclusionSubstitutions = Substitutions.empty
//      premise.condense(conclusion, premiseSubstitutions, conclusionSubstitutions) must beSome((
//        Substitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty),
//        Substitutions(Map(χ -> φ, φ -> ψ), Map.empty)))
//    }
//
//    "condense a compound statement to a statement variable with a known matching substitution" in {
//      val premise = φ
//      val premiseSubstitutions = Substitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty)
//      val conclusion = Conjunction(χ, φ)
//      val conclusionSubstitutions = Substitutions.empty
//      conclusion.condense(premise, conclusionSubstitutions, premiseSubstitutions) must beSome((
//        Substitutions(Map(χ -> φ, φ -> ψ), Map.empty),
//        Substitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty)))
//    }
//  }
}
