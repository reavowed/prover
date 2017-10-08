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
      parseStatement("∀ y with y χ") mustEqual ForAll("y")(χ.!(FunctionParameter("y", 0)))
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
    "substitute a statement variable" in {
      val substitutions = Substitutions(Map(φ -> Implication(φ, ψ), ψ -> χ), Map.empty)
      φ.applySubstitutions(substitutions) must beSome(Implication(φ, ψ))
    }

    "substitute a predicate into a defined statement" in {
      Equals(x, x)
          .applySubstitutions(Substitutions(Map(x -> FunctionParameter.anonymous(0)), Map.empty))
          .must(beSome(Equals.!(FunctionParameter.anonymous(0), FunctionParameter.anonymous(0))))
    }

    "substitute a predicate into a defined statement's predicate component" in {
      ForAll("x")(φ.!(FunctionParameter.anonymous(0)))
          .applySubstitutions(Substitutions(Map.empty, Map(φ.! -> Equals.!!(FunctionParameter.anonymous(0, 2), FunctionParameter.anonymous(0, 2)))))
          .must(beSome(ForAll("x")(Equals.!(FunctionParameter.anonymous(0), FunctionParameter.anonymous(0)))))
    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      φ.calculateSubstitutions(Implication(φ, ψ), Substitutions.empty) mustEqual
        Seq(Substitutions(Map(φ -> Implication(φ, ψ)), Map.empty))
    }
    "not match a defined statement to a statement variable" in {
      Implication(φ, ψ).calculateSubstitutions(φ, Substitutions.empty) must beEmpty
    }
    "not match two different defined statements" in {
      Implication(φ, ψ).calculateSubstitutions(Conjunction(φ, ψ), Substitutions.empty) must beEmpty
    }
    "not match two defined statements of the same type whose components don't match" in {
      Implication(Implication(φ, ψ), χ)
        .calculateSubstitutions(Implication(φ, ψ), Substitutions.empty)
        .must(beEmpty)
    }
    "match two defined statements of the same type whose components are different statement variables" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(φ, χ), Substitutions.empty)
        .mustEqual(Seq(Substitutions(Map(φ -> φ, ψ -> χ), Map.empty)))
    }
    "match two defined statements of the same type whose components are different but match" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), Substitutions.empty)
        .mustEqual(Seq(Substitutions(Map(φ -> Conjunction(φ, ψ), ψ -> χ), Map.empty)))
    }
    "match two connectives of the same type whose components merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)), Substitutions.empty)
        .mustEqual(Seq(Substitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty)))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), Substitutions.empty)
        .must(beEmpty)
    }

    "match a predicate application to a defined statement" in {
      φ(a)
        .calculateSubstitutions(ElementOf(b, EmptySet), Substitutions.empty)
        .must(contain(exactly(
          Substitutions(Map.empty, Map(φ.! -> ElementOf.!(ConstantFunction(b, 1), EmptySet.!))),
          Substitutions(Map(a -> b), Map(φ.! -> ElementOf.!(FunctionParameter.anonymous(0), EmptySet.!))))))
    }

    "match a predicate application to another predicate application" in {
      φ(a)
        .calculateSubstitutions(ψ(b), Substitutions.empty)
        .must(contain(exactly(
          Substitutions(Map.empty, Map(φ.! -> ψ.!(ConstantFunction(b, 1)))),
          Substitutions(Map(a -> b), Map(φ.! -> ψ.!(FunctionParameter.anonymous(0)))))))
    }

    "match a predicate application to a defined predicate" in {
      val res = φ(a)
        .calculateSubstitutions(ElementOf.!(FunctionParameter.anonymous(0), ConstantFunction(b, 1)), Substitutions.empty)
      res.must(contain(exactly(
        Substitutions(Map.empty, Map(φ.! -> ElementOf.!!(FunctionParameter.anonymous(0, 2), ConstantFunction(b, 2)))),
        Substitutions(Map(a -> FunctionParameter.anonymous(0)), Map(φ.! -> ElementOf.!!(FunctionParameter.anonymous(0, 1, 2), ConstantFunction(b, 2)))),
        Substitutions(Map(a -> ConstantFunction(b, 1)), Map(φ.! -> ElementOf.!!(FunctionParameter.anonymous(0, 2), FunctionParameter.anonymous(0, 1, 2)))))))
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
