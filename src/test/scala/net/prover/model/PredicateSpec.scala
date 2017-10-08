package net.prover.model

import net.prover.model.expressions.{ConstantFunction, FunctionParameter, MetaPredicateApplication}

class PredicateSpec extends ProverSpec {
  "predicate match" should {
    "match a metapredicate application to a predicate" in {
      val x = MetaPredicateApplication(φ.!, Seq(FunctionParameter("x", 0, 1)), 1)
        .calculateSubstitutions(ElementOf.!(FunctionParameter("x", 0, 1), ConstantFunction(a, 1)), Substitutions.empty)
        x.must(contain(exactly(
          Substitutions(Map.empty, Map(φ.! -> ElementOf.!!(FunctionParameter("x", 0, 1, 2), ConstantFunction(a, 2)))),
          Substitutions(Map.empty, Map(φ.! -> ElementOf.!!(FunctionParameter("x", 0, 2), ConstantFunction(a, 2)))))))
    }
  }
  "predicate apply substitutions" should {
    "substitute a metapredicate application" in {
      MetaPredicateApplication(φ.!, Seq(FunctionParameter("x", 0, 1)), 1)
        .applySubstitutions(Substitutions(Map.empty, Map(φ.! -> ElementOf.!!(FunctionParameter("x", 0, 2), ConstantFunction(a, 2)))))
        .must(beSome(ElementOf.!(FunctionParameter("x", 0, 1), ConstantFunction(a, 1))))
    }
  }
}
