package net.prover.model

import net.prover.model.expressions.FunctionParameter

class FunctionParameterSpec extends ProverSpec {

  "a function parameter specified to a non-variable term should substitute that term" in {
    FunctionParameter.anonymous(1, 2)
      .specifyWithSubstitutions(Seq(a, b, c), Substitutions(terms = Map(b -> EmptySet)), 1, 1, 1)
      .must(beSome(EmptySet))
  }

  "a function parameter specified to a parameter pointing at the shared context should maintain that reference" in {
    FunctionParameter.anonymous(0, 2)
      .specifyWithSubstitutions(Seq(FunctionParameter.anonymous(2, 0)), Substitutions.empty, 1, 1, 1)
      .must(beSome(FunctionParameter.anonymous(2, 1)))
  }

  "a function parameter specified to a parameter pointing at the extended context should maintain that reference" in {
    FunctionParameter.anonymous(0, 2)
      .specifyWithSubstitutions(Seq(FunctionParameter.anonymous(2, 1)), Substitutions.empty, 1, 1, 1)
      .must(beSome(FunctionParameter.anonymous(2, 2)))
  }

  "a function parameter pointing to an external variable should remain pointing to that variable" in {
    FunctionParameter.anonymous(0, 1)
      .specifyWithSubstitutions(Nil, Substitutions.empty, 1, 1, 1)
      .must(beSome(FunctionParameter.anonymous(0, 2)))
  }

  "a function parameter pointing to an internal variable should remain pointing to that variable" in {
    FunctionParameter.anonymous(0, 0)
      .specifyWithSubstitutions(Nil, Substitutions.empty, 1, 1, 1)
      .must(beSome(FunctionParameter.anonymous(0, 0)))
  }
}
