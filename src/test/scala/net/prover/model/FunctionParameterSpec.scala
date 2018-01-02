package net.prover.model

import net.prover.model.expressions.{ArgumentList, FunctionParameter}

class FunctionParameterSpec extends ProverSpec {

  "a function parameter specified to a non-variable term should substitute that term" in {
    FunctionParameter.anonymous(index = 1, level = 1, depth = 2)
      .specifyWithSubstitutions(ArgumentList(Seq(a.^^, b.^^, c.^^), 2), Substitutions(terms = Map(b -> EmptySet)))
      .must(beSome(EmptySet.^^^))
  }

  "a function parameter specified to a parameter pointing at the shared context should maintain that reference" in {
    FunctionParameter.anonymous(index = 0, level = 1, depth = 2)
      .specifyWithSubstitutions(ArgumentList(Seq(FunctionParameter.anonymous(index = 2, level = 1, depth = 2)), 2), Substitutions.empty)
      .must(beSome(FunctionParameter.anonymous(index = 2, level = 1, depth = 3)))
  }

  "a function parameter specified to a parameter pointing at the extended context should maintain that reference" in {
    FunctionParameter.anonymous(index = 0, level = 1, depth = 2)
      .specifyWithSubstitutions(ArgumentList(Seq(FunctionParameter.anonymous(index = 2, level = 1, depth = 2)), 2), Substitutions.empty)
      .must(beSome(FunctionParameter.anonymous(index = 2, level = 1, depth = 3)))
  }

  "a function parameter pointing to an internal variable should remain pointing to that variable" in {
    FunctionParameter.anonymous(index = 1, level = 2, depth = 3)
      .specifyWithSubstitutions(ArgumentList(Nil, 2), Substitutions.empty)
      .must(beSome(FunctionParameter.anonymous(index = 1, level = 3, depth = 4)))
  }
}
