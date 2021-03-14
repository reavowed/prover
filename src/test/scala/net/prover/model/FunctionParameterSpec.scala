package net.prover.model

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.FunctionParameter
import net.prover.old.{OldSubstitutionSpecifier, SubstitutionSpecificationParameters}
import org.specs2.mutable.Specification

class FunctionParameterSpec extends Specification {
  "a function parameter specified to a non-variable term should substitute that term" in {
    OldSubstitutionSpecifier.transformParameterWithContext(
      FunctionParameter(1, 2),
      SubstitutionSpecificationParameters(Seq(a, b, c), Substitutions(Nil, Seq(b, EmptySet, b)), ContextWithInternalDepth(1), ContextWithExternalDepth(1)))(
      ContextWithInternalDepth(1)
    ).must(beSuccessfulTry(EmptySet))
  }

  "a function parameter specified to a parameter pointing at the shared context should maintain that reference" in {
    OldSubstitutionSpecifier.transformParameterWithContext(
      FunctionParameter(0, 2),
      SubstitutionSpecificationParameters(Seq(FunctionParameter(2, 0)), Substitutions.empty, ContextWithInternalDepth(1), ContextWithExternalDepth(1)))(
      ContextWithInternalDepth(1)
    ).must(beSuccessfulTry(FunctionParameter(2, 1)))
  }

  "a function parameter specified to a parameter pointing at the extended context should maintain that reference" in {
    OldSubstitutionSpecifier.transformParameterWithContext(
      FunctionParameter(0, 2),
      SubstitutionSpecificationParameters(Seq(FunctionParameter(2, 1)), Substitutions.empty, ContextWithInternalDepth(1), ContextWithExternalDepth(1)))(
      ContextWithInternalDepth(1)
    ).must(beSuccessfulTry(FunctionParameter(2, 2)))
  }

  "a function parameter pointing to an external variable should remain pointing to that variable" in {
    OldSubstitutionSpecifier.transformParameterWithContext(
      FunctionParameter(0, 1),
      SubstitutionSpecificationParameters(Nil, Substitutions.empty, ContextWithInternalDepth(1), ContextWithExternalDepth(1)))(
      ContextWithInternalDepth(1)
    ).must(beSuccessfulTry(FunctionParameter(0, 2)))
  }

  "a function parameter pointing to an internal variable should remain pointing to that variable" in {
    OldSubstitutionSpecifier.transformParameterWithContext(
      FunctionParameter(0, 0),
      SubstitutionSpecificationParameters(Nil, Substitutions.empty, ContextWithInternalDepth(1), ContextWithExternalDepth(1)))(
      ContextWithInternalDepth(1)
    ).must(beSuccessfulTry(FunctionParameter(0, 0)))
  }
}
