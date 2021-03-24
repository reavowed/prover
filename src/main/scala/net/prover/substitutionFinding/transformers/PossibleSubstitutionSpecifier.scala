package net.prover.substitutionFinding.transformers

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.expressions.{FunctionParameter, Term}
import net.prover.old.{InsertionParameters, OldExpressionTransformer, OldParameterInserter, OldSubstitutionApplier}
import net.prover.substitutionFinding.model.PossibleSubstitutions

import scala.util.{Success, Try}

case class PossibleSubstitutionSpecificationParameters(targetArguments: Seq[Term], substitutions: PossibleSubstitutions, applicationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth)
object PossibleSubstitutionSpecifier
    extends OldExpressionTransformer.TryExpressionTransformer[PossibleSubstitutionSpecificationParameters]
    with OldExpressionTransformer.DefaultVariableTransformation[Try, PossibleSubstitutionSpecificationParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Try, PossibleSubstitutionSpecificationParameters]
{
  override def transformParameterWithContext(parameter: FunctionParameter, specificationParameters: PossibleSubstitutionSpecificationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = {
    if (parameter.level == context.internalDepth + specificationParameters.externalContext.externalDepth)
      PossibleSubstitutionApplier.transformTermWithContext(specificationParameters.targetArguments(parameter.index), PossibleSubstitutionApplicationParameters(specificationParameters.substitutions, specificationParameters.externalContext))(specificationParameters.applicationContext)
        .map(OldParameterInserter.transformTermWithoutContext(_, InsertionParameters(context.internalDepth, 0)))
    else
      Success(OldParameterInserter.transformParameterWithContext(parameter, InsertionParameters(specificationParameters.applicationContext.internalDepth, 0))(context))
  }
}
