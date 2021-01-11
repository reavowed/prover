package net.prover.core.substitutions

import net.prover.core.expressions._
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth, ExpressionTransformer, InsertionParameters, ParameterInserter}

import scala.util.{Success, Try}

/**
  * Specify a given expression using an argument list, lazily substituting any argument used.
  *
  * Used during the specification of an expression variable (see [[SubstitutionApplier.transformExpressionVariableWithContext]]
  */
case class SpecificationParameters(targetArguments: Seq[Term], substitutions: Substitutions, applicationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth)
object SubstitutionSpecifier
    extends ExpressionTransformer.TryExpressionTransformer[SpecificationParameters]
    with ExpressionTransformer.DefaultVariableTransformation[Try, SpecificationParameters]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Try, SpecificationParameters]
{
  override def transformParameterWithContext(parameter: Parameter, specificationParameters: SpecificationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = {
    if (parameter.level == context.internalDepth + specificationParameters.externalContext.externalDepth)
      SubstitutionApplier.transformTermWithContext(specificationParameters.targetArguments(parameter.index), ApplicationParameters(specificationParameters.substitutions, specificationParameters.externalContext))(specificationParameters.applicationContext)
        .map(ParameterInserter.transformTermWithoutContext(_, InsertionParameters(context.internalDepth, 0)))
    else
      Success(ParameterInserter.transformParameterWithContext(parameter, InsertionParameters(specificationParameters.applicationContext.internalDepth, 0))(context))
  }
}
