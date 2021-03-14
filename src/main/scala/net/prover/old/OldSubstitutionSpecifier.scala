package net.prover.old

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.Substitutions
import net.prover.model.expressions._

import scala.util.{Success, Try}

/**
  * Specify a given expression using an argument list, lazily substituting any argument used.
  *
  * Used during the specification of an expression variable (see [[SubstitutionApplier.transformExpressionVariableWithContext]]
  */
case class SubstitutionSpecificationParameters(targetArguments: Seq[Term], substitutions: Substitutions, applicationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth)
object OldSubstitutionSpecifier
    extends OldExpressionTransformer.TryExpressionTransformer[SubstitutionSpecificationParameters]
    with OldExpressionTransformer.DefaultVariableTransformation[Try, SubstitutionSpecificationParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Try, SubstitutionSpecificationParameters]
{
  override def transformParameterWithContext(parameter: FunctionParameter, specificationParameters: SubstitutionSpecificationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = {
    if (parameter.level == context.internalDepth + specificationParameters.externalContext.externalDepth)
      OldSubstitutionApplier.transformTermWithContext(specificationParameters.targetArguments(parameter.index), ApplicationParameters(specificationParameters.substitutions, specificationParameters.externalContext))(specificationParameters.applicationContext)
        .map(OldParameterInserter.transformTermWithoutContext(_, InsertionParameters(context.internalDepth, 0)))
    else
      Success(OldParameterInserter.transformParameterWithContext(parameter, InsertionParameters(specificationParameters.applicationContext.internalDepth, 0))(context))
  }
}
