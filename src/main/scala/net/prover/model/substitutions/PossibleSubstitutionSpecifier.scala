package net.prover.model.substitutions

import net.prover.core.expressions._
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth, ExpressionTransformer, InsertionParameters, ParameterInserter}

import scala.util.{Success, Try}

/**
  * Specify a given expression using an argument list, lazily substituting any argument used.
  *
  * Used during the specification of an expression variable (see [[PossibleSubstitutionApplier.transformExpressionVariableWithContext]]
  */
case class PossibleSubstitutionSpecificationParameters(targetArguments: Seq[Term], substitutions: PossibleSubstitutions, applicationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth)
object PossibleSubstitutionSpecifier
    extends ExpressionTransformer.OptionExpressionTransformer[PossibleSubstitutionSpecificationParameters]
    with ExpressionTransformer.DefaultVariableTransformation[Option, PossibleSubstitutionSpecificationParameters]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Option, PossibleSubstitutionSpecificationParameters]
{
  override def transformParameterWithContext(parameter: Parameter, specificationParameters: PossibleSubstitutionSpecificationParameters)(implicit context: ContextWithInternalDepth): Option[Term] = {
    if (parameter.level == context.internalDepth + specificationParameters.externalContext.externalDepth)
      PossibleSubstitutionApplier.transformTermWithContext(specificationParameters.targetArguments(parameter.index), PossibleSubstitutionApplicationParameters(specificationParameters.substitutions, specificationParameters.externalContext))(specificationParameters.applicationContext)
        .map(ParameterInserter.transformTermWithoutContext(_, InsertionParameters(context.internalDepth, 0)))
    else
      Some(ParameterInserter.transformParameterWithContext(parameter, InsertionParameters(specificationParameters.applicationContext.internalDepth, 0))(context))
  }
}
