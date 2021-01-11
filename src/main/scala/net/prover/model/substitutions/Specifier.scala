package net.prover.model.substitutions;

import net.prover.core.expressions.{Parameter, Statement, Term}
import net.prover.core.transformers.{ContextWithInternalDepth, ExpressionTransformer, ParameterInserter}

case class SpecificationParameters(targetArguments: Map[Int, Term], externalContext: ContextWithExternalDepth)
object Specifier
    extends ExpressionTransformer.OptionExpressionTransformer[SpecificationParameters]
    with ExpressionTransformer.DefaultVariableTransformation[Option, SpecificationParameters]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Option, SpecificationParameters]
{
  def specifyOutsideProof(statement: Statement, targetArguments: Seq[Term]): Option[Statement] = transformStatementWithoutContext(statement, SpecificationParameters(targetArguments.indices.zip(targetArguments).toMap, ContextWithExternalDepth.outsideProof))

  override def transformParameterWithContext(parameter: Parameter, parameters: SpecificationParameters)(implicit context: ContextWithInternalDepth): Option[Term] = {
    if (parameter.level == context.internalDepth + parameters.externalContext.externalDepth)
      parameters.targetArguments.get(parameter.index).map(ParameterInserter.insertParameters(_, context.internalDepth, 0))
    else
      Some(parameter)
  }
}
