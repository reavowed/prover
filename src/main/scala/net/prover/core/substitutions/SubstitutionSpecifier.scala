package net.prover.core.substitutions

import net.prover.core.expressions._
import net.prover.core.substitutions.SubstitutionSpecifier.Context

import scala.util.{Success, Try}

case class SubstitutionSpecifier(substitutions: Substitutions) {

  def specifyAndApplySubstitutions(expression: Expression, targetArguments: Seq[Term])(implicit context: Context): Try[Expression] = expression match {
    case statement: Statement =>
      specifyAndApplySubstitutions(statement, targetArguments)
    case term: Term =>
      specifyAndApplySubstitutions(term, targetArguments)
  }
  def specifyAndApplySubstitutions(statement: Statement, targetArguments: Seq[Term])(implicit context: Context): Try[Statement] = statement match {
    case sv @ StatementVariable(_, arguments) =>
      arguments.map(specifyAndApplySubstitutions(_, targetArguments)).traverseTry.map(sv.withNewArguments)
    case definedStatement : DefinedStatement =>
      specifyAndApplySubstitutionsToDefinedExpression(definedStatement, targetArguments)
  }
  def specifyAndApplySubstitutions(term: Term, targetArguments: Seq[Term])(implicit context: Context): Try[Term] = term match {
    case tv @ TermVariable(_, arguments) =>
      arguments.map(specifyAndApplySubstitutions(_, targetArguments)).traverseTry.map(tv.withNewArguments)
    case definedTerm : DefinedTerm =>
      specifyAndApplySubstitutionsToDefinedExpression(definedTerm, targetArguments)
    case parameter @ Parameter(index, level) =>
      if (level == context.internalDepth + context.externalDepth)
        substitutions.applier.applySubstitutions(targetArguments(index))(context.previousContext).map(ParameterInserter.insertExternalParameters(_, context.internalDepth)(ParameterInserter.Context(0)))
      else
        Success(ParameterInserter.insertExternalParameters(parameter, context.previousContext.internalDepth)(ParameterInserter.Context(context.internalDepth)))
  }
  def specifyAndApplySubstitutions(expressions: Seq[Expression], targetArguments: Seq[Term])(implicit specificationContext: Context): Try[Seq[Expression]] = {
    expressions.map(specifyAndApplySubstitutions(_, targetArguments)).traverseTry
  }

  private def specifyAndApplySubstitutionsToDefinedExpression[T <: DefinedExpression[T]](expression: T, targetArguments: Seq[Term])(implicit context: Context): Try[T] = {
    val innerContext = context.increaseDepth(expression)
    expression.components.map(specifyAndApplySubstitutions(_, targetArguments)(innerContext)).traverseTry.map(expression.withNewComponents)
  }
}

object SubstitutionSpecifier {
  case class Context(internalDepth: Int, previousContext: SubstitutionApplier.Context) extends ContextWithInternalDepth[Context] {
    def externalDepth: Int = previousContext.externalDepth
    override def withInternalDepth(newInternalDepth: Int): Context = copy(internalDepth = newInternalDepth)
  }
}
