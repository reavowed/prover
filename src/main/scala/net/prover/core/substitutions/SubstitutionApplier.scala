package net.prover.core.substitutions

import net.prover.core.RuleOfInference
import net.prover.core.expressions._
import net.prover.core.substitutions.SubstitutionApplier.Context

import scala.util.{Success, Try}

case class SubstitutionApplier(substitutions: Substitutions) {
  import substitutions._

  def applySubstitutions(expression: Expression)(implicit context: Context): Try[Expression] = expression match {
    case statement: Statement => applySubstitutions(statement)
    case term: Term => applySubstitutions(term)
  }
  def applySubstitutions(statement: Statement)(implicit context: Context): Try[Statement] = statement match {
    case StatementVariable(index, arguments) =>
      for {
        predicate <- statements.lift(index).orExceptionWithMessage(s"No substitution statement with index $index found")
        result <- substitutions.specifier.specifyAndApplySubstitutions(predicate, arguments)(context.toSpecificationContext)
      } yield result
    case definedStatement : DefinedStatement =>
      applySubstitutionsToDefinedExpression(definedStatement)
  }
  def applySubstitutions(term: Term)(implicit context: Context): Try[Term] = term match {
    case TermVariable(index, arguments) =>
      for {
        function <- terms.lift(index).orExceptionWithMessage(s"No substitution statement with index $index found")
        result <- substitutions.specifier.specifyAndApplySubstitutions(function, arguments)(context.toSpecificationContext)
      } yield result
    case definedTerm: DefinedTerm =>
      applySubstitutionsToDefinedExpression(definedTerm)
    case parameter: Parameter =>
      Success(parameter)
  }
  def applySubstitutions(expressions: Seq[Expression])(implicit context: Context): Try[Seq[Expression]] = {
      expressions.map(applySubstitutions).traverseTry
  }

  def applySubstitutions(ruleOfInference: RuleOfInference): Try[RuleOfInference] = {
    implicit val context = Context.outsideProof
    for {
      premises <- ruleOfInference.premises.map(applySubstitutions).traverseTry
      conclusion <- applySubstitutions(ruleOfInference.conclusion)
    } yield RuleOfInference.Raw(premises, conclusion)
  }

  private def applySubstitutionsToDefinedExpression[T <: DefinedExpression[T]](expression: T)(implicit context: Context): Try[T] = {
    val innerContext = context.increaseDepth(expression)
    expression.components.map(applySubstitutions(_)(innerContext)).traverseTry.map(expression.withNewComponents)
  }
}

object SubstitutionApplier {
  case class Context(internalDepth: Int, externalDepth: Int) extends ContextWithInternalDepth[Context] {
    def toSpecificationContext: SubstitutionSpecifier.Context = SubstitutionSpecifier.Context(0, this)
    override def withInternalDepth(newInternalDepth: Int): Context = copy(internalDepth = newInternalDepth)
  }
  object Context {
    val outsideProof: Context = Context(0, 0)
  }
}
