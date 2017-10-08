package net.prover.model.expressions
import net.prover.model.ParsingContext

case class PredicateApplicationVariable(
    variable: PredicateVariable,
    parameters: Seq[FunctionParameter],
    depth: Int)
  extends Variable
{
  override def name = s"${variable.name}(${parameters.map(_.name).mkString(", ")})"
  override def expression = MetaPredicateApplication(variable, parameters, depth)
  override def expressionParser(parameterList: Seq[String])(implicit context: ParsingContext) = {
    Predicate.parser(context.addParameterList(parameterList))
  }
  override def depthDifference(expression: Expression): Option[Int] = {
    expression match {
      case predicate: Predicate if predicate.depth >= depth => Some(predicate.depth - depth)
      case _ => None
    }
  }
}
