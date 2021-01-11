package net.prover.model.simplification

import net.prover.core.expressions.{CompoundExpression, Expression}

object ComponentFinder {
  def findComponentPath(baseExpression: Expression, targetExpression: Expression): Option[Seq[Int]] = {
    findComponentPath(baseExpression, targetExpression, Nil)
  }
  private def findComponentPath(baseExpression: Expression, targetExpression: Expression, pathSoFar: Seq[Int]): Option[Seq[Int]] = baseExpression match {
    case `targetExpression` =>
      Some(pathSoFar)
    case compoundExpression: CompoundExpression[_] =>
      compoundExpression.components.zipWithIndex.mapFind { case (component, index) =>
        findComponentPath(component, targetExpression, pathSoFar :+ index)
      }
    case _ =>
      None
  }
}
