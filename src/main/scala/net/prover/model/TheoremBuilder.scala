package net.prover.model

import net.prover.model.TheoremBuilder.Fantasy

trait TheoremBuildable[T <: TheoremBuildable[T]] {
  def steps: Seq[Step]
  def fantasyOption: Option[Fantasy]

  protected def withStep(step: Step): T
  protected def withFantasy(fantasyOption: Option[Fantasy]): T

  def addStep(step: Step): T = {
    fantasyOption match {
      case Some(fantasy) =>
        withFantasy(Some(fantasy.addStep(step)))
      case None =>
        withStep(step)
    }
  }

  def addFantasy(fantasyHypothesis: Statement): T = {
    val newFantasy = fantasyOption match {
      case Some(fantasy) =>
        fantasy.addFantasy(fantasyHypothesis)
      case None =>
        Fantasy(fantasyHypothesis)
    }
    withFantasy(Some(newFantasy))
  }

  def replaceFantasy(f: Fantasy => Step): T = {
    fantasyOption match {
      case Some(fantasy) =>
        fantasy.fantasyOption match {
          case None =>
            withFantasy(None).withStep(f(fantasy))
          case Some(_) =>
            withFantasy(Some(fantasy.replaceFantasy(f)))
        }
      case None =>
        throw new Exception("No fantasy to replace")
    }
  }

  def resolveReference(reference: String): Statement = {
    val fantasyReference = "f\\.(.+)".r
    val stepReference = "(\\d+)".r
    reference match {
      case fantasyReference(innerReference) =>
        fantasyOption.getOrElse(throw new Exception("No fantasy to reference")).resolveReference(innerReference)
      case stepReference(IntParser(number)) =>
        steps(number - 1).statement
    }
  }
}

case class TheoremBuilder(
    hypotheses: Seq[Statement] = Nil,
    steps: Seq[Step] = Nil,
    fantasyOption: Option[Fantasy] = None)
  extends TheoremBuildable[TheoremBuilder] {

  def addHypothesis(hypothesis: Statement) = copy(hypotheses = hypotheses :+ hypothesis)

  override protected def withStep(step: Step): TheoremBuilder = copy(steps = steps :+ step)
  override protected def withFantasy(fantasyOption: Option[Fantasy]): TheoremBuilder = copy(fantasyOption = fantasyOption)

  override def resolveReference(reference: String): Statement = {
    val hypothesisReference = "h(\\d+)".r
    reference match {
      case hypothesisReference(IntParser(number)) =>
        hypotheses(number - 1)
      case _ =>
        super.resolveReference(reference)
    }
  }
}

object TheoremBuilder {
  case class Fantasy(
      hypothesis: Statement,
      steps: Seq[Step] = Nil,
      fantasyOption: Option[Fantasy] = None)
    extends TheoremBuildable[Fantasy] {

    override protected def withStep(step: Step): Fantasy = copy(steps = steps :+ step)
    override protected def withFantasy(fantasyOption: Option[Fantasy]): Fantasy = copy(fantasyOption = fantasyOption)

    override def resolveReference(reference: String): Statement = {
      reference match {
        case "h" =>
          hypothesis
        case _ =>
          super.resolveReference(reference)
      }
    }
  }
}
