package net.prover.model

import net.prover.model.TheoremBuilder.Fantasy

import scala.util.control.NonFatal

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
    try {
      reference match {
        case fantasyReference(innerReference) =>
          fantasyOption
            .getOrElse(throw ReferenceResolveException(reference, "No fantasy to reference"))
            .resolveReference(innerReference)
        case stepReference(IntParser(number)) =>
          steps.lift(number - 1)
            .getOrElse(throw ReferenceResolveException(reference, "Step index out of range"))
            .statement
        case _ =>
          resolveSpecificReference.lift(reference)
            .getOrElse(throw ReferenceResolveException(reference, "Unrecognised reference format"))
      }
    } catch {
      case e: ReferenceResolveException =>
        throw e.copy(reference = reference)
      case NonFatal(other) =>
        throw ReferenceResolveException(reference, "unexpected error")
    }
  }

  protected def resolveSpecificReference: PartialFunction[String, Statement]

  def nonFreeTerms: Seq[TermVariable] = fantasyOption.toSeq.flatMap(_.nonFreeTerms)
}

case class TheoremBuilder(
    hypotheses: Seq[Statement] = Nil,
    steps: Seq[Step] = Nil,
    fantasyOption: Option[Fantasy] = None)
  extends TheoremBuildable[TheoremBuilder] {

  def addHypothesis(hypothesis: Statement) = copy(hypotheses = hypotheses :+ hypothesis)

  override protected def withStep(step: Step): TheoremBuilder = copy(steps = steps :+ step)
  override protected def withFantasy(fantasyOption: Option[Fantasy]): TheoremBuilder = copy(fantasyOption = fantasyOption)
  override protected def resolveSpecificReference: PartialFunction[String, Statement] = {
    val hypothesisReference = "h(\\d+)".r;
    {
      case reference @ hypothesisReference(IntParser(number)) =>
        hypotheses.lift(number - 1)
          .getOrElse(throw ReferenceResolveException(reference, "Step index out of range"))
    }
  }

  override def nonFreeTerms: Seq[TermVariable] = {
    (super.nonFreeTerms ++ hypotheses.flatMap(_.freeVariables.termVariables)).distinct
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
    override protected def resolveSpecificReference: PartialFunction[String, Statement] = {
      case "h" =>
        hypothesis
    }
    override def nonFreeTerms: Seq[TermVariable] = {
      (super.nonFreeTerms ++ hypothesis.freeVariables.termVariables).distinct
    }
  }
}
