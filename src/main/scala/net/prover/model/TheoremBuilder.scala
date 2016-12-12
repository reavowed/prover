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

  def addFantasy(assumption: Statement): T = {
    val newFantasy = fantasyOption match {
      case Some(fantasy) =>
        fantasy.addFantasy(assumption)
      case None =>
        Fantasy(assumption)
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
}

case class TheoremBuilder(
    premises: Seq[Statement] = Nil,
    steps: Seq[Step] = Nil,
    fantasyOption: Option[Fantasy] = None,
    arbitraryVariables: Seq[TermVariable] = Nil)
  extends TheoremBuildable[TheoremBuilder] {

  def addPremise(premise: Statement): TheoremBuilder = {
    copy(premises = premises :+ premise)
  }
  def withArbitraryVariables(newArbitraryVariables: Seq[TermVariable]): TheoremBuilder = {
    val fantasyVariables = fantasyAssumptions.flatMap(_.freeVariables)
    if (fantasyVariables.intersect(newArbitraryVariables).nonEmpty) {
      throw ArbitraryVariableException(
        s"Variables ${fantasyVariables.intersect(newArbitraryVariables).mkString(", ")} were non-arbitrary")
    }
    copy(arbitraryVariables = (arbitraryVariables ++ newArbitraryVariables).distinct.sortBy(_.i))
  }

  override protected def withStep(step: Step): TheoremBuilder = copy(steps = steps :+ step)
  override protected def withFantasy(fantasyOption: Option[Fantasy]): TheoremBuilder = copy(fantasyOption = fantasyOption)
  override protected def resolveSpecificReference: PartialFunction[String, Statement] = {
    val premiseReference = "p(\\d+)".r;
    {
      case reference @ premiseReference(IntParser(number)) =>
        premises.lift(number - 1)
          .getOrElse(throw ReferenceResolveException(reference, s"No premise with number $number"))
    }
  }
  protected def fantasyAssumptions: Seq[Statement] = {
    def helper(fantasy: Option[Fantasy], acc: Seq[Statement]): Seq[Statement] = {
      fantasy match {
        case Some(f) =>
          helper(f.fantasyOption, acc :+ f.assumption)
        case None =>
          acc
      }
    }
    helper(fantasyOption, Nil)
  }
}

object TheoremBuilder {
  case class Fantasy(
      assumption: Statement,
      steps: Seq[Step] = Nil,
      fantasyOption: Option[Fantasy] = None)
    extends TheoremBuildable[Fantasy] {
    override protected def withStep(step: Step): Fantasy = copy(steps = steps :+ step)
    override protected def withFantasy(fantasyOption: Option[Fantasy]): Fantasy = copy(fantasyOption = fantasyOption)
    override protected def resolveSpecificReference: PartialFunction[String, Statement] = {
      case "a" =>
        assumption
    }
  }
}
