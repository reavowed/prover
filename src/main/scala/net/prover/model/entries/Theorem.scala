package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.controllers.Identity
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import scalaz.Functor
import scalaz.syntax.functor._

@JsonIgnoreProperties(Array("rearrangementType"))
case class Theorem(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement,
    proof: Seq[Step],
    rearrangementType: RearrangementType)
  extends Inference.Entry
{
  override def referencedInferenceIds: Set[String] = proof.flatMap(_.referencedInferenceIds).toSet
  override def referencedEntries: Set[ChapterEntry] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions ++ proof.flatMap(_.referencedDefinitions).toSet
  override def inferences: Seq[Inference] = Seq(this)

  def initialStepContext(entryContext: EntryContext): StepContext = StepContext.withPremisesAndTerms(premises, requiredSubstitutions.terms, entryContext)

  def isComplete: Boolean = proof.forall(_.isComplete)

  private def replaceProof(newProof: Seq[Step]): Theorem = copy(proof = newProof)
  def findStep(indexes: Seq[Int], entryContext: EntryContext): Option[(Step, StepContext)] = {
    indexes match {
      case Nil =>
        None
      case head +: tail =>
        val initialStepContextAndPathOption = proof.splitAtIndexIfValid(head).map { case (before, step, _) =>
          (step, initialStepContext(entryContext).addSteps(before).atIndex(head), Seq(head))
        }
        tail.foldLeft(initialStepContextAndPathOption) { case (currentStepContextAndPathOption, index) =>
          currentStepContextAndPathOption.flatMap { case (step, stepContext, path) =>
            step.getSubstep(index, stepContext).map { case (newStep, newStepContext) => (newStep, newStepContext, path :+ index) }
          }
        }.map { case (step, stepContext, _) => (step, stepContext) }
    }
  }
  def modifySteps[F[_] : Functor](indexes: Seq[Int], entryContext: EntryContext, f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Theorem]] = {
    def helper(indexes: Seq[Int], steps: Seq[Step], outerContext: StepContext): Option[F[Seq[Step]]] = {
      indexes match {
        case Nil =>
          f(steps, outerContext)
        case head +: tail =>
          steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
              step.modifySubsteps(outerContext.addSteps(before).atIndex(head), (substeps, innerContext) => helper(tail, substeps, innerContext))
                .map(_.map(updatedStep => (before :+ updatedStep) ++ after))
          }
      }
    }
    helper(indexes, proof, initialStepContext(entryContext)).map(_.map(replaceProof))
  }
  def modifyStep[F[_] : Functor](indexes: Seq[Int], entryContext: EntryContext, f: (Step, StepContext) => F[Step]): Option[F[Theorem]] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        modifySteps(init, entryContext, (steps, context) => steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
          f(step, context.atIndex(last)).map { updatedStep =>
            (before :+ updatedStep) ++ after
          }
        })
    }
  }
  def insertStep(indexes: Seq[Int], newStep: Step, entryContext: EntryContext): Option[Theorem] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        modifySteps[Identity](init, entryContext, (steps, _) => {
          val (before, after) = steps.splitAt(last)
          Some((before :+ newStep) ++ after)
        })
    }
  }
  def recalculateReferences(entryContext: EntryContext): Theorem = {
    val newProof = proof.recalculateReferences(initialStepContext(entryContext))
    copy(proof = newProof)
  }

  def findAssertions(entryContext: EntryContext): Seq[(Step.Assertion, StepContext)] = {
    def forStep(step: Step, context: StepContext): Seq[(Step.Assertion, StepContext)] = {
      step match {
        case assertion: Step.Assertion =>
          Seq((assertion, context))
        case stepWithSubsteps: Step.WithSubsteps =>
          forSteps(stepWithSubsteps.substeps, stepWithSubsteps.specifyStepContext(context))
        case _ =>
          Nil
      }
    }
    def forSteps(steps: Seq[Step], context: StepContext): Seq[(Step.Assertion, StepContext)] = {
      steps.zipWithIndex.flatMap { case (step, index) => forStep(step, context.atIndex(index))}
    }
    forSteps(proof, initialStepContext(entryContext))
  }

  override def serializedLines: Seq[String] = Seq(s"theorem $name") ++
    rearrangementType.serialized.toSeq ++
    premises.map("premise " + _.serialized) ++
    Seq("conclusion " + conclusion.serialized) ++
    Seq("{") ++
    proof.flatMap(_.serializedLines).indent ++
    Seq("}")

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): Theorem = {
    Theorem(
      name,
      premises.map(_.replaceDefinition(oldDefinition, newDefinition)),
      conclusion.replaceDefinition(oldDefinition, newDefinition),
      proof.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)),
      rearrangementType)
  }
}

object Theorem extends Inference.EntryParser {
  override val name: String = "theorem"

  def proofParser(premises: Seq[Statement], conclusion: Statement)(implicit entryContext: EntryContext): Parser[Seq[Step]] = {
    Step.listParser(
      entryContext,
      StepContext.withPremisesAndTerms(premises, (premises :+ conclusion).map(_.requiredSubstitutions).foldTogether.terms, entryContext)
    ).inBraces
  }

  override def parser(implicit entryContext: EntryContext): Parser[Theorem] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- premisesParser
      conclusion <- conclusionParser
      proof <- proofParser(premises, conclusion)
    } yield {
      if (!proof.mapCollect(_.provenStatement).lastOption.contains(conclusion)) throw new Exception(s"Proof of theorem '$name' did not prove $conclusion")
      Theorem(
        name,
        premises,
        conclusion,
        proof,
        rearrangementType)
    }
  }
}
