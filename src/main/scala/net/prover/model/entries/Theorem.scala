package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof._

import scala.util.Try

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
  override def referencedDefinitions: Set[ExpressionDefinition] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions ++ proof.flatMap(_.referencedDefinitions).toSet
  override def inferences: Seq[Inference] = Seq(this)
  private def initialStepContext: StepContext = StepContext.empty
  private def initialPremiseContext(entryContext: EntryContext): PremiseContext = PremiseContext.justWithPremises(premises, entryContext)

  private def replaceProof(newProof: Seq[Step]): Theorem = copy(proof = newProof)
  def findStep(indexes: Seq[Int], entryContext: EntryContext): Option[(Step, StepContext, PremiseContext)] = {
    indexes match {
      case Nil =>
        None
      case head +: tail =>
        val initialStepContextAndPathOption = proof.splitAtIndexIfValid(head).map { case (before, step, _) =>
          (step, initialStepContext.atIndex(head), initialPremiseContext(entryContext).addSteps(before, initialStepContext), Seq(head))
        }
        tail.foldLeft(initialStepContextAndPathOption) { case (currentStepContextAndPathOption, index) =>
          currentStepContextAndPathOption.flatMap { case (step, stepContext, premiseContext, path) =>
            step.getSubstep(index, stepContext, premiseContext).map { case (newStep, newStepContext, newPremiseContext) => (newStep, newStepContext, newPremiseContext, path :+ index) }
          }
        }.map { case (step, stepContext, premiseContext, path) => (step, stepContext, premiseContext) }
    }
  }
  def modifySteps(indexes: Seq[Int], f: (Seq[Step], StepContext) => Option[Seq[Step]]): Option[Theorem] = {
    def helper(indexes: Seq[Int], steps: Seq[Step], outerContext: StepContext): Option[Seq[Step]] = {
      indexes match {
        case Nil =>
          f(steps, outerContext)
        case head +: tail =>
          steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
              step.modifySubsteps(outerContext.atIndex(head), (substeps, innerContext) => helper(tail, substeps, innerContext))
                .map { updatedStep =>
                  (before :+ updatedStep) ++ after
                }
          }
      }
    }
    helper(indexes, proof, initialStepContext).map(replaceProof)
  }
  def tryModifySteps(indexes: Seq[Int], entryContext: EntryContext, f: (Seq[Step], StepContext, PremiseContext) => Option[Try[Seq[Step]]]): Option[Try[Theorem]] = {
    def helper(indexes: Seq[Int], steps: Seq[Step], stepContext: StepContext, premiseContext: PremiseContext): Option[Try[Seq[Step]]] = {
      indexes match {
        case Nil =>
          f(steps, stepContext, premiseContext)
        case head +: tail =>
          steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
            step.tryModifySubsteps(
              stepContext.atIndex(head),
              premiseContext.addSteps(before, stepContext),
              (substeps, innerStepContext, innerPremiseContext) => helper(tail, substeps, innerStepContext, innerPremiseContext)
            ).mapMap { updatedStep =>
              (before :+ updatedStep) ++ after
            }
          }
      }
    }
    helper(indexes, proof, initialStepContext, initialPremiseContext(entryContext)).mapMap(replaceProof)
  }
  def tryModifyStep(indexes: Seq[Int], entryContext: EntryContext, f: (Step, StepContext) => Try[Step]): Option[Try[Theorem]] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        tryModifySteps(init, entryContext, (steps, context, _) => steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
          f(step, context.atIndex(last)).map { updatedStep =>
            (before :+ updatedStep) ++ after
          }
        })
    }
  }
  def insertStep(indexes: Seq[Int], newStep: Step): Option[Theorem] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        modifySteps(init, (steps, _) => {
          val (before, after) = steps.splitAt(last)
          Some((before :+ newStep) ++ after)
        })
    }
  }
  def recalculateReferences(entryContext: EntryContext): Theorem = {
    val newProof = proof.recalculateReferences(initialStepContext, initialPremiseContext(entryContext))
    copy(proof = newProof)
  }

  def findAssertions: Seq[(Step.Assertion, StepContext)] = {
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
    forSteps(proof, initialStepContext)
  }

  override def serializedLines: Seq[String] = Seq(s"theorem $name") ++
    rearrangementType.serialized.toSeq ++
    premises.map("premise " + _.serialized) ++
    Seq("conclusion " + conclusion.serialized) ++
    Seq("{") ++
    proof.flatMap(_.serializedLines).indent ++
    Seq("}")

  override def toString: String = name
}

object Theorem extends Inference.EntryParser {
  override val name: String = "theorem"

  def proofParser(premises: Seq[Statement])(implicit entryContext: EntryContext): Parser[Seq[Step]] = {
    Step.listParser(entryContext, StepContext.empty, PremiseContext.justWithPremises(premises, entryContext)).inBraces
  }

  override def parser(implicit entryContext: EntryContext): Parser[Theorem] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- premisesParser
      conclusion <- conclusionParser
      proof <- proofParser(premises)
    } yield {
      Theorem(
        name,
        premises,
        conclusion,
        proof,
        rearrangementType)
    }
  }
}
