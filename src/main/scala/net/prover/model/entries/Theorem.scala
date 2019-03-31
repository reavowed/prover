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
    override val key: ChapterEntry.Key.Standalone,
    premises: Seq[Statement],
    conclusion: Statement,
    proof: Seq[Step],
    rearrangementType: RearrangementType)
  extends Inference.Entry
{
  override def referencedInferenceIds: Set[String] = proof.flatMap(_.referencedInferenceIds).toSet
  override def referencedDefinitions: Set[ExpressionDefinition] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions ++ proof.flatMap(_.referencedDefinitions).toSet
  override def inferences: Seq[Inference] = Seq(this)
  private def initialContext: StepContext = StepContext.justWithPremises(premises)

  private def replaceProof(newProof: Seq[Step]): Theorem = copy(proof = newProof)
  def findStep(indexes: Seq[Int]): Option[(Step, StepContext)] = {
    indexes match {
      case Nil =>
        None
      case head +: tail =>
        val initialStepContextAndPathOption = proof.lift(head).map(step => (step, initialContext.addSteps(proof.take(head)), Seq(head)))
        tail.foldLeft(initialStepContextAndPathOption) { case (currentStepContextAndPathOption, index) =>
          currentStepContextAndPathOption.flatMap { case (step, context, path) =>
            step.getSubstep(index, context).map { case (newStep, newContext) => (newStep, newContext, path :+ index) }
          }
        }.map { case (step, context, path) => (step, context) }
    }
  }
  def modifySteps(indexes: Seq[Int], f: (Seq[Step], StepContext) => Option[Seq[Step]]): Option[Theorem] = {
    def helper(indexes: Seq[Int], steps: Seq[Step], outerContext: StepContext): Option[Seq[Step]] = {
      indexes match {
        case Nil =>
          f(steps, outerContext)
        case head +: tail =>
          steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
              step.modifySubsteps(outerContext.addSteps(before).atIndex(head), (substeps, innerContext) => helper(tail, substeps, innerContext))
                .map { updatedStep =>
                  (before :+ updatedStep) ++ after
                }
          }
      }
    }
    helper(indexes, proof, initialContext).map(replaceProof)
  }
  def tryModifySteps(indexes: Seq[Int], f: (Seq[Step], StepContext) => Option[Try[Seq[Step]]]): Option[Try[Theorem]] = {
    def helper(indexes: Seq[Int], steps: Seq[Step], outerContext: StepContext): Option[Try[Seq[Step]]] = {
      indexes match {
        case Nil =>
          f(steps, outerContext)
        case head +: tail =>
          steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
              step.tryModifySubsteps(outerContext.addSteps(before).atIndex(head), (substeps, innerContext) => helper(tail, substeps, innerContext))
                .mapMap { updatedStep =>
                  (before :+ updatedStep) ++ after
                }
          }
      }
    }
    helper(indexes, proof, initialContext).mapMap(replaceProof)
  }
  def tryModifyStep(indexes: Seq[Int], f: (Step, StepContext) => Try[Step]): Option[Try[Theorem]] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        tryModifySteps(init, (steps, context) => steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
          f(step, context.addSteps(before)).map { updatedStep =>
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
  def recalculateReferences(parsingContext: ParsingContext): Theorem = {
    val newProof = proof.recalculateReferences(initialContext, parsingContext)
    copy(proof = newProof)
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

  def proofParser(premises: Seq[Statement])(implicit parsingContext: ParsingContext): Parser[Seq[Step]] = {
    Step.listParser(Nil)(parsingContext, StepContext.justWithPremises(premises)).inBraces
  }

  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- premisesParser
      conclusion <- conclusionParser
      proof <- proofParser(premises)
    } yield {
      Theorem(
        name,
        ChapterEntry.Key.Standalone(name, getKey),
        premises,
        conclusion,
        proof,
        rearrangementType)
    }
  }
}
