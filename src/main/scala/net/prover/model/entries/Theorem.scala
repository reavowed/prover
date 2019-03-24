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
    key: ChapterEntry.Key.Standalone,
    premises: Seq[Premise],
    conclusion: Statement,
    proof: Seq[Step],
    rearrangementType: RearrangementType)
  extends Inference.Entry
{
  def referencedInferenceIds: Set[String] = proof.flatMap(_.referencedInferenceIds).toSet
  override def inferences: Seq[Inference] = Seq(this)

  private def replaceProof(newProof: Seq[Step]): Theorem = copy(proof = newProof)
  def findStep(indexes: Seq[Int]): Option[Step] = {
    indexes match {
      case Nil =>
        None
      case head +: tail =>
        tail.foldLeft(proof.lift(head)) { (currentStepOption, index) => currentStepOption.flatMap(_.getSubstep(index)) }
    }
  }
  def modifySteps(indexes: Seq[Int], f: Seq[Step] => Option[Seq[Step]]): Option[Theorem] = {
    def helper(indexes: Seq[Int], steps: Seq[Step]): Option[Seq[Step]] = {
      indexes match {
        case Nil =>
          f(steps)
        case head +: tail =>
          steps.updateAtIndexIfDefined(head, _.modifySubsteps(substeps => helper(tail, substeps)))
      }
    }
    helper(indexes, proof).map(replaceProof)
  }
  def tryModifySteps(indexes: Seq[Int], f: Seq[Step] => Option[Try[Seq[Step]]]): Option[Try[Theorem]] = {
    def helper(indexes: Seq[Int], steps: Seq[Step]): Option[Try[Seq[Step]]] = {
      indexes match {
        case Nil =>
          f(steps)
        case head +: tail =>
          steps.tryUpdateAtIndexIfDefined(head, _.tryModifySubsteps(substeps => helper(tail, substeps)))
      }
    }
    helper(indexes, proof).mapMap(replaceProof)
  }
  def tryModifyStep(indexes: Seq[Int], f: Step => Try[Step]): Option[Try[Theorem]] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        tryModifySteps(init, _.tryUpdateAtIndex(last, f))
    }
  }
  def insertStep(indexes: Seq[Int], newStep: Step): Option[Theorem] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        modifySteps(init, steps => {
          val (before, after) = steps.splitAt(last)
          Some((before :+ newStep) ++ after)
        })
    }
  }
  def recalculateReferences(parsingContext: ParsingContext): Theorem = {
    val stepContext = StepContext(premises.map(_.provenStatement), Nil)
    val newProof = proof.recalculateReferences(Nil, stepContext, parsingContext)
    copy(proof = newProof)
  }

  override def serializedLines = Seq(s"theorem $name") ++
    rearrangementType.serialized.toSeq ++
    premises.map(_.serialized) ++
    Seq("conclusion " + conclusion.serialized) ++
    Seq("{") ++
    proof.flatMap(_.serializedLines).indent ++
    Seq("}")

  override def toString = name
}

object Theorem extends ChapterEntryParser {
  override val name: String = "theorem"

  private def conclusionParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }
  def proofParser(premises: Seq[Premise])(implicit parsingContext: ParsingContext): Parser[Seq[Step]] = {
    Step.listParser(Nil)(parsingContext, StepContext(premises.map(_.provenStatement), Nil)).inBraces
  }

  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- Premise.listParser
      conclusion <- conclusionParser
      proof <- proofParser(premises)
    } yield {
      Theorem(
        name,
        (ChapterEntry.Key.Standalone.apply _).tupled(getKey(name)),
        premises,
        conclusion,
        proof,
        rearrangementType)
    }
  }
}
