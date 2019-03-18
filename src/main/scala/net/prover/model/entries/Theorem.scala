package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof._

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement", "proofOutline"))
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
  def findStep(indexes: Seq[Int]): Option[Step] = {
    indexes match {
      case Nil =>
        None
      case head +: tail =>
        proof.findSubstep(head, tail)
    }
  }
  def replaceStep(indexes: Seq[Int], newStep: Step): Theorem = {
    copy(proof = proof.updated(indexes.head, proof(indexes.head).replaceStep(indexes.tail, newStep)))
  }
  def insertStep(indexes: Seq[Int], newStep: Step): Option[Theorem] = {
    indexes match {
      case Nil =>
        None
      case head +: tail =>
        proof.insertSubstep(head, tail, newStep).map(newProof => copy(proof = newProof))
    }
  }
  def recalculateReferences(): Option[Theorem] = {
    val stepContext = StepContext(premises.map(_.provenStatement), 0)
    proof.recalculateReferences(Nil, stepContext).map(newProof => copy(proof = newProof))
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
    Step.listParser(Nil)(parsingContext, StepContext(premises.map(_.provenStatement), 0)).inBraces
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
