package net.prover.model

import net.prover.model.Inference.{Premise, RearrangementType}

case class Axiom(
    name: String,
    key: String,
    chapterKey: String,
    chapterTitle: String,
    bookKey: String,
    bookTitle: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(Axiom)
  with Inference
{
  val id = calculateHash()
  def keyOption = Some(key)
}

object Axiom extends ChapterEntryParser[Axiom] with InferenceParser {
  override val name: String = "axiom"

  private def conclusionParser(implicit context: Context): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }

  def parser(book: Book, chapter: Chapter)(implicit context: Context): Parser[Axiom] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      allowsRearrangement <- Parser.optionalWord("disallow-rearrangement").isUndefined
      premises <- premisesParser
      conclusion <- conclusionParser
      conditions <- Conditions.parser
    } yield {
      Axiom(
        name,
        context.nextInferenceKey(name),
        chapter.key,
        chapter.title,
        book.key,
        book.title,
        premises,
        ProvenStatement(conclusion, conditions),
        rearrangementType,
        allowsRearrangement)
    }
  }

  override def addToContext(axiom: Axiom, context: Context): Context = {
    context.copy(inferences = context.inferences :+ axiom)
  }
}
