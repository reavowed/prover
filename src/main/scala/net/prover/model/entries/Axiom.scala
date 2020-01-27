package net.prover.model.entries

import net.prover.model.expressions.Statement
import net.prover.model._
import net.prover.model.definitions.Definitions

case class Axiom(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement)
  extends Inference.Entry
{
  override def withName(newName: String): Axiom = copy(name = newName)
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions
  override def isComplete(definitions: Definitions): Boolean = true
  override def inferences: Seq[Inference.FromEntry] = Seq(this)
  override def serializedLines: Seq[String] = {
    Seq(s"axiom $name") ++
      premises.map("premise " + _.serialized) ++
      Seq(s"conclusion ${conclusion.serialized}")
  }

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): Axiom = {
    Axiom(
      name,
      premises.map(_.replaceDefinition(oldDefinition, newDefinition)),
      conclusion.replaceDefinition(oldDefinition, newDefinition))
  }
}

object Axiom extends Inference.EntryParser {
  override val name: String = "axiom"

  def parser(implicit entryContext: EntryContext): Parser[Axiom] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
    for {
      name <- Parser.toEndOfLine
      premises <- premisesParser
      conclusion <- conclusionParser
    } yield {
      Axiom(
        name,
        premises,
        conclusion)
    }
  }
  override def toString = name
}
