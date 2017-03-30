package net.prover.model

import net.prover.model.Inference.DirectPremise

case class InferenceTransform(placeholderStatement: Statement) extends ChapterEntry(InferenceTransform) {
  def matchConclusion(inference: Inference, statement: Statement): Option[PartialSubstitutions] = {
    val transformedConclusion = transformStatement(inference.conclusion.statement)
    transformedConclusion.calculateSubstitutions(statement, PartialSubstitutions.empty)
  }
  def transformStatement(statement: Statement): Statement = {
    placeholderStatement.replacePlaceholder(statement)
  }
  def transformPremise(premise: DirectPremise): DirectPremise = {
    premise.copy(statement = transformStatement(premise.statement))
  }
}

object InferenceTransform extends ChapterEntryParser[InferenceTransform] {
  override def name: String = "inference-transform"
  override def parser(implicit context: Context): Parser[InferenceTransform] = {
    for {
      placeholderStatement <- Statement.parser
    } yield {
      InferenceTransform(placeholderStatement)
    }
  }
  override def addToContext(transform: InferenceTransform, context: Context): Context = {
    context.addInferenceTransform(transform)
  }
}
