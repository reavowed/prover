package net.prover.model

import net.prover.model.Inference.DirectPremise

abstract class InferenceTransform extends ChapterEntry(InferenceTransform) {
  def transform(premises: Seq[DirectPremise], conclusion: Statement): Seq[(Seq[DirectPremise], Seq[Statement])]
}

case class SimpleInferenceTransform(placeholderStatement: Statement) extends InferenceTransform {
  def transformStatement(statement: Statement): Option[Statement] = {
    placeholderStatement.replacePlaceholder(statement)
  }
  def transformPremise(premise: DirectPremise): Option[DirectPremise] = {
    for {
      transformedStatement <- transformStatement(premise.statement)
    } yield premise.copy(statement = transformedStatement)
  }
  def transform(premises: Seq[DirectPremise], conclusion: Statement): Seq[(Seq[DirectPremise], Seq[Statement])] = {
    val resultOption = for {
      transformedPremises <- premises.map(transformPremise).traverseOption
      transformedConclusion <- transformStatement(conclusion)
      statementsToProve = premises.map(_.statement) :+ conclusion :+ transformedConclusion
    } yield Seq((transformedPremises, statementsToProve))
    resultOption.toSeq.flatten
  }
}

case class PartialInferenceTransform(
  premisePlaceholderStatement: Statement,
  conclusionPlaceholderStatement: Statement)
extends InferenceTransform {
  def transformPremiseFully(premise: DirectPremise): Option[(DirectPremise, Option[Statement])] = {
    for {
      transformedPremiseStatement <- premisePlaceholderStatement.replacePlaceholder(premise.statement)
      transformedPremiseConclusion <- conclusionPlaceholderStatement.replacePlaceholder(premise.statement)
    } yield (premise.copy(statement = transformedPremiseStatement), Some(transformedPremiseConclusion))
  }
  def transformPremisePartially(premise: DirectPremise): Option[(DirectPremise, Option[Statement])] = {
    for {
      transformedPremiseStatement <- conclusionPlaceholderStatement.replacePlaceholder(premise.statement)
    } yield (premise.copy(statement = transformedPremiseStatement), None)
  }

  def transformPremise(premise: DirectPremise): Seq[(DirectPremise, Option[Statement])] = {
    transformPremiseFully(premise).toSeq ++ transformPremisePartially(premise).toSeq
  }
  def transform(premises: Seq[DirectPremise], conclusion: Statement): Seq[(Seq[DirectPremise], Seq[Statement])] = {
    (for {
      transformedConclusion <- conclusionPlaceholderStatement.replacePlaceholder(conclusion)
    } yield {
      premises
        .foldLeft(Seq((Seq.empty[DirectPremise], Seq.empty[Statement]))) { case (acc, premise) =>
          transformPremise(premise).flatMap { case (transformedPremise, statementOption) =>
            acc.map { case (previousPremises, previousStatements) =>
              (previousPremises :+ transformedPremise, previousStatements ++ statementOption.toSeq)
            }
          }
        }
        .map { case (premises, statements) =>
          (premises, statements :+ transformedConclusion)
        }
    }).toSeq.flatten
  }
}

object InferenceTransform extends ChapterEntryParser[InferenceTransform] {
  override def name: String = "inference-transform"
  override def parser(book: Book, chapter: Chapter)(implicit context: Context): Parser[InferenceTransform] = {
    for {
      placeholderStatement <- Statement.parser.inParens
      conclusionPlaceholderStatementOption <- Statement.parser.optionalInParens
    } yield {
      conclusionPlaceholderStatementOption match {
        case Some(conclusionPlaceholderStatement) =>
          PartialInferenceTransform(placeholderStatement, conclusionPlaceholderStatement)
        case None =>
          SimpleInferenceTransform(placeholderStatement)
      }
    }
  }

  override def addToContext(transform: InferenceTransform, context: Context): Context = {
    context.addInferenceTransform(transform)
  }
}
