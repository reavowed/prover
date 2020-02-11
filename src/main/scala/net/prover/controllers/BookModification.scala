package net.prover.controllers

import net.prover.controllers.models.{LinkSummary, PathData, UpdateProps}
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.definitions.{Equality, Transitivity}
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.{Expression, Term}
import net.prover.model.proof.{Step, StepProvingContext}

import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Try}

trait BookModification {
  def bookService: BookService

  implicit def toIndexes(pathData: PathData): Seq[Int] = pathData.indexes

  private def splitPrecedingStepsWhileTransitive(before: Seq[Step], after: Seq[Step])(implicit stepProvingContext: StepProvingContext): (Seq[Step], Seq[Step]) = {
    def getTargetLhsFromTransitivity[T <: Expression](currentRhs: Expression, followingSteps: Seq[Step], transitivity: Transitivity[T]): Option[T] = {
      followingSteps match {
        case Step.Assertion(transitivity.statement(lhs, `currentRhs`), transitivity.inference, _, _) +: _ =>
          Some(lhs)
        case _ =>
          None
      }
    }
    @scala.annotation.tailrec
    def takeWhileTransitive(steps: Seq[Step], targetLhs: Term, currentLhs: Term, acc: Seq[Step], equality: Equality): (Seq[Step], Seq[Step]) = {
      steps match {
        case first :+ preceding :+ (transitive: Step.Assertion)
          if transitive.inference.id == equality.transitivity.inference.id
        =>
          (preceding.provenStatement, transitive.statement) match {
            case (Some(equality(newLhs, `currentLhs`)), equality(`targetLhs`, `currentLhs`)) =>
              takeWhileTransitive(first, targetLhs, newLhs, Seq(preceding, transitive) ++ acc, equality)
            case _ =>
              (steps, acc)
          }
        case first :+ preceding =>
          preceding.provenStatement match {
            case Some(equality(`targetLhs`, `currentLhs`)) =>
              (first, preceding +: acc)
          }
        case _ =>
          (steps, acc)
      }
    }
    (for {
      equality <- stepProvingContext.provingContext.equalityOption
      (firstStep, followingSteps) <- after.headAndTailOption
      statement <- firstStep.provenStatement
      (lhs, rhs) <- equality.unapply(statement)
      targetLhs <- getTargetLhsFromTransitivity(rhs, followingSteps, equality.transitivity)
    } yield takeWhileTransitive(before, targetLhs, lhs, Nil, equality)) getOrElse (before, Nil)
  }

  protected def insertTargetsBeforeTransitivity(before: Seq[Step], newAfter: Seq[Step], newTargets: Seq[Step])(implicit stepProvingContext: StepProvingContext): Seq[Step] = {
    val (existingStepsBeforeTransitive, transitiveSteps) = splitPrecedingStepsWhileTransitive(before, newAfter)
    (existingStepsBeforeTransitive ++ newTargets ++ transitiveSteps ++ newAfter)
  }

  protected def replaceStepAndAddBeforeTransitivity[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (TStep, StepProvingContext) => Try[(Step, Seq[Step])]): Try[UpdateProps] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps(bookKey, chapterKey, theoremKey, proofIndex, init) { case (steps, stepProvingContext) =>
          import stepProvingContext._
          steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            for {
              typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
              (replacementStep, stepsToAddBeforeTransitive) <- f(typedStep, StepProvingContext(stepContext.addSteps(before).atIndex(last), provingContext))
            } yield insertTargetsBeforeTransitivity(before, replacementStep +: after, stepsToAddBeforeTransitive)(stepProvingContext)
          }.orNotFound(s"Step $stepPath").flatten
        }
      case _ =>
        Failure(NotFoundException(s"Step $stepPath"))
    }
  }

  protected def findInference(inferenceId: String)(implicit stepProvingContext: StepProvingContext): Try[Inference.Summary] = {
    stepProvingContext.provingContext.entryContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  def getInferenceUsages(entry: ChapterEntry, books: Seq[Book]): Seq[(String, String, Seq[LinkSummary])] = {
    val inferenceIds = entry.inferences.map(_.id).toSet
    for {
      (book, bookKey) <- bookService.getBooksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      theoremsWithKeys = BookService.getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
        .filter(_._1.referencedInferenceIds.intersect(inferenceIds).nonEmpty)
      if theoremsWithKeys.nonEmpty
    } yield (book.title, chapter.title, theoremsWithKeys.map { case (theorem, key) => LinkSummary(theorem.name, BookService.getEntryUrl(bookKey, chapterKey, key) + "#inferencesToHighlight=" + entry.inferences.map(_.id).mkString(","))})
  }

  def findUsage(entriesPotentiallyUsing: Seq[ChapterEntry], entriesPotentiallyBeingUsed: Seq[ChapterEntry]): Option[(ChapterEntry, ChapterEntry)] = {
    entriesPotentiallyBeingUsed.mapFind(e =>
      e.referencedInferenceIds
        .mapFind(i => entriesPotentiallyUsing.find(_.inferences.exists(_.id == i)).map(_ -> e)) orElse
      e.referencedDefinitions
        .mapFind(d => entriesPotentiallyUsing.find(_.referencedDefinitions.contains(d)).map(_ -> e)))
  }

  def findUsage(entry: ChapterEntry, books: Seq[Book]): Option[(ChapterEntry, ChapterEntry)] = {
    findUsage(Seq(entry), books.view.flatMap(_.chapters).flatMap(_.entries))
  }

  def hasUsages(entriesPotentiallyBeingUsed: Seq[ChapterEntry], entriesPotentiallyUsing: Seq[ChapterEntry]): Boolean = {
    val inferenceIds = entriesPotentiallyBeingUsed.flatMap(_.inferences.map(_.id)).toSet
    entriesPotentiallyUsing.exists(e => e.referencedInferenceIds.intersect(inferenceIds).nonEmpty || e.referencedDefinitions.exists(entriesPotentiallyBeingUsed.contains))
  }
}
