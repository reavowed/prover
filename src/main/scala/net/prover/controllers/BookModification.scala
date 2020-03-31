package net.prover.controllers

import net.prover.controllers.models.{InsertionAndReplacementProps, LinkSummary, PathData, ProofUpdateProps, StepInsertionProps, StepReplacementProps}
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.{Step, StepProvingContext, StepReference, SubstitutionContext}

import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Try}

trait BookModification {
  def bookService: BookService

  implicit def toIndexes(pathData: PathData): Seq[Int] = pathData.indexes

  def matchTransitiveChaining[T <: Expression : ChainingMethods](stepOne: Step, stepTwo: Step, stepThree: Step, outerPath: Seq[Int], firstStepIndex: Int)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(T, T, T)] = {
    if (stepThree.referencedLines.flatMap(_.asOptionalInstanceOf[StepReference]).map(_.stepPath) == Set(outerPath :+ firstStepIndex, outerPath :+ (firstStepIndex + 1)))
      for {
        statementOne <- stepOne.provenStatement
        (_, lhsOne, rhsOne) <- ChainingMethods.getJoiner(statementOne)
        statementTwo <- stepTwo.provenStatement
        (_, lhsTwo, rhsTwo) <- ChainingMethods.getJoiner(statementTwo)
        statementThree <- stepThree.provenStatement
        (_, lhsThree, rhsThree) <- ChainingMethods.getJoiner(statementThree)
        if (lhsOne == lhsThree && rhsOne == lhsTwo && rhsTwo == rhsThree)
      } yield (lhsOne, rhsOne, rhsTwo)
    else None
  }
  def matchReplacementChaining[T <: Expression : ChainingMethods](stepOne: Step, stepTwo: Step, outerPath: Seq[Int], firstStepIndex: Int)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(T, T, T)] = {
    if (stepTwo.referencedLines.flatMap(_.asOptionalInstanceOf[StepReference]).map(_.stepPath).contains(outerPath :+ firstStepIndex))
      for {
        statementOne <- stepOne.provenStatement
        (_, lhsOne, rhsOne) <- ChainingMethods.getJoiner(statementOne)
        statementTwo <- stepTwo.provenStatement
        (_, lhsTwo, rhsTwo) <- ChainingMethods.getJoiner(statementTwo)
        if lhsOne == lhsTwo
      } yield (lhsOne, rhsOne, rhsTwo)
    else None
  }

  private def splitPrecedingStepsWhileTransitive(before: Seq[Step], after: Seq[Step], outerPath: Seq[Int])(implicit stepProvingContext: StepProvingContext): (Seq[Step], Seq[Step]) = {

    def splitPrecedingStepsWhileTransitiveGeneric[T <: Expression : ChainingMethods]: Option[(Seq[Step], Seq[Step])] = {
      @scala.annotation.tailrec
      def removeWhileTransitive(currentBefore: Seq[Step], currentStep: Step, currentTransitive: Seq[Step], targetLhs: T): (Seq[Step], Seq[Step]) = {
        currentBefore match {
          case moreBefore :+ first :+ second if matchTransitiveChaining(first, second, currentStep, outerPath, moreBefore.length).exists(_._1 == targetLhs) =>
            removeWhileTransitive(moreBefore, first, first +: second +: currentTransitive, targetLhs)
          case moreBefore :+ first if matchReplacementChaining(first, currentStep, outerPath, moreBefore.length).exists(_._1 == targetLhs) =>
            removeWhileTransitive(moreBefore, first, first +: currentTransitive, targetLhs)
          case _ =>
            (currentBefore, currentTransitive)
        }
      }
      def withTransitivityInFollowingStep: Option[(Seq[Step], Seq[Step])] = {
        for {
          (firstStep, followingSteps) <- after.headAndTailOption
          nextStep <- followingSteps.headOption
          (moreBefore, previousStep) <- before.initAndLastOption
          (lhs, _, _) <- matchTransitiveChaining(previousStep, firstStep, nextStep, outerPath, before.length - 1)
        } yield removeWhileTransitive(moreBefore, previousStep, Seq(previousStep), lhs)
      }
      def fromCurrentStep: Option[(Seq[Step], Seq[Step])] = {
        for {
          firstStep <- after.headOption
          statement <- firstStep.provenStatement
          (_, lhs, _) <- ChainingMethods.getJoiner(statement)
        } yield removeWhileTransitive(before, firstStep, Nil, lhs)
      }

      withTransitivityInFollowingStep orElse fromCurrentStep
    }

    splitPrecedingStepsWhileTransitiveGeneric[Statement] orElse splitPrecedingStepsWhileTransitiveGeneric[Term] getOrElse (before, Nil)
  }

  protected def insertTargetsBeforeTransitivity(outerPath: Seq[Int], before: Seq[Step], newAfter: Seq[Step], newTargets: Seq[Step])(implicit stepProvingContext: StepProvingContext): (Seq[Step], StepInsertionProps) = {
    val (existingStepsBeforeTransitive, transitiveSteps) = splitPrecedingStepsWhileTransitive(before, newAfter, outerPath)
    (existingStepsBeforeTransitive ++ newTargets ++ transitiveSteps ++ newAfter, StepInsertionProps(outerPath :+ existingStepsBeforeTransitive.length, newTargets))
  }

  protected def addBeforeTransitivity[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (StepProvingContext) => Try[Seq[Step]]): Try[ProofUpdateProps[StepInsertionProps]] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps[WithValue[StepInsertionProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { case (steps, stepProvingContext) =>
          steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            for {
              stepsToAddBeforeTransitive <- f(stepProvingContext.updateStepContext(_.addSteps(before).atIndex(last)))
            } yield insertTargetsBeforeTransitivity(init, before, step +: after, stepsToAddBeforeTransitive)(stepProvingContext)
          }.orNotFound(s"Step $stepPath").flatten
        }.map { case (proofUpdateProps, stepInsertionProps) =>
          proofUpdateProps.withNewStepUpdateProps(stepInsertionProps.updateStepsFrom(proofUpdateProps.stepUpdates))
        }
      case _ =>
        Failure(NotFoundException(s"Step $stepPath"))
    }
  }

  protected def replaceStepAndAddBeforeTransitivity[TStep <: Step : ClassTag](
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)
    (f: (TStep, StepProvingContext) => Try[(Step, Seq[Step])]
    ): Try[ProofUpdateProps[InsertionAndReplacementProps]] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps[WithValue[InsertionAndReplacementProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { case (steps, stepProvingContext) =>
          steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            for {
              typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
              (replacementStep, stepsToAddBeforeTransitive) <- f(typedStep, stepProvingContext.updateStepContext(_.addSteps(before).atIndex(last)))
              (newSteps, stepInsertionProps) = insertTargetsBeforeTransitivity(init, before, replacementStep +: after, stepsToAddBeforeTransitive)(stepProvingContext)
            } yield (newSteps, InsertionAndReplacementProps(stepInsertionProps, StepReplacementProps(stepPath.indexes, Seq(replacementStep))))
          }.orNotFound(s"Step $stepPath").flatten
        }.map { case (proofUpdateProps, stepProps) =>
          proofUpdateProps.withNewStepUpdateProps(
            InsertionAndReplacementProps(
              stepProps.insertion.updateStepsFrom(proofUpdateProps.stepUpdates),
              stepProps.replacement.updateStepsFrom(proofUpdateProps.stepUpdates, stepProps.insertion.newSteps.length)))
        }
      case _ =>
        Failure(NotFoundException(s"Step $stepPath"))
    }
  }

  protected def findInference(inferenceId: String)(implicit stepProvingContext: StepProvingContext): Try[Inference.Summary] = {
    stepProvingContext.provingContext.entryContext.allInferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
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
    entriesPotentiallyUsing
      .mapFind { entryUsing =>
        entryUsing.referencedInferenceIds
          .mapFind(referencedInference => entriesPotentiallyBeingUsed.find(_.inferences.exists(_.id == referencedInference)).map(entryUsing -> _)) orElse
        entryUsing.referencedDefinitions
          .mapFind(referencedDefinition => entriesPotentiallyBeingUsed.find(_ == referencedDefinition).map(entryUsing -> _))
      }
  }

  def findUsage(books: Seq[Book], entry: ChapterEntry): Option[(ChapterEntry, ChapterEntry)] = {
    findUsage(books.view.flatMap(_.chapters).flatMap(_.entries), Seq(entry))
  }

  def hasUsages(entriesPotentiallyBeingUsed: Seq[ChapterEntry], entriesPotentiallyUsing: Seq[ChapterEntry]): Boolean = {
    val inferenceIds = entriesPotentiallyBeingUsed.flatMap(_.inferences.map(_.id)).toSet
    entriesPotentiallyUsing.exists(e => e.referencedInferenceIds.intersect(inferenceIds).nonEmpty || e.referencedDefinitions.exists(entriesPotentiallyBeingUsed.contains))
  }
}
