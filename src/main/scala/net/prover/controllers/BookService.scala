package net.prover.controllers

import net.prover.books.management.{BookStateManager, ReloadBooks, UpdateBooks}
import net.prover.books.model.Book
import net.prover.controllers.models._
import net.prover.entries._
import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.Step
import net.prover.theorems.{FindStep, RecalculateReferences, ReplaceSteps}
import net.prover.util.FunctorTypes._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import scalaz.Functor
import scalaz.syntax.functor._

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

@Service
class BookService @Autowired() (implicit bookStateManager: BookStateManager) {
  def globalContext: GlobalContext = bookStateManager.globalContext
  def books: Seq[Book] = bookStateManager.books

  def findBook(bookKey: String): Try[BookWithContext] = {
    bookStateManager.globalContext.findBook(bookKey)
  }
  def findChapter(bookKey: String, chapterKey: String): Try[ChapterWithContext] = {
    bookStateManager.globalContext.findChapter(bookKey, chapterKey)
  }
  def findEntry[T <: ChapterEntry : ClassTag](bookKey: String, chapterKey: String, entryKey: String): Try[TypedEntryWithContext[T]] = {
    bookStateManager.globalContext.findEntry(bookKey, chapterKey, entryKey)
  }

  def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData): Try[TypedStepWithContext[T]] = {
    for {
      theoremWithContext <- findEntry[Theorem](bookKey, chapterKey, theoremKey)
      stepWithContext <- FindStep(theoremWithContext, proofIndex, stepPath.indexes).orNotFound(s"Step $stepPath")
      step <- stepWithContext.step.asOptionalInstanceOf[T].orBadRequest(s"Step is not ${classTag[T].runtimeClass.getName}")
    } yield stepWithContext.withStep(step)
  }

  def modifyBooks[F[_] : Functor](f: GlobalContext => F[Seq[Book]]): F[GlobalContext] = UpdateBooks(f)

  def modifyBook[F[_] : Functor](bookKey: String, f: BookWithContext => Try[F[Book]]): Try[F[BookWithContext]] = {
    modifyBooks[TryFWithValue[F, Book]#Type] { globalContext =>
      for {
        bookWithContext <- globalContext.findBook(bookKey)
        newBookF <- f(bookWithContext)
      } yield newBookF.map(newBook => (globalContext.allBooks.replaceValue(bookWithContext.book, newBook), newBook))
    }.map(_.map { case (newContext, newBook) => newContext.booksWithContexts.find(_.book == newBook).get })
  }

  def modifyChapter[F[_] : Functor](bookKey: String, chapterKey: String, f: ChapterWithContext => Try[F[Chapter]]): Try[F[ChapterWithContext]] = {
    modifyBook[FWithValue[F, Chapter]#Type](bookKey, bookWithContext =>
      for {
        chapterWithContext <- bookWithContext.getChapter(chapterKey)
        newChapterF <- f(chapterWithContext)
      } yield newChapterF.map(newChapter => (bookWithContext.book.updateChapter(chapterKey, newChapter), newChapter))
    ).map(_.map { case (newBookWithContext, newChapter) => newBookWithContext.getChapter(newChapter) })
  }

  def modifyEntry[TEntry <: ChapterEntry : ClassTag, F[_] : Functor](bookKey: String, chapterKey: String, entryKey: String, f: TypedEntryWithContext[TEntry] => Try[F[TEntry]]): Try[F[TypedEntryWithContext[TEntry]]] = {
    modifyChapter[FWithValue[F, TEntry]#Type](bookKey, chapterKey, chapterWithContext =>
      for {
        entryWithContext <- chapterWithContext.getEntry(entryKey)
        newEntryF <- f(entryWithContext)
      } yield newEntryF.map(newEntry => (chapterWithContext.chapter.updateEntry(entryKey, newEntry), newEntry))
    ).map(_.map { case (newChapterWithContext, newEntry) => newChapterWithContext.getEntry(newEntry) })
  }

  def modifyTheorem[F[_] : Functor](bookKey: String, chapterKey: String, theoremKey: String)(getUpdatedTheorem: TheoremWithContext => Try[F[Theorem]]): Try[F[TheoremUpdateProps]] = {
    modifyEntry[Theorem, FWithValue[F, TheoremUpdateProps]#Type](bookKey, chapterKey, theoremKey, theoremWithContext => {
      getUpdatedTheorem(theoremWithContext).map { fTheorem =>
        import theoremWithContext._
        fTheorem.map { newTheoremWithoutReferenceChanges =>
          val (newTheoremWithReferenceChanges, stepsWithReferenceChanges) = RecalculateReferences(theoremWithContext.copy(entry = newTheoremWithoutReferenceChanges))
          val newInferenceIds = newTheoremWithReferenceChanges.referencedInferenceIds.diff(entry.referencedInferenceIds)
          val inferenceLinks = BookService.getInferenceLinks(newInferenceIds, theoremWithContext.globalContext)
          (newTheoremWithReferenceChanges, TheoremUpdateProps(newTheoremWithReferenceChanges, theoremWithContext.copy(entry = newTheoremWithReferenceChanges), inferenceLinks, stepsWithReferenceChanges))
        }
      }
    }).map(_.map(_._2))
  }

  def replaceSteps[F[_]: Functor](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: StepsWithContext => Try[F[Seq[Step]]]): Try[F[ProofUpdateProps[MultipleStepReplacementProps]]] = {
    modifyTheorem[FWithValue[F, MultipleStepReplacementProps]#Type](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      ReplaceSteps[TryFWithValue[F, MultipleStepReplacementProps]#Type](theoremWithContext, proofIndex, stepPath) { stepsWithContext =>
        Some(f(stepsWithContext).map(_.map { newSteps =>
          (newSteps, MultipleStepReplacementProps(stepPath, 0, stepsWithContext.steps.length, newSteps))
        }))
      }.orNotFound(s"Step $stepPath").flatten
    }.map(_.map { case (theoremUpdateProps, stepReplacementProps) =>
      ProofUpdateProps(
        MultipleStepReplacementProps(
          stepPath,
          stepReplacementProps.startIndex,
          stepReplacementProps.endIndex,
          if (stepPath.nonEmpty) FindStep(theoremUpdateProps.theoremWithContext, proofIndex, stepPath).get.step.asInstanceOf[Step.WithSubsteps].substeps else theoremUpdateProps.theorem.proofs(proofIndex).steps),
        theoremUpdateProps.newInferences,
        theoremUpdateProps.stepsWithReferenceChanges(proofIndex))
    })
  }

  def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: TypedStepWithContext[TStep] => Try[Seq[Step]]): Try[ProofUpdateProps[StepReplacementProps]] = {
    stepPath.initAndLastOption.map { case (init, last) =>
      replaceSteps[WithValue[Seq[Step]]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { stepsWithContext =>
        stepsWithContext.steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
          for {
            typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
            stepWithContext = stepsWithContext.atChild(before, typedStep)
            replacementSteps <- f(stepWithContext)
          } yield (before ++ replacementSteps ++ after, replacementSteps)
        }.orNotFound(s"Step $stepPath").flatten
      }.map { case (proofUpdateProps, replacementSteps) =>
        proofUpdateProps.withNewStepUpdateProps(StepReplacementProps(stepPath, proofUpdateProps.stepUpdates.newSteps.slice(last, last + replacementSteps.length)))
      }
    }.orNotFound(s"Step $stepPath").flatten
  }

  def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: TypedStepWithContext[TStep] => Try[Step]): Try[ProofUpdateProps[StepReplacementProps]] = {
    replaceStep[TStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { stepWithContext => f(stepWithContext).map(Seq(_)) }
  }

  def insertSteps[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: TypedStepWithContext[TStep] => Try[Seq[Step]]): Try[ProofUpdateProps[StepInsertionProps]] = {
    replaceStep[TStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath)(stepWithContext => f(stepWithContext).map { steps => steps :+ stepWithContext.step})
      .map { proofUpdateProps =>
        proofUpdateProps.withNewStepUpdateProps(StepInsertionProps(proofUpdateProps.stepUpdates.path, proofUpdateProps.stepUpdates.newSteps.init))
      }
  }
}

object BookService {
  def getBookUrl(bookWithContext: BookWithContext): String = s"/books/${bookWithContext.bookKey}"
  def getChapterUrl(chapterWithContext: ChapterWithContext): String = s"${getBookUrl(chapterWithContext.bookWithContext)}/${chapterWithContext.chapterKey}"
  def getEntryUrl(entryWithContext: EntryWithContext): String = s"${getChapterUrl(entryWithContext.chapterWithContext)}/${entryWithContext.entryKey}"

  def getInferenceLinks(inferenceIds: Set[String], globalContext: GlobalContext): Map[String, InferenceSummary] = {
    (for {
      bookWithContext <- globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      entryWithContext <- chapterWithContext.inferencesWithContexts
      inference <- entryWithContext.entry.inferences
      if inferenceIds.contains(inference.id)
    } yield inference.id -> InferenceSummary(inference.name, getEntryUrl(entryWithContext), globalContext.definitions.isInferenceComplete(inference))).toMap
  }
}
