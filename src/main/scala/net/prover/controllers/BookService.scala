package net.prover.controllers

import net.prover.books.management.{BookStateManager, ReloadBooks, UpdateBooks}
import net.prover.books.model.{Book, KeyAccumulator}
import net.prover.controllers.models._
import net.prover.entries._
import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.{Step, StepProvingContext}
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

  def reload(): Try[Any] = Try { ReloadBooks() }

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
    } yield stepWithContext.copy(step = step)
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
      } yield newChapterF.map(newChapter => (bookWithContext.book.copy(chapters = bookWithContext.book.chapters.replaceValue(chapterWithContext.chapter, newChapter)), newChapter))
    ).map(_.map { case (newBookWithContext, newChapter) => newBookWithContext.getChapter(newChapter) })
  }

  def modifyEntry[TEntry <: ChapterEntry : ClassTag, F[_] : Functor](bookKey: String, chapterKey: String, entryKey: String, f: TypedEntryWithContext[TEntry] => Try[F[TEntry]]): Try[F[TypedEntryWithContext[TEntry]]] = {
    modifyChapter[FWithValue[F, TEntry]#Type](bookKey, chapterKey, chapterWithContext =>
      for {
        entryWithContext <- chapterWithContext.getEntry(entryKey)
        newEntryF <- f(entryWithContext)
      } yield newEntryF.map(newEntry => (chapterWithContext.chapter.copy(entries = chapterWithContext.chapter.entries.replaceValue(entryWithContext.entry, newEntry)), newEntry))
    ).map(_.map { case (newChapterWithContext, newEntry) => newChapterWithContext.getEntry(newEntry) })
  }

  def modifyTheorem[F[_] : Functor](bookKey: String, chapterKey: String, theoremKey: String)(getUpdatedTheorem: TheoremWithContext => Try[F[Theorem]]): Try[F[TheoremUpdateProps]] = {
    modifyEntry[Theorem, FWithValue[F, TheoremUpdateProps]#Type](bookKey, chapterKey, theoremKey, theoremWithContext => {
      getUpdatedTheorem(theoremWithContext).map { fTheorem =>
        import theoremWithContext._
        fTheorem.map { newTheoremWithoutReferenceChanges =>
          val (newTheoremWithReferenceChanges, stepsWithReferenceChanges) = RecalculateReferences(newTheoremWithoutReferenceChanges, provingContext)
          val newInferenceIds = newTheoremWithReferenceChanges.referencedInferenceIds.diff(entry.referencedInferenceIds)
          val inferenceLinks = BookService.getInferenceLinks(newInferenceIds, theoremWithContext.globalContext)
          (newTheoremWithReferenceChanges, TheoremUpdateProps(newTheoremWithReferenceChanges, theoremWithContext.copy(entry = newTheoremWithReferenceChanges), inferenceLinks, stepsWithReferenceChanges))
        }
      }
    }).map(_.map(_._2))
  }

  def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (TStep, StepProvingContext) => Try[Step]): Try[ProofUpdateProps[StepReplacementProps]] = {
    replaceStep[TStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) => f(step, stepProvingContext).map(Seq(_)) }
  }

  def replaceSteps[F[_]: Functor](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (Seq[Step], StepProvingContext) => Try[F[Seq[Step]]]): Try[F[ProofUpdateProps[MultipleStepReplacementProps]]] = {
    modifyTheorem[FWithValue[F, MultipleStepReplacementProps]#Type](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      import theoremWithContext._
      ReplaceSteps[TryFWithValue[F, MultipleStepReplacementProps]#Type](entry, proofIndex, stepPath) { (steps, stepContext) =>
        Some(f(steps, StepProvingContext(stepContext, provingContext)).map(_.map { newSteps =>
          (newSteps, MultipleStepReplacementProps(stepPath, 0, steps.length, newSteps))
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

  def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (TStep, StepProvingContext) => Try[Seq[Step]]): Try[ProofUpdateProps[StepReplacementProps]] = {
    stepPath.initAndLastOption.map { case (init, last) =>
      replaceSteps[WithValue[Seq[Step]]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { (steps, stepProvingContext) =>
        steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
          for {
            typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
            replacementSteps <- f(typedStep, stepProvingContext.updateStepContext(_.addSteps(before).atIndex(last)))
          } yield (before ++ replacementSteps ++ after, replacementSteps)
        }.orNotFound(s"Step $stepPath").flatten
      }.map { case (proofUpdateProps, replacementSteps) =>
        proofUpdateProps.withNewStepUpdateProps(StepReplacementProps(stepPath, proofUpdateProps.stepUpdates.newSteps.slice(last, last + replacementSteps.length)))
      }
    }.orNotFound(s"Step $stepPath").flatten
  }

  def insertSteps[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (TStep, StepProvingContext) => Try[Seq[Step]]): Try[ProofUpdateProps[StepInsertionProps]] = {
    replaceStep[TStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath)((step, stepProvingContext) => f(step, stepProvingContext).map { steps => steps :+ step})
      .map { proofUpdateProps =>
        proofUpdateProps.withNewStepUpdateProps(StepInsertionProps(proofUpdateProps.stepUpdates.path, proofUpdateProps.stepUpdates.newSteps.init))
      }
  }
}

object BookService {
  def getBookUrl(bookKey: String): String = s"/books/$bookKey"
  def getBookUrl(bookWithContext: BookWithContext): String = getBookUrl(bookWithContext.bookKey)
  def getChapterUrl(bookKey: String, chapterKey: String): String = s"${getBookUrl(bookKey)}/$chapterKey"
  def getChapterUrl(chapterWithContext: ChapterWithContext): String = getChapterUrl(chapterWithContext.bookKey, chapterWithContext.chapterKey)
  def getEntryUrl(bookKey: String, chapterKey: String, entryKey: String): String = s"${getChapterUrl(bookKey, chapterKey)}/$entryKey"
  def getEntryUrl(entryWithContext: EntryWithContext): String = getEntryUrl(entryWithContext.bookKey, entryWithContext.chapterKey, entryWithContext.entryKey)

  def getBooksWithKeys(books: Seq[Book]): List[(Book, String)] = getWithKeys(books)(_.title)
  def getChaptersWithKeys(book: Book): List[(Chapter, String)] = getWithKeys(book.chapters)(_.title)
  def getEntriesWithKeys(chapter: Chapter): List[(ChapterEntry, String)] = getWithKeys(chapter.entries)(_.name)

  private def getWithKeys[T](seq: Seq[T])(keyProperty: T => String): List[(T, String)] = {
    seq.foldLeft((List.empty[(T, String)], KeyAccumulator.Empty)) { case ((results, acc), t) =>
      val (key, newAcc) = acc.getNextKey(keyProperty(t).formatAsKey)
      (results :+ (t -> key), newAcc)
    }._1
  }

  def getInferenceLinks(inferenceIds: Set[String], globalContext: GlobalContext): Map[String, InferenceSummary] = {
    (for {
      (book, bookKey) <- globalContext.booksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (inference, key) <- BookService.getEntriesWithKeys(chapter)
        .flatMap { case (entry, key) => entry.inferences.map(_ -> key) }
        .filter{ case (inference, _) => inferenceIds.contains(inference.id) }
    } yield inference.id -> InferenceSummary(inference.name, getEntryUrl(bookKey, chapterKey, key), globalContext.definitions.isInferenceComplete(inference))).toMap
  }
}
