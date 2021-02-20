package net.prover.structure

import net.prover._
import net.prover.controllers.models._
import net.prover.model._
import net.prover.model.definitions.Definitions
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.structure.datatransfer.InferenceSummary
import net.prover.structure.model.entries.{ChapterEntry, Theorem}
import net.prover.structure.model.{Book, Chapter}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import scalaz.Functor
import scalaz.syntax.functor._

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

@Service
class BookService @Autowired() (bookRepository: BookRepository) {
  def booksAndDefinitions: (Seq[Book], Definitions) = bookRepository.booksAndDefinitions
  def books: Seq[Book] = bookRepository.books
  def getBooksWithKeys: Seq[(Book, String)] = BookService.getBooksWithKeys(books)

  def reload(): Try[Any] = bookRepository.reload()

  def findBook(bookKey: String): Try[Book] = {
    findBook(bookRepository.books, bookKey)
  }

  def findBook(books: Seq[Book], bookKey: String): Try[Book] = {
    findBook(BookService.getBooksWithKeys(books), bookKey)
  }

  def findBook(booksWithKeys: Seq[(Book, String)], bookKey: String)(implicit dummyImplicit: DummyImplicit): Try[Book] = {
    booksWithKeys.find(_._2 == bookKey).map(_._1).orNotFound(s"Book $bookKey")
  }

  def findChapter(book: Book, chapterKey: String): Try[Chapter] = {
    findChapter(BookService.getChaptersWithKeys(book), chapterKey)
  }

  def findChapter(chaptersWithKeys: Seq[(Chapter, String)], chapterKey: String): Try[Chapter] = {
    chaptersWithKeys.find(_._2 == chapterKey).map(_._1).orNotFound(s"Chapter $chapterKey")
  }

  def findEntry[T <: ChapterEntry : ClassTag](chapter: Chapter, entryKey: String): Try[T] = {
    findEntry(BookService.getEntriesWithKeys(chapter), entryKey)
  }

  def findEntry[T <: ChapterEntry : ClassTag](entriesWithKeys: Seq[(ChapterEntry, String)], entryKey: String): Try[T] = {
    entriesWithKeys
      .find(_._2 == entryKey).map(_._1)
      .orNotFound(s"Entry $entryKey")
      .flatMap(_.asOptionalInstanceOf[T].orBadRequest(s"Entry is not a ${classTag[T].runtimeClass.getSimpleName}"))
  }

  def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData): Try[(T, StepProvingContext)] = {
    val (books, definitions) = bookRepository.booksAndDefinitions
    for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (rawStep, stepContext) <- theorem.findStep(proofIndex, stepPath.indexes).orNotFound(s"Step $stepPath")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step is not ${classTag[T].runtimeClass.getName}")
    } yield (step, StepProvingContext(stepContext, provingContext))
  }

  def modifyBooks[F[_] : Functor](f: (Seq[Book], Definitions) => F[Seq[Book]]): F[(Seq[Book], Definitions)] = bookRepository.modifyBooks(f)

  def modifyBook[F[_] : Functor](bookKey: String, f: (Seq[Book], Definitions, Book) => Try[F[Book]]): Try[F[(Seq[Book], Definitions, Book)]] = {
    bookRepository.modifyBooks[TryFWithValue[F, Book]#Type] { (books, definitions) =>
      for {
        currentBook <- findBook(books, bookKey)
        newBookF <- f(books, definitions, currentBook)
      } yield newBookF.map(newBook => (books.replaceValue(currentBook, newBook), newBook))
    }.map(_.map { case ((books, definitions), book) => (books, definitions, book)})
  }

  def modifyChapter[F[_] : Functor](bookKey: String, chapterKey: String, f: (Seq[Book], Definitions, Book, Chapter) => Try[F[Chapter]]): Try[F[(Seq[Book], Definitions, Book, Chapter)]] = {
    modifyBook[FWithValue[F, Chapter]#Type](bookKey, (books, definitions, book) =>
      for {
        currentChapter <- findChapter(book, chapterKey)
        newChapterF <- f(books, definitions, book, currentChapter)
      } yield newChapterF.map(newChapter => (book.copy(chapters = book.chapters.replaceValue(currentChapter, newChapter)), newChapter))
    ).map(_.map { case ((books, definitions, book), chapter) => (books, definitions, book, chapter)})
  }

  def modifyEntry[TEntry <: ChapterEntry : ClassTag, F[_] : Functor](bookKey: String, chapterKey: String, entryKey: String, f: (Seq[Book], Definitions, Book, Chapter, TEntry) => Try[F[TEntry]]): Try[F[(Seq[Book], Definitions, Book, Chapter, TEntry)]] = {
    modifyChapter[FWithValue[F, TEntry]#Type](bookKey, chapterKey, (books, definitions, book, chapter) =>
      for {
        currentEntry <- findEntry[TEntry](chapter, entryKey)
        newEntryF <- f(books, definitions, book, chapter, currentEntry)
      } yield newEntryF.map(newEntry => (chapter.copy(entries = chapter.entries.replaceValue(currentEntry, newEntry)), newEntry))
    ).map(_.map  { case ((books, definitions, book, chapter), entry) => (books, definitions, book, chapter, entry) })
  }

  def modifyTheorem[F[_] : Functor](bookKey: String, chapterKey: String, theoremKey: String)(getUpdatedTheorem: (Theorem, ProvingContext) => Try[F[Theorem]]): Try[F[TheoremUpdateProps]] = {
    modifyEntry[Theorem, FWithValue[F, TheoremUpdateProps]#Type](bookKey, chapterKey, theoremKey, (books, definitions, book, chapter, theorem) => {
      implicit val provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      getUpdatedTheorem(theorem, provingContext).map { fTheorem =>
        fTheorem.map { newTheoremWithoutReferenceChanges =>
          val (newTheoremWithReferenceChanges, stepsWithReferenceChanges) = newTheoremWithoutReferenceChanges.recalculateReferences(provingContext)
          val newInferenceIds = newTheoremWithReferenceChanges.referencedInferenceIds.diff(theorem.referencedInferenceIds)
          val inferenceLinks = BookService.getInferenceLinks(newInferenceIds, books, definitions)
          (newTheoremWithReferenceChanges, TheoremUpdateProps(newTheoremWithReferenceChanges, inferenceLinks, stepsWithReferenceChanges))
        }
      }
    }).map(_.map(_._2))
  }

  def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (TStep, StepProvingContext) => Try[Step]): Try[ProofUpdateProps[StepReplacementProps]] = {
    replaceStep[TStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) => f(step, stepProvingContext).map(Seq(_)) }
  }

  def replaceSteps[F[_]: Functor](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (Seq[Step], StepProvingContext) => Try[F[Seq[Step]]]): Try[F[ProofUpdateProps[MultipleStepReplacementProps]]] = {
    modifyTheorem[FWithValue[F, MultipleStepReplacementProps]#Type](bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      theorem.replaceSteps[TryFWithValue[F, MultipleStepReplacementProps]#Type](proofIndex, stepPath) { (steps, stepContext) =>
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
          if (stepPath.nonEmpty) theoremUpdateProps.theorem.findStep(proofIndex, stepPath).get._1.asInstanceOf[Step.WithSubsteps].substeps else theoremUpdateProps.theorem.proofs(proofIndex).steps),
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
  def getChapterUrl(bookKey: String, chapterKey: String): String = s"${getBookUrl(bookKey)}/$chapterKey"
  def getEntryUrl(bookKey: String, chapterKey: String, entryKey: String): String = s"${getChapterUrl(bookKey, chapterKey)}/$entryKey"

  def getBooksWithKeys(books: Seq[Book]): Seq[(Book, String)] = getWithKeys(books)(_.title)
  def getChaptersWithKeys(book: Book): Seq[(Chapter, String)] = getWithKeys(book.chapters)(_.title)
  def getEntriesWithKeys(chapter: Chapter): Seq[(ChapterEntry, String)] = getWithKeys(chapter.entries)(_.name)

  private def getWithKeys[T](seq: Seq[T])(keyProperty: T => String): Seq[(T, String)] = {
    seq.foldLeft(Seq.empty[(T, String)]) { case (acc, t) =>
      def keyExists(k: String): Boolean = acc.exists(_._2 == k)
      val initialKey = keyProperty(t).formatAsKey
      val key = if (keyExists(initialKey))
        Stream.from(2)
          .mapFind(i => Some(s"$initialKey-$i").filter(s => !keyExists(s)))
          .getOrElse(throw new Exception("Map somehow contained every possible key"))
      else
        initialKey
      acc :+ (t -> key)
    }
  }

  def getInferenceLinks(inferenceIds: Set[String], books: Seq[Book], definitions: Definitions): Map[String, InferenceSummary] = {
    (for {
      (book, bookKey) <- getBooksWithKeys(books)
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (inference, key) <- BookService.getEntriesWithKeys(chapter)
        .flatMap { case (entry, key) => entry.inferences.map(_ -> key) }
        .filter{ case (inference, _) => inferenceIds.contains(inference.id) }
    } yield inference.id -> InferenceSummary(inference.name, getEntryUrl(bookKey, chapterKey, key), definitions.isInferenceComplete(inference))).toMap
  }
}
