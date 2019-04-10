package net.prover.controllers

import net.prover.controllers.models.{LinkSummary, PathData}
import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.{PremiseContext, Step, StepContext}

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

trait BookModification {
  def bookService: BookService

  def getBooksWithKeys(books: Seq[Book]): Seq[(Book, String)] = getWithKeys(books)(_.title)
  def getChaptersWithKeys(book: Book): Seq[(Chapter, String)] = getWithKeys(book.chapters)(_.title)
  def getEntriesWithKeys(chapter: Chapter): Seq[(ChapterEntry, String)] = getWithKeys(chapter.entries)(_.name)

  def getBookUrl(bookKey: String): String = s"/books/$bookKey"
  def getChapterUrl(bookKey: String, chapterKey: String): String = s"${getBookUrl(bookKey)}/$chapterKey"
  def getEntryUrl(bookKey: String, chapterKey: String, entryKey: String): String = s"${getChapterUrl(bookKey, chapterKey)}/$entryKey"

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

  protected def findBook(bookKey: String): Try[Book] = {
    findBook(bookService.books, bookKey)
  }

  protected def findBook(books: Seq[Book], bookKey: String): Try[Book] = {
    findBook(getBooksWithKeys(books), bookKey)
  }

  protected def findBook(booksWithKeys: Seq[(Book, String)], bookKey: String)(implicit dummyImplicit: DummyImplicit): Try[Book] = {
    booksWithKeys.find(_._2 == bookKey).map(_._1).orNotFound(s"Book $bookKey")
  }

  protected def findChapter(book: Book, chapterKey: String): Try[Chapter] = {
    findChapter(getChaptersWithKeys(book), chapterKey)
  }

  protected def findChapter(chaptersWithKeys: Seq[(Chapter, String)], chapterKey: String): Try[Chapter] = {
    chaptersWithKeys.find(_._2 == chapterKey).map(_._1).orNotFound(s"Chapter $chapterKey")
  }

  protected def findEntry[T <: ChapterEntry : ClassTag](chapter: Chapter, entryKey: String): Try[T] = {
    findEntry(getEntriesWithKeys(chapter), entryKey)
  }

  protected def findEntry[T <: ChapterEntry : ClassTag](entriesWithKeys: Seq[(ChapterEntry, String)], entryKey: String): Try[T] = {
    entriesWithKeys
      .find(_._2 == entryKey).map(_._1)
      .orNotFound(s"Entry $entryKey")
      .flatMap(_.asOptionalInstanceOf[T].orBadRequest(s"Entry is not a ${classTag[T].runtimeClass.getSimpleName}"))
  }

  protected def findStep[T <: Step : ClassTag](theorem: Theorem, stepPath: PathData, entryContext: EntryContext): Try[(T, StepContext, PremiseContext)] = {
    for {
      (rawStep, stepContext, premiseContext) <- theorem.findStep(stepPath.indexes, entryContext).orNotFound(s"Step $stepPath")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step is not ${classTag[T].runtimeClass.getName}")
    } yield {
      (step, stepContext, premiseContext)
    }
  }

  def modifyBook[T](bookKey: String, f: (Seq[Book], Book) => Try[(Book, T)]): Try[(Seq[Book], Book, T)] = {
    bookService.modifyBooks { books =>
      for {
        currentBook <- findBook(books, bookKey)
        (newBook, result) <- f(books, currentBook)
      } yield (books.replaceValue(currentBook, newBook), (newBook, result))
    }.map { case (books, (book, result)) => (books, book, result) }
  }

  def modifyChapter[T](bookKey: String, chapterKey: String, f: (Seq[Book], Book, Chapter) => Try[(Chapter, T)]): Try[(Seq[Book], Book, Chapter, T)] = {
    modifyBook(bookKey, (books, book) =>
      for {
        currentChapter <- findChapter(book, chapterKey)
        (newChapter, result) <- f(books, book, currentChapter)
      } yield (book.copy(chapters = book.chapters.replaceValue(currentChapter, newChapter)), (newChapter, result))
    ).map { case (books, book, (chapter, result)) => (books, book, chapter, result) }
  }

  def modifyEntry[TEntry <: ChapterEntry : ClassTag, TResult](bookKey: String, chapterKey: String, entryKey: String, f: (Seq[Book], Book, Chapter, TEntry) => Try[(TEntry, TResult)]): Try[(Seq[Book], Book, Chapter, TEntry, TResult)] = {
    modifyChapter(bookKey, chapterKey, (books, book, chapter) =>
      for {
        currentEntry <- findEntry[TEntry](chapter, entryKey)
        (newEntry, result) <- f(books, book, chapter, currentEntry)
      } yield (chapter.copy(entries = chapter.entries.replaceValue(currentEntry, newEntry)), (newEntry, result))
    ).map { case (books, book, chapter, (entry, result)) => (books, book, chapter, entry, result) }
  }

  def addChapterEntry(bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => Try[ChapterEntry]): Try[(Seq[Book], Book, Chapter)] = {
    modifyChapter(bookKey, chapterKey, (books, book, chapter) =>
      f(books, book, chapter).map(chapter.addEntry).map(_ -> ())
    ).map { case (books, book, chapter, _) => (books, book, chapter) }
  }

  case class TheoremProps(theorem: Theorem, newInferences: Map[String, LinkSummary])
  protected def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Theorem, EntryContext) => Try[Theorem]): Try[TheoremProps] = {
    modifyEntry[Theorem, TheoremProps](bookKey, chapterKey, theoremKey, (books, book, chapter, theorem) => {
      val entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      for {
        newTheorem <- f(theorem, entryContext).map(_.recalculateReferences(entryContext))
        newInferenceIds = newTheorem.referencedInferenceIds.diff(theorem.referencedInferenceIds)
        inferenceLinks = getInferenceLinks(newInferenceIds)
      } yield (newTheorem, TheoremProps(newTheorem, inferenceLinks))
    }).map(_._5)
  }

  protected def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepPath: PathData)(f: (TStep, StepContext, EntryContext) => Try[Step]): Try[TheoremProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, entryContext) =>
      theorem.tryModifyStep(stepPath.indexes, entryContext, (step, stepContext) => {
        for {
          typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(typedStep, stepContext, entryContext)
        } yield newStep
      }).orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepPath: PathData)(f: (TStep, StepContext, PremiseContext, EntryContext) => Try[Seq[Step]]): Try[TheoremProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, entryContext) =>
      (stepPath.indexes match {
        case init :+ last =>
          theorem.tryModifySteps(init, entryContext, (steps, stepContext, premiseContext) => {
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              for {
                typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
                replacementSteps <- f(typedStep, stepContext.atIndex(last), premiseContext.addSteps(before, stepContext), entryContext)
              } yield before ++ replacementSteps ++ after
            }
          })
        case _ =>
          None
      }).orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def findInference(inferenceId: String)(implicit entryContext: EntryContext): Try[Inference.Summary] = {
    entryContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  protected def getInferenceLinks(inferenceIds: Set[String]): Map[String, LinkSummary] = {
    (for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      (inference, key) <- getEntriesWithKeys(chapter)
        .flatMap { case (entry, key) => entry.inferences.map(_ -> key) }
        .filter{ case (inference, key) => inferenceIds.contains(inference.id) }
    } yield inference.id -> LinkSummary(inference.name, getEntryUrl(bookKey, chapterKey, key))).toMap
  }
}