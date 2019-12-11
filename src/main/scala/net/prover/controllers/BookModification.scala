package net.prover.controllers

import net.prover.controllers.models.{LinkSummary, PathData}
import net.prover.model._
import net.prover.model.definitions.{Definitions, Equality}
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.Term
import net.prover.model.proof.{Step, StepContext, StepProvingContext}
import scalaz.Functor
import scalaz.syntax.functor._

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

  protected def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData): Try[(T, StepProvingContext)] = {
    val (books, definitions) = bookService.booksAndDefinitions
    for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (rawStep, stepContext) <- theorem.findStep(proofIndex, stepPath.indexes).orNotFound(s"Step $stepPath")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step is not ${classTag[T].runtimeClass.getName}")
    } yield (step, StepProvingContext(stepContext, provingContext))
  }

  def modifyBook[F[_] : Functor](bookKey: String, f: (Seq[Book], Definitions, Book) => Try[F[Book]]): Try[F[(Seq[Book], Book)]] = {
    bookService.modifyBooks[TryFWithValue[F, Book]#Type] { (books, definitions) =>
      for {
        currentBook <- findBook(books, bookKey)
        newBookF <- f(books, definitions, currentBook)
      } yield newBookF.map(newBook => (books.replaceValue(currentBook, newBook), newBook))
    }
  }

  def modifyChapter[F[_] : Functor](bookKey: String, chapterKey: String, f: (Seq[Book], Definitions, Book, Chapter) => Try[F[Chapter]]): Try[F[(Seq[Book], Book, Chapter)]] = {
    modifyBook[FWithValue[F, Chapter]#Type](bookKey, (books, definitions, book) =>
      for {
        currentChapter <- findChapter(book, chapterKey)
        newChapterF <- f(books, definitions, book, currentChapter)
      } yield newChapterF.map(newChapter => (book.copy(chapters = book.chapters.replaceValue(currentChapter, newChapter)), newChapter))
    ).map(_.map { case ((books, book), chapter) => (books, book, chapter)})
  }

  def modifyEntry[TEntry <: ChapterEntry : ClassTag, F[_] : Functor](bookKey: String, chapterKey: String, entryKey: String, f: (Seq[Book], Definitions, Book, Chapter, TEntry) => Try[F[TEntry]]): Try[F[(Seq[Book], Book, Chapter, TEntry)]] = {
    modifyChapter[FWithValue[F, TEntry]#Type](bookKey, chapterKey, (books, definitions, book, chapter) =>
      for {
        currentEntry <- findEntry[TEntry](chapter, entryKey)
        newEntryF <- f(books, definitions, book, chapter, currentEntry)
      } yield newEntryF.map(newEntry => (chapter.copy(entries = chapter.entries.replaceValue(currentEntry, newEntry)), newEntry))
    ).map(_.map  { case ((books, book, chapter), entry) => (books, book, chapter, entry) })
  }

  def addChapterEntry(bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => Try[ChapterEntry]): Try[(Seq[Book], Book, Chapter)] = {
    modifyChapter[Identity](bookKey, chapterKey, (books, _, book, chapter) =>
      f(books, book, chapter).map(chapter.addEntry)
    )
  }

  case class TheoremProps(theorem: Theorem, newInferences: Map[String, LinkSummary])
  protected def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Theorem, ProvingContext) => Try[Theorem]): Try[TheoremProps] = {
    modifyEntry[Theorem, WithValueB[TheoremProps]#Type](bookKey, chapterKey, theoremKey, (books, definitions, book, chapter, theorem) => {
      val provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      for {
        newTheorem <- f(theorem, provingContext).map(_.recalculateReferences(provingContext))
        newInferenceIds = newTheorem.referencedInferenceIds.diff(theorem.referencedInferenceIds)
        inferenceLinks = getInferenceLinks(newInferenceIds)
      } yield (newTheorem, TheoremProps(newTheorem, inferenceLinks))
    }).map(_._2)
  }

  protected def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (TStep, StepProvingContext) => Try[Step]): Try[TheoremProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      theorem.modifyStep[Try](proofIndex, stepPath.indexes, (step, stepContext) => {
        for {
          typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(typedStep, StepProvingContext(stepContext, provingContext))
        } yield newStep
      }).orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (TStep, StepProvingContext) => Try[Seq[Step]]): Try[TheoremProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      (stepPath.indexes match {
        case init :+ last =>
          theorem.modifySteps[Try](proofIndex, init) { (steps, stepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              for {
                typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
                replacementSteps <- f(typedStep, StepProvingContext(stepContext.addSteps(before).atIndex(last), provingContext))
              } yield before ++ replacementSteps ++ after
            }
          }
        case _ =>
          None
      }).orNotFound(s"Step $stepPath").flatten
    }
  }

  private def splitPrecedingStepsWhileTransitive(before: Seq[Step], step: Step)(implicit stepProvingContext: StepProvingContext): (Seq[Step], Seq[Step]) = {
    @scala.annotation.tailrec
    def takeWhileTransitive(steps: Seq[Step], lhs: Term, rhs: Term, acc: Seq[Step], equality: Equality): (Seq[Step], Seq[Step]) = {
      steps match {
        case first :+ preceding :+ (transitive: Step.Assertion)
          if transitive.inference.id == equality.transitivity.inference.id
        =>
          (preceding.provenStatement, transitive.statement) match {
            case (Some(equality(`lhs`, middle1)), equality(middle2, `rhs`)) if middle1 == middle2 =>
              takeWhileTransitive(first, lhs, middle1, Seq(preceding, transitive) ++ acc, equality)
            case _ =>
              (steps, acc)
          }
        case _ =>
          (steps, acc)
      }
    }
    (for {
      equality <- stepProvingContext.provingContext.equalityOption
      statement <- step.provenStatement
      (lhs, rhs) <- equality.unapply(statement)
    } yield takeWhileTransitive(before, lhs, rhs, Nil, equality)) getOrElse (before, Nil)
  }

  protected def insertTargetsBeforeTransitivity(before: Seq[Step], old: Step, after: Seq[Step], newSteps: Seq[Step], newTargets: Seq[Step])(implicit stepProvingContext: StepProvingContext): Seq[Step] = {
    val (existingStepsBeforeTransitive, transitiveSteps) = splitPrecedingStepsWhileTransitive(before, old)
    (existingStepsBeforeTransitive ++ newTargets ++ transitiveSteps ++ newSteps ++ after)
  }

  protected def replaceStepAndAddBeforeTransitivity[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (TStep, StepProvingContext) => Try[(Step, Seq[Step])]): Try[TheoremProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      (stepPath.indexes match {
        case init :+ last =>
          theorem.modifySteps[Try](proofIndex, init) { (steps, stepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              for {
                typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
                (replacementStep, stepsToAddBeforeTransitive) <- f(typedStep, StepProvingContext(stepContext.addSteps(before).atIndex(last), provingContext))
              } yield insertTargetsBeforeTransitivity(before, step, after, Seq(replacementStep), stepsToAddBeforeTransitive)(StepProvingContext(stepContext, provingContext))
            }
          }
        case _ =>
          None
      }).orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def findInference(inferenceId: String)(implicit stepProvingContext: StepProvingContext): Try[Inference.Summary] = {
    stepProvingContext.provingContext.entryContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
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
