package net.prover.controllers

import net.prover.controllers.models.{InferenceSummary, LinkSummary, PathData}
import net.prover.model._
import net.prover.model.definitions.{BinaryRelation, BinaryStatement, Definitions, Equality, Transitivity}
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.{Expression, Term}
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

  def modifyBook[F[_] : Functor](bookKey: String, f: (Seq[Book], Definitions, Book) => Try[F[Book]]): Try[F[(Seq[Book], Definitions, Book)]] = {
    bookService.modifyBooks[TryFWithValue[F, Book]#Type] { (books, definitions) =>
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

  def addChapterEntry(bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => Try[ChapterEntry]): Try[(Seq[Book], Definitions, Book, Chapter)] = {
    modifyChapter[Identity](bookKey, chapterKey, (books, _, book, chapter) =>
      f(books, book, chapter).map(chapter.addEntry)
    )
  }

  case class TheoremUpdateProps(theorem: Theorem, newInferences: Map[String, InferenceSummary])
  protected def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Theorem, ProvingContext) => Try[Theorem]): Try[TheoremUpdateProps] = {
    modifyEntry[Theorem, WithValue[TheoremUpdateProps]#Type](bookKey, chapterKey, theoremKey, (books, definitions, book, chapter, theorem) => {
      implicit val provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      for {
        newTheorem <- f(theorem, provingContext).map(_.recalculateReferences(provingContext))
        newInferenceIds = newTheorem.referencedInferenceIds.diff(theorem.referencedInferenceIds)
        inferenceLinks = getInferenceLinks(newInferenceIds, definitions)
      } yield (newTheorem, TheoremUpdateProps(newTheorem, inferenceLinks))
    }).map(_._2)
  }

  implicit def toIndexes(pathData: PathData): Seq[Int] = pathData.indexes

  protected def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (TStep, StepProvingContext) => Try[Step]): Try[TheoremUpdateProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      theorem.modifyStep[Try](proofIndex, stepPath) { (step, stepContext) =>
        for {
          typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(typedStep, StepProvingContext(stepContext, provingContext))
        } yield newStep
      }.orNotFound(s"Step $stepPath").flatten
    }
  }
  protected def modifySteps(bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: Seq[Int])(f: (Seq[Step], StepProvingContext) => Try[Seq[Step]]): Try[TheoremUpdateProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      theorem.modifySteps[Try](proofIndex, stepPath) { (steps, stepContext) =>
        Some(f(steps, StepProvingContext(stepContext, provingContext)))
      }.orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (TStep, StepProvingContext) => Try[Seq[Step]]): Try[TheoremUpdateProps] = {
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

  protected def replaceStepAndAddBeforeTransitivity[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, proofIndex: Int, stepPath: PathData)(f: (TStep, StepProvingContext) => Try[(Step, Seq[Step])]): Try[TheoremUpdateProps] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
      (stepPath.indexes match {
        case init :+ last =>
          theorem.modifySteps[Try](proofIndex, init) { (steps, stepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              for {
                typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
                (replacementStep, stepsToAddBeforeTransitive) <- f(typedStep, StepProvingContext(stepContext.addSteps(before).atIndex(last), provingContext))
              } yield insertTargetsBeforeTransitivity(before, replacementStep +: after, stepsToAddBeforeTransitive)(StepProvingContext(stepContext, provingContext))
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

  protected def getInferenceLinks(inferenceIds: Set[String], definitions: Definitions): Map[String, InferenceSummary] = {
    (for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      (inference, key) <- getEntriesWithKeys(chapter)
        .flatMap { case (entry, key) => entry.inferences.map(_ -> key) }
        .filter{ case (inference, _) => inferenceIds.contains(inference.id) }
    } yield inference.id -> InferenceSummary(inference.name, getEntryUrl(bookKey, chapterKey, key), definitions.isInferenceComplete(inference))).toMap
  }

  def getInferenceUsages(entry: ChapterEntry, books: Seq[Book]): Seq[(String, String, Seq[LinkSummary])] = {
    val inferenceIds = entry.inferences.map(_.id).toSet
    for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      theoremsWithKeys = getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
        .filter(_._1.referencedInferenceIds.intersect(inferenceIds).nonEmpty)
      if theoremsWithKeys.nonEmpty
    } yield (book.title, chapter.title, theoremsWithKeys.map { case (theorem, key) => LinkSummary(theorem.name, getEntryUrl(bookKey, chapterKey, key))})
  }

  def hasUsages(entry: ChapterEntry, books: Seq[Book]): Boolean = {
    hasUsages(Seq(entry), books.iterator.flatMap(_.chapters).flatMap(_.entries))
  }

  def hasUsages(entriesPotentiallyBeingUsed: Seq[ChapterEntry], entriesPotentiallyUsing: TraversableOnce[ChapterEntry]): Boolean = {
    val inferenceIds = entriesPotentiallyBeingUsed.flatMap(_.inferences.map(_.id)).toSet
    entriesPotentiallyUsing.exists(e => e.referencedInferenceIds.intersect(inferenceIds).nonEmpty || e.referencedDefinitions.exists(entriesPotentiallyBeingUsed.contains))
  }
}
