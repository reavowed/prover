package net.prover.structure

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.locks.{Lock, ReentrantLock}

import com.googlecode.concurentlocks.ReentrantReadWriteUpdateLock
import net.prover._
import net.prover.model.definitions.Definitions
import net.prover.structure.model.Book
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.stereotype.Service
import scalaz.Functor
import scalaz.syntax.functor._

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.Try

@Service
class BookRepository {
  private val bookDirectoryPath = Paths.get("books")

  trait UpdateAction {
    def runActionOnThisThread(books: Seq[Book], definitions: Definitions): UpdateActionResult
  }
  trait UpdateActionResult {
    def isSuccess: Boolean
    def withResult(action: (Seq[Book], Definitions) => Unit): Unit
  }

  case class TypedUpdateAction[F[_] : Functor](action: (Seq[Book], Definitions) => F[Seq[Book]]) extends UpdateAction {
    private val promise: Promise[F[(Seq[Book], Definitions)]] = Promise[F[(Seq[Book], Definitions)]]()
    val future: Future[F[(Seq[Book], Definitions)]] = promise.future

    def runActionOnThisThread(books: Seq[Book], definitions: Definitions): TypedUpdateActionResult[F] = {
      val result = Try {
        for {
          newBooks <- action(books, definitions)
        } yield {
          val newDefinitions = getDefinitions(newBooks)
          (newBooks, newDefinitions)
        }
      }
      promise.complete(result)
      TypedUpdateActionResult(result)
    }
  }
  case class TypedUpdateActionResult[F[_] : Functor](result: Try[F[(Seq[Book], Definitions)]]) extends UpdateActionResult {
    override def isSuccess: Boolean = result.isSuccess
    def withResult(action: (Seq[Book], Definitions) => Unit): Unit = {
      result.foreach(_.map[Unit](action.tupled))
    }
  }

  private var isUpdateThreadRunning: Boolean = false
  private val updateActivityLock = new ReentrantLock()
  private val pendingUpdateActions = new ConcurrentLinkedQueue[UpdateAction]

  private val bookLock = new ReentrantReadWriteUpdateLock()
  private val writeLock = new ReentrantLock()
  private var _booksAndDefinitions: (Seq[Book], Definitions) = (Nil, Definitions(EntryContext(Nil)))

  ExecutionContext.global.execute(() => {
    val initialParseAction = TypedUpdateAction[Identity]((_, _) => parseBooksInitiallyUnsafe())
    queueUpdateActions(initialParseAction)
    Await.ready(initialParseAction.future, Duration.Inf).value.get.recover {
      case e => BookRepository.logger.error("Error parsing books", e)
    }
  })

  def booksAndDefinitions: (Seq[Book], Definitions) = withLock(bookLock.readLock()) {
    _booksAndDefinitions
  }
  def books: Seq[Book] = booksAndDefinitions._1

  private def withLock[T](lock: Lock)(t: => T): T = {
    lock.lock()
    try {
      t
    } finally {
      lock.unlock()
    }
  }

  private def queueUpdateActions(actions: UpdateAction*): Unit = {
    withLock(updateActivityLock) {
      pendingUpdateActions.addAll(actions.asJava)
      if (!isUpdateThreadRunning) {
        startUpdateThread()
      }
    }
  }
  private def startUpdateThread(): Unit = {
    withLock(updateActivityLock) {
      if (!isUpdateThreadRunning) {
        isUpdateThreadRunning = true
        ExecutionContext.global.execute(() => processUpdateActions())
      }
    }
  }
  private def processUpdateActions(): Unit = {
    val results = Seq.newBuilder[UpdateActionResult]
    def processNextAction(): Boolean = {
      val nextAction = withLock(updateActivityLock) {
        val nextAction = Option(pendingUpdateActions.poll())
        if (nextAction.isEmpty) {
          isUpdateThreadRunning = false
        }
        nextAction
      }
      for {action <- nextAction} {
        val result = (action.runActionOnThisThread _).tupled(_booksAndDefinitions)
        withLock(bookLock.writeLock()) {
          result.withResult { (books, definitions) =>
            _booksAndDefinitions = (books, definitions)
          }
        }
        results += result
      }
      nextAction.isDefined
    }
    withLock(bookLock.updateLock()) {
      while (processNextAction()) {}
      writeLock.lock()
    }
    try {
      val lastSuccessfulResult = results.result().filter(_.isSuccess).lastOption
      lastSuccessfulResult.foreach(_.withResult { (books, _) => writeBooks(books) })
    } finally {
      writeLock.unlock()
    }
  }

  private def parseBooksInitiallyUnsafe(): Seq[Book] = {
    val books = getBookList.mapReduceWithPrevious[Book] { case (booksSoFar, bookTitle) =>
      val newBook = parseBook(bookTitle, booksSoFar)
      val definitions = getDefinitions(booksSoFar :+ newBook)
      withLock(bookLock.writeLock()) {
        _booksAndDefinitions = (booksSoFar :+ newBook, definitions)
      }
      newBook
    }
    BookRepository.logger.info(s"Parsed ${books.length} books")
    books
  }

  def modifyBooks[F[_] : Functor](f: (Seq[Book], Definitions) => F[Seq[Book]]): F[(Seq[Book], Definitions)] = {
    val updateAction = TypedUpdateAction[F](f)
    queueUpdateActions(updateAction)
    Await.result(updateAction.future, Duration.Inf)
  }

  def reload(): Try[Any] = {
    val clearAction = TypedUpdateAction[Identity]((_, _) => Nil)
    val reloadAction = TypedUpdateAction[Identity]((_, _) => parseBooksInitiallyUnsafe())
    queueUpdateActions(clearAction, reloadAction)
    Await.ready(reloadAction.future, Duration.Inf).value.get
  }

  private def parseBook(title: String, previousBooks: Seq[Book]): Book = {
    BookRepository.logger.info(s"Parsing book $title")
    Book.parse(title, getBookPath(title), previousBooks, getChapterPath(title, _, _))
  }

  private def getBookList: Seq[String] = {
    Files
      .readAllLines(getBookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
  }

  private def writeBooks(books: Seq[Book]): Unit = {
    writeBooklist(books)
    books.foreach(writeBookAndChapterFiles)
  }

  case class FilePathAndContents(path: Path, getContents: () => String) {
    def write(): Unit = {
      Files.write(path, getContents().getBytes("UTF-8"))
    }
  }

  private def deleteUnusedFiles(directoryPath: Path, filePathsAndContents: Seq[FilePathAndContents]): Unit = {
    FileUtils.listFiles(directoryPath.toFile, TrueFileFilter.INSTANCE, null).asScala.foreach { file =>
      if (!filePathsAndContents.exists(_.path.toAbsolutePath.toString == file.getAbsolutePath)) {
        BookRepository.logger.info(s"Deleting file ${file.getAbsolutePath}")
        file.delete()
      }
    }
  }

  private def writeBooklist(books: Seq[Book]): Unit = {
    val file = FilePathAndContents(getBookListPath, () => books.map(_.title).mkString("\n") + "\n")
    file.write()
    deleteUnusedFiles(bookDirectoryPath, Seq(file))
  }

  private def writeBookAndChapterFiles(book: Book): Unit = {
    Files.createDirectories(getBookPath(book.title).getParent)
    val files = FilePathAndContents(getBookPath(book.title), () => book.serialized) +:
      book.chapters.mapWithIndex((chapter, index) => FilePathAndContents(getChapterPath(book.title, chapter.title, index), () => chapter.serialized ))
    files.foreach(_.write())
    deleteUnusedFiles(bookDirectoryPath.resolve(book.title.formatAsKey), files)
  }

  private def getBookListPath: Path = {
    bookDirectoryPath.resolve("books.list")
  }

  private def getBookPath(title: String): Path = {
    val key = title.formatAsKey
    bookDirectoryPath.resolve(key).resolve(key + ".book")
  }

  private def getChapterPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    bookDirectoryPath.resolve(bookTitle.formatAsKey).resolve("%02d".format(chapterIndex + 1) + "." + chapterTitle.camelCase + ".chapter")
  }

  private def getDefinitions(books: Seq[Book]): Definitions = {
    Definitions(EntryContext(books.flatMap(_.chapters).flatMap(_.entries)))
  }
}

object BookRepository {
  val logger: Logger = LoggerFactory.getLogger(BookRepository.getClass)
}
