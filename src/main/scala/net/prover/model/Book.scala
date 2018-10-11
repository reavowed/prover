package net.prover.model

import java.nio.file.Path

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries._
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

@JsonIgnoreProperties(Array("dependencies"))
case class Book(
    title: String,
    key: Book.Key,
    dependencies: Seq[Book],
    chapters: Seq[Chapter],
    statementVariableNames: Seq[String],
    termVariableNames: Seq[String]) {
  implicit def displayContext: DisplayContext = DisplayContext(allTransitive(_.displayShorthands))

  def inferences: Seq[Inference] = chapters.flatMap(_.inferences)
  def theorems: Seq[Theorem] = chapters.flatMap(_.theorems)
  def statementDefinitions: Seq[StatementDefinition] = chapters.flatMap(_.statementDefinitions)
  def displayShorthands: Seq[DisplayShorthand] = chapters.flatMap(_.displayShorthands)

  def allTransitive[T](f: Book => Seq[T]): Seq[T] = (dependencies.transitive :+ this).flatMap(f)

  def serialized: String = {
    Seq(
      dependencies.map(d => s"import ${d.title}"),
      Seq(s"variables (${statementVariableNames.mkString(" ")}) (${termVariableNames.mkString(" ")})"),
      chapters.map(c => s"chapter ${c.title}")
    ).map(_.mkString("\n")).mkString("\n\n") + "\n"
  }
}

object Book {
  val logger = LoggerFactory.getLogger(Book.getClass)

  case class Key(value: String) {
    def url = s"/books/$value"
  }

  def parse(title: String, path: Path, previousBooks: Seq[Book], getChapterPath: (String, Int) => Path): Option[Book] = {
    Try(parser(title, previousBooks, getChapterPath).parse(Tokenizer.fromPath(path, s"book '$title'"))._1) match {
      case Success(bookOutline) =>
        Some(bookOutline)
      case Failure(e) =>
        logger.error(s"Error parsing book '$title'\n${e.getMessage}")
        None
    }
  }

  def parser(title: String, previousBooks: Seq[Book], getChapterPath: (String, Int) => Path): Parser[Book] = {
    val key = Key(title.formatAsKey)
    for {
      imports <- importsParser
      dependencies = imports.map { importTitle =>
        previousBooks.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      statementAndTermVariableNames <- variableDefinitionsParser
      chapterTitles <- chapterTitlesParser
    } yield {
      val (statementVariableNames, termVariableNames) = statementAndTermVariableNames
      val transitiveDependencies = dependencies.transitive
      val initialContext = ParsingContext(
        transitiveDependencies.inferences,
        transitiveDependencies.statementDefinitions,
        transitiveDependencies.termDefinitions,
        statementVariableNames.toSet,
        termVariableNames.toSet,
        Seq.empty)
      val chapters = chapterTitles.zipWithIndex.mapFold(initialContext) { case (context, (chapterTitle, index)) =>
        val chapterPath = getChapterPath(chapterTitle, index)
        val (chapterOutline, newContext) = Chapter.parser(chapterTitle, key)(context)
          .parseAndDiscard(Tokenizer.fromPath(chapterPath, s"book '$title' chapter '$chapterTitle'"))
        (newContext, chapterOutline)
      }._2
      Book(
        title,
        key,
        dependencies,
        chapters,
        statementVariableNames,
        termVariableNames)
    }
  }

  private def importsParser: Parser[Seq[String]] = {
    Parser
      .optionalWord("import")
      .flatMapMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

  def variableDefinitionsParser: Parser[(Seq[String], Seq[String])] = {
    Parser.optionalWord("variables").flatMapMap { _ =>
      for {
        statementVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
        termVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
      } yield {
        (statementVariableNames, termVariableNames)
      }
    }.getOrElse((Nil, Nil))
  }

  def chapterTitlesParser: Parser[Seq[String]] = {
    Parser.optionalWord("chapter").flatMapMap(_ => Parser.toEndOfLine).whileDefined
  }

  implicit class BookSeqOps(books: Seq[Book]) {
    def transitive: Seq[Book] = {
      (books.flatMap(_.dependencies.transitive) ++ books).distinctBy(_.title)
    }
    def inferences = books.flatMap(_.chapters).flatMap(_.inferences)
    def statementDefinitions = books.flatMap(_.chapters).flatMap(_.statementDefinitions)
    def termDefinitions = books.flatMap(_.chapters).flatMap(_.termDefinitions)
  }
}
