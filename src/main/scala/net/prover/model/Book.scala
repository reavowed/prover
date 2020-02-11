package net.prover.model

import java.nio.file.Path

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import org.slf4j.{Logger, LoggerFactory}

@JsonIgnoreProperties(Array("dependencies"))
case class Book(
    title: String,
    imports: Seq[String],
    chapters: Seq[Chapter],
    termVariableNames: Seq[String])
{
  def serialized: String = {
    val sections = Seq(
      imports.map(i => s"import $i"),
      if (termVariableNames.nonEmpty) Seq(s"term-variables (${termVariableNames.mkString(" ")})") else Nil,
      chapters.map(c => s"chapter ${c.title}"))

    sections.filter(_.nonEmpty).map(_.mkString("\n")).mkString("\n\n") + "\n"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Book]

  override def equals(other: Any): Boolean = other match {
    case that: Book =>
      (that canEqual this) &&
        title == that.title
    case _ => false
  }

  override def hashCode(): Int = {
    title.hashCode
  }
}

object Book {
  val logger: Logger = LoggerFactory.getLogger(Book.getClass)

  def getDependencies(imports: Seq[String], availableBooks: Seq[Book]): Seq[Book] = {
    imports
      .map { importTitle =>
        availableBooks.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      .flatMap { importedBook =>
        getDependencies(importedBook.imports, availableBooks) :+ importedBook
      }
      .distinctBy(_.title)
  }

  def parse(title: String, path: Path, previousBooks: Seq[Book], getChapterPath: (String, Int) => Path): Book = {
    parser(title, previousBooks, getChapterPath).parseFromFile(path, s"book '$title'")
  }

  def parser(title: String, previousBooks: Seq[Book], getChapterPath: (String, Int) => Path): Parser[Book] = {
    for {
      imports <- importsParser
      _ <- variableDefinitionsParser
      termVariableNames <- termVariableNamesParser
      chapterTitles <- chapterTitlesParser
    } yield {
      val dependencies = getDependencies(imports, previousBooks)
      val entryContext = EntryContext.forBooks(dependencies, termVariableNames)
      val chapters = chapterTitles.zipWithIndex.mapFold(entryContext) { case (context, (chapterTitle, index)) =>
        val chapterPath = getChapterPath(chapterTitle, index)
        val (chapterOutline, newContext) = Chapter.parser(chapterTitle)(context)
          .parseFromFile(chapterPath, s"book '$title' chapter '$chapterTitle'")
        (newContext, chapterOutline)
      }._2
      Book(title, imports, chapters, termVariableNames)
    }
  }

  private def importsParser: Parser[Seq[String]] = {
    Parser.optional("import", Parser.toEndOfLine).whileDefined
  }

  def termVariableNamesParser: Parser[Seq[String]] = {
    Parser.optional("term-variables", Parser.allInParens.map(_.splitByWhitespace()), Nil)
  }

  def variableDefinitionsParser: Parser[(Seq[String], Seq[String])] = {
    Parser.optional(
      "variables",
      for {
        statementVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
        termVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
      } yield {
        (statementVariableNames, termVariableNames)
      },
      (Nil, Nil))
  }

  def chapterTitlesParser: Parser[Seq[String]] = {
    Parser.optional("chapter", Parser.toEndOfLine).whileDefined
  }
}
