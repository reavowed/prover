package net.prover.books.reading

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.BookOutline
import net.prover.parsing.{KnownWordParser, Parser}

object ReadBookOutline {
  def apply(title: String): BookOutline = {
      val path = BookDirectoryConfig.getBookFilePath(title)
      val parser = for {
        imports <- KnownWordParser("import")(Parser.toEndOfLine).whileDefined()
        chapterTitles <- KnownWordParser("chapter")(Parser.toEndOfLine).whileDefined()
      } yield BookOutline(title, imports, chapterTitles)

      parser.parseFromFile(path, s"book '$title'")
  }
}
