package net.prover.viewmodel

import net.prover.model

sealed trait Breadcrumb {
  def text: String
  def link: String
}

object Breadcrumb {
  case object Root extends Breadcrumb {
    val text = "Books"
    val link = "/books"
  }
  case class Book(book: model.Book) extends Breadcrumb {
    val text = book.title
    val link = book.key.url
  }
  case class Chapter(chapter: model.Chapter) extends Breadcrumb {
    val text = chapter.title
    val link = chapter.key.url
  }
  case class ChapterEntry(entry: model.entries.ChapterEntry) extends Breadcrumb {
    val text = entry.name
    val link = entry.key.url
  }
}
