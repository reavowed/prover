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
    val link = "/books/" + book.key
  }
  case class Chapter(chapter: model.Chapter) extends Breadcrumb {
    val text = chapter.title
    val link = "/books/" + chapter.bookKey + "/" + chapter.key
  }
  case class Inference(inference: model.Inference.Entry) extends Breadcrumb {
    val text = inference.name
    val link = "/books/" + inference.bookKey + "/" + inference.chapterKey + "/" + inference.key
  }
}
