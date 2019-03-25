package net.prover.views

import net.prover.model.Book

object BooksView {
  def apply(books: Seq[Book]) = OldMainTemplate() {
    <div>
      { books.map { book =>
        <h3 class="bookLink"><a href={book.key.url}>{book.title}</a></h3>
      }}
    </div>
  }
}
