package net.prover.views

import net.prover.model.Book

object BooksView {
  def apply(books: Seq[Book]) = MainTemplate() {
    <div>
      { books.map { book =>
        <div>
          <h3>
            <a href={book.key.url}>{book.title}</a>
          </h3>
        </div>
      }}
    </div>
  }
}
