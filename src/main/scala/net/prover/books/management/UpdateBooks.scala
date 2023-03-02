package net.prover.books.management

import net.prover.books.model.Book
import net.prover.model.definitions.Definitions
import scalaz.Functor
import scalaz.syntax.functor._

object UpdateBooks {
  def apply[F[_] : Functor](
    f: (Seq[Book], Definitions) => F[Seq[Book]])(
    implicit bookStateManager: BookStateManager
  ): F[(Seq[Book], Definitions)] = {
    bookStateManager.updateBooks(setBooks => {
      val result = f.tupled(bookStateManager.booksAndDefinitions)
      result.map { books =>
        setBooks(books)
      }
    })
  }
}
