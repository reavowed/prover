package net.prover.books.management

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.Book
import net.prover.entries.GlobalContext
import net.prover.model.definitions.Definitions
import scalaz.Functor
import scalaz.syntax.functor._

object UpdateBooks {
  def apply[F[_] : Functor](
    f: GlobalContext => F[Seq[Book]])(
    implicit bookStateManager: BookStateManager
  ): F[GlobalContext] = {
    bookStateManager.updateBooks(setBooks => {
      f(bookStateManager.globalContext).map(books => setBooks(ListWithKeys(books.toList)))
    })
  }
}
