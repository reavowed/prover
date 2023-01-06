package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.TheoremWithContext
import net.prover.model.ProvingContext
import net.prover.model.definitions.Definitions
import net.prover.model.entries.Theorem

import scala.util.Try

object UpdateTheorems {
  def apply(
    getUpdateOperation: Definitions => TheoremWithContext => Theorem)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    UpdateEntries(definitions => {
      val updateOperation = getUpdateOperation(definitions)
      entryWithContext => {
        import entryWithContext._
        entry match {
          case theorem: Theorem =>
            updateOperation(TheoremWithContext(book, chapter, theorem, ProvingContext(entryContext, definitions)))
          case other =>
            other
        }
      }
    })
  }
}
