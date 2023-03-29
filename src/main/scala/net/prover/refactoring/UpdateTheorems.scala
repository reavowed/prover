package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.{GlobalContext, TheoremWithContext}
import net.prover.model.entries.Theorem

object UpdateTheorems {
  def apply(
    getUpdateOperation: GlobalContext => TheoremWithContext => Theorem)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    UpdateEntries(globalContext => {
      val updateOperation = getUpdateOperation(globalContext)
      entryWithContext => {
        import entryWithContext._
        entry match {
          case _: Theorem =>
            updateOperation(entryWithContext.asInstanceOf[TheoremWithContext])
          case other =>
            other
        }
      }
    })
  }
}
