package net.prover.books.management

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.{Book, BookDefinition}
import net.prover.controllers.AnyWithResponseExceptionOps
import net.prover.entries.GlobalContext
import net.prover.util.FunctorTypes._

import scala.util.Try

object CreateBook {
  def apply(bookDefinition: BookDefinition)(implicit bookStateManager: BookStateManager): Try[GlobalContext] = {
    UpdateBooks[Try](globalContext => {
      def validateImport(title: String): Unit = {
        if (!globalContext.allBooks.exists(_.title == title)) {
          throw new Exception(s"Could not find import $title")
        }
      }
      for {
       _ <- bookDefinition.imports.foreach(validateImport).recoverWithBadRequest
      } yield globalContext.allBooks :+ Book(bookDefinition.title, bookDefinition.imports, ListWithKeys.empty)
    })
  }
}
