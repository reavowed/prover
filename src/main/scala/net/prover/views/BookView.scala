package net.prover.views

import net.prover.model.Book
import net.prover.viewmodel.Breadcrumb

object BookView {
  def apply(book: Book) = OldMainTemplate(Breadcrumb.Root, Breadcrumb.Book(book)) {
    <div class="book">
      <h3>{book.title}</h3>
      { book.chapters.map { chapter =>
        <div class="chapter">
          <h4><a href={chapter.key.url}>{chapter.title}</a></h4>
          <p>{chapter.summary}</p>
        </div>
      }}
    </div>
  }

}
