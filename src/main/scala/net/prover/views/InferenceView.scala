package net.prover.views

import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.{Book, Chapter, Inference}
import net.prover.viewmodel.Breadcrumb

import scala.xml.Elem

object InferenceView {
  def apply(
    description: String,
    inference: Inference.Entry,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])])(
    content: Elem)(
  ) = OldMainTemplate(
    Breadcrumb.Root,
    Breadcrumb.Book(book),
    Breadcrumb.Chapter(chapter),
    Breadcrumb.ChapterEntry(inference))
  {
    import book.displayContext
    <div class="inference">
      <div class="navigationLinks">
        { previousOption.toSeq.map { previous => <a class="navigationLink float-left" href={previous.key.url}>&laquo; {previous.name}</a> }}
        { nextOption.toSeq.map { next => <a class="navigationLink float-right" href={next.key.url}>{next.name} &raquo;</a> }}
      </div>
      <div class="inferenceTitle">
        <h3>
          {description}: {inference.name}
        </h3>
        <div class="inferenceId">
          {inference.id}
        </div>
      </div>

      <p>
        {PremisesView(inference.premises)}
        <div>
          { if(inference.premises.nonEmpty) "Then" }
          {ExpressionView(inference.conclusion)}.
        </div>
      </p>

      {content}

      { if (usages.nonEmpty) {
        <div>
          <hr />
          { for((usageBook, usageChapter, theorems) <- usages) yield {
            <h6>{usageBook.title} - {usageChapter.title}</h6>
            <p>
              { theorems.map { theorem =>
                <span class="usage">
                  <a class="usageLink" href={theorem.key.url}>{theorem.name}</a>
                </span>
              }}
            </p>
          }}
        </div>
      }}
    </div>
  }
}
