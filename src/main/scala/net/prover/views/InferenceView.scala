package net.prover.views

import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.ReferenceMap
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
    referenceMap: ReferenceMap,
    usages: Seq[(Book, Chapter, Seq[Theorem])])(
    content: Elem)(
  ) = MainTemplate(
    Breadcrumb.Root,
    Breadcrumb.Book(book),
    Breadcrumb.Chapter(chapter),
    Breadcrumb.ChapterEntry(inference))
  {
    import book.displayContext
    <div class="inference">
      <div class="inferenceLinks">
        { previousOption.toSeq.map { previous => <a class="inferenceLink pull-left" href={previous.key.url}>&laquo; {previous.name}</a> }}
        { nextOption.toSeq.map { next => <a class="inferenceLink pull-right" href={next.key.url}>{next.name} &raquo;</a> }}
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
        {PremisesView(inference.premises, referenceMap)}
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
            <div><label>{usageBook.title} - {usageChapter.title}</label></div>
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
