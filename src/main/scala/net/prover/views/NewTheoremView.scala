package net.prover.views

import net.prover.JsonMapping
import net.prover.model.{Book, Chapter}
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.viewmodel.Breadcrumb

import scala.xml.{Elem, Unparsed}

object NewTheoremView {
  def apply(
    theorem: Theorem,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])],
    content: String
  ): Elem = MainTemplate(
    Breadcrumb.Root,
    Breadcrumb.Book(book),
    Breadcrumb.Chapter(chapter),
    Breadcrumb.ChapterEntry(theorem))
  {
    <div>
      <div id="theorem">{Unparsed(content)}</div>
      <script src="/js/bundle.js"></script>
      <script type="text/javascript">
        let theorem = {Unparsed(JsonMapping.toString(theorem))};
        let previousEntry = {Unparsed(JsonMapping.toString(previousOption))};
        let nextEntry = {Unparsed(JsonMapping.toString(nextOption))};
        let usages = {Unparsed(JsonMapping.toString(usages))};
        renderTheoremClient(theorem, previousEntry, nextEntry, usages);
      </script>
    </div>
  }
}
