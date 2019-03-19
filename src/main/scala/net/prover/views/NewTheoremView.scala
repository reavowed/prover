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
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ): Elem = MainTemplate(
    Breadcrumb.Root,
    Breadcrumb.Book(book),
    Breadcrumb.Chapter(chapter),
    Breadcrumb.ChapterEntry(theorem))
  {
    <div>
      <div id="theorem"></div>
      <script type="text/javascript">
        let theorem = {Unparsed(JsonMapping.toString(theorem))};
        theorem.proof = {Unparsed(JsonMapping.toString(theorem.proof))};
        let previousEntry = {Unparsed(JsonMapping.toString(previousOption))};
        let nextEntry = {Unparsed(JsonMapping.toString(nextOption))};
        let usages = {Unparsed(JsonMapping.toString(usages))};
      </script>
      <script src="https://unpkg.com/react@16/umd/react.development.js"></script>
      <script src="https://unpkg.com/react-dom@16/umd/react-dom.development.js"></script>
      <script src="https://unpkg.com/babel-standalone@6/babel.min.js"></script>
      <script src="https://unpkg.com/styled-components/dist/styled-components.min.js"></script>
      <script src="/js/newTheorem.jsx" type="text/babel"></script>
    </div>
  }
}
