package net.prover.views

import net.prover.JsonMapping
import net.prover.model.{Book, Chapter, ParsingContext}
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
    parsingContext: ParsingContext
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
        window.definitions = {Unparsed(JsonMapping.toString(
          (parsingContext.statementDefinitions ++ parsingContext.termDefinitions).filter(_.componentTypes.nonEmpty).map(d => d.symbol -> d).toMap
        ))}
        window.shorthands = {Unparsed(JsonMapping.toString(book.displayContext.displayShorthands))}
      </script>
      <script src="http://localhost:8081/js/bundle.js"></script>
    </div>
  }
}
