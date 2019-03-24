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
      <script src="http://localhost:8081/js/bundle.js"></script>
      <script type="text/javascript">{Unparsed(s"""
            |window.definitions = ${JsonMapping.toString((parsingContext.statementDefinitions ++ parsingContext.termDefinitions).filter(_.componentTypes.nonEmpty).map(d => d.symbol -> d).toMap)};
            |window.shorthands = ${JsonMapping.toString(book.displayContext.displayShorthands)};
            |App.renderTheorem(
            |  {
            |    theorem: App.Parser.parseTheorem(${JsonMapping.toString(theorem)}),
            |    previousEntry: ${JsonMapping.toString(previousOption)},
            |    nextEntry: ${JsonMapping.toString(nextOption)},
            |    usages: ${JsonMapping.toString(usages)}
            |  },
            |  document.getElementById("theorem")
            |);
          """.stripMargin
        )}
      </script>
    </div>
  }
}
