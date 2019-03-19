package net.prover.views

import net.prover.JsonMapping
import net.prover.model.{Book, Chapter}
import net.prover.model.entries.{ChapterEntry, Theorem}

import scala.xml.{Elem, Unparsed}

object NewTheoremView {
  def apply(
    theorem: Theorem,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ): Elem = InferenceView("Theorem", theorem, chapter, book, previousOption, nextOption, usages) {
    <div>
      <div class="theoremProof">
        <hr/>
        <h4>Proof</h4>
        <div id="proof"></div>
      </div>
      <script type="text/javascript">
        let proof = {Unparsed(JsonMapping.toString(theorem.proof))};
      </script>
      <script src="https://unpkg.com/react@16/umd/react.development.js"></script>
      <script src="https://unpkg.com/react-dom@16/umd/react-dom.development.js"></script>
      <script src="https://unpkg.com/babel-standalone@6/babel.min.js"></script>
      <script src="https://unpkg.com/styled-components/dist/styled-components.min.js"></script>
      <script src="/js/newTheorem.jsx" type="text/babel"></script>
    </div>
  }
}
