package net.prover.views

import net.prover.model.entries._
import net.prover.model._
import net.prover.model.proof.ReferenceMap
import net.prover.viewmodel.Breadcrumb

object ChapterView {
  private def StatementDefinitionView(statementDefinition: StatementDefinition)(implicit displayContext: DisplayContext) = {
    <div class="result">
      <h5>Statement Definition: {ExpressionView(statementDefinition.defaultValue)}</h5>
      {statementDefinition.definingStatement.toSeq.map { definingStatement =>
        <div>{ExpressionView(statementDefinition.defaultValue)} is defined by {ExpressionView(definingStatement)}. </div>
      }}
    </div>
  }
  private def TermDefinitionView(termDefinition: TermDefinition)(implicit displayContext: DisplayContext) = {
    <div class="result">
      <h5>Term Definition: {ExpressionView(termDefinition.defaultValue)}</h5>
      <div>
        {PremisesView(termDefinition.premises)}
        <div>
          { if(termDefinition.premises.nonEmpty) "Then" }
          {ExpressionView(termDefinition.defaultValue)} is defined by {ExpressionView(termDefinition.definingStatement)}.
        </div>
      </div>
    </div>
  }
  private def InferenceView(description: String, inference: Inference.Entry)(implicit displayContext: DisplayContext) = {
    <div class="result">
      <h5>
        <a href={inference.key.url} class="inferenceEntryTitle">
          {description}: {inference.name}
        </a>
      </h5>
      <div class="resultBlock">
        { PremisesView(inference.premises, ReferenceMap.empty) }
        <div>
          { if(inference.premises.nonEmpty) "Then" }
          {ExpressionView(inference.conclusion)}.
        </div>
      </div>
    </div>
  }

  def apply(chapter: Chapter, book: Book) = MainTemplate(Breadcrumb.Root, Breadcrumb.Book(book), Breadcrumb.Chapter(chapter)) {
    import book.displayContext
    <div class="chapter">
      <h3>{chapter.title}</h3>
      <p class="comment">{chapter.summary}</p>
      {chapter.entries.map {
        case comment: Comment => {
          <p>{comment.text}</p>
        }
        case statementDefinition: StatementDefinition => StatementDefinitionView(statementDefinition)
        case termDefinition: TermDefinition => TermDefinitionView(termDefinition)
        case axiom: Axiom => InferenceView("Axiom", axiom)
        case theorem: Theorem => InferenceView("Theorem", theorem)
        case _ => {}
      }}
    </div>
  }
}
