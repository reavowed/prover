package net.prover.views

import net.prover.model.entries._
import net.prover.model._
import net.prover.model.proof.ReferenceMap
import net.prover.viewmodel.Breadcrumb

import scala.xml.Unparsed

object ChapterView {
  private def DefinitionTitle(description: String, definition: ExpressionDefinition)(implicit displayContext: DisplayContext) = {
    <div class="entryTitle">
      <h5>{description} Definition: {ExpressionView(definition.defaultValue)}</h5>
      <button class="btn btn-success btn-xs editShorthand"
              data-key={definition.key.value}
              data-shorthand={definition.shorthand.getOrElse("")}
              data-toggle="modal"
              data-target="#editShorthandModal"
      >Shorthand</button>
    </div>
  }

  private def StatementDefinitionView(statementDefinition: StatementDefinition)(implicit displayContext: DisplayContext) = {
    <div id={statementDefinition.key.value} class="result">
      {DefinitionTitle("Statement", statementDefinition)}
      {statementDefinition.definingStatement.toSeq.map { definingStatement =>
        <div>{ExpressionView(statementDefinition.defaultValue)} is defined by {ExpressionView(definingStatement)}. </div>
      }}
    </div>
  }
  private def TermDefinitionView(termDefinition: TermDefinition)(implicit displayContext: DisplayContext) = {
    <div id={termDefinition.key.value} class="result">
      {DefinitionTitle("Term", termDefinition)}
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
    <div id={inference.key.value} class="result">
      <div class="entryTitle">
        <h5>
          <a href={inference.key.url} class="inferenceEntryTitle">
            {description}: {inference.name}
          </a>
        </h5>
        <button class="btn btn-danger btn-xs deleteInference"
                data-key={inference.key.value}
        ><span class="glyphicon glyphicon-remove" aria-hidden="true"></span></button>
      </div>
      <div class="resultBlock">
        { PremisesView(inference.premises, ReferenceMap.empty) }
        <div>
          { if(inference.premises.nonEmpty) "Then" }
          {ExpressionView(inference.conclusion)}.
        </div>
      </div>
    </div>
  }

  def apply(
    chapter: Chapter,
    book: Book,
    previousChapterOption: Option[Chapter],
    nextChapterOption: Option[Chapter]
   ) = MainTemplate(Breadcrumb.Root, Breadcrumb.Book(book), Breadcrumb.Chapter(chapter)) {
    import book.displayContext
    <div class="chapter">
      <div class="navigationLinks">
        { previousChapterOption.toSeq.map { previous => <a class="navigationLink pull-left" href={previous.key.url}>&laquo; {previous.title}</a> }}
        { nextChapterOption.toSeq.map { next => <a class="navigationLink pull-right" href={next.key.url}>{next.title} &raquo;</a> }}
      </div>
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
      <div class="btn-group">
        <button type="button" class="btn btn-success" data-toggle="modal" data-target="#addTheoremModal">
          Add Theorem
        </button>
      </div>
      <div class="navigationLinks">
        { previousChapterOption.toSeq.map { previous => <a class="navigationLink pull-left" href={previous.key.url}>&laquo; {previous.title}</a> }}
        { nextChapterOption.toSeq.map { next => <a class="navigationLink pull-right" href={next.key.url}>{next.title} &raquo;</a> }}
      </div>
      <div class="modal" tabindex="-1" role="dialog" id="editShorthandModal">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
              <h4 class="modal-title">Edit shorthand for <span id="entryDescriptionForShorthand"></span></h4>
            </div>
            <div class="modal-body">
              <div id="editShorthandModalAlert" class="alert alert-danger alert-dismissible" role="alert" style="display: none;">
                <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
                <span id="editShorthandModalAlertContent"></span>
              </div>
              <div class="form-group">
                <label for="shorthandInput">Shorthand</label>
                <input type="text" class="form-control" id="shorthandInput" />
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
              <button type="button" class="btn btn-primary" id="saveShorthandButton">Save</button>
            </div>
          </div>
        </div>
      </div>
      <div class="modal" tabindex="-1" role="dialog" id="addTheoremModal">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
              <h4 class="modal-title">Add theorem</h4>
            </div>
            <div class="modal-body">
              <div class="form-group">
                <label for="theoremName">Name</label>
                <input type="text" class="form-control" id="theoremName" />
              </div>
              <div class="form-group">
                <label for="premises">Premises</label>
                <textarea id="theoremPremises" class="form-control replaceShorthands"></textarea>
              </div>
              <div class="form-group">
                <label for="theoremConclusion">Conclusion</label>
                <input type="text" class="form-control replaceShorthands" id="theoremConclusion" />
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
              <button type="button" class="btn btn-primary" id="addTheoremButton">Save changes</button>
            </div>
          </div>
        </div>
      </div>
      <script src="/js/chapter.js"></script>
    </div>
  }
}
