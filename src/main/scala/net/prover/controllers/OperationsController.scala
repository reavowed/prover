package net.prover.controllers

import net.prover.books.management.BookStateManager
import net.prover.entries.EntryWithContext
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.refactoring.{ReplaceInference, UpdateEntries}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RequestParam, RestController}

@RestController
@RequestMapping(Array("/"))
class OperationsController @Autowired() (implicit bookStateManager: BookStateManager) {

  @GetMapping(value = Array("clearInferencesUsingOldFunction"))
  def clearInferencesUsingOldFunction(): Unit = {
    UpdateEntries[Seq[Inference]](Nil, definitions => {
      val oldFunctionDefinition = definitions.rootEntryContext.typeDefinitions("oldFunction")
      (entryWithContext, inferencesToClear) => {
        import entryWithContext._
        val referencedEntries = entry match {
          case theorem: Theorem =>
            (theorem.premises :+ theorem.conclusion).flatMap(_.referencedDefinitions).map(_.associatedChapterEntry).toSet
          case other =>
            other.referencedEntries
        }
        val updatedInferences = if (referencedEntries.contains(oldFunctionDefinition)) inferencesToClear ++ entry.inferences else inferencesToClear
        val updatedEntry = entry.asOptionalInstanceOf[Theorem] match {
          case Some(theorem) =>
            inferencesToClear.foldLeft(theorem)(_.clearInference(_))
          case _ =>
            entry
        }
        (updatedEntry, updatedInferences)
      }
    })
  }

  @GetMapping(value = Array("replaceInference"))
  def replaceInference(
    @RequestParam("old") oldInferenceId: String,
    @RequestParam("new") newInferenceId: String
  ): Unit = {
    ReplaceInference(oldInferenceId, newInferenceId)
  }

  @GetMapping(value = Array("clearInference"))
  def clearInference(
    @RequestParam("id") inferenceId: String
  ): Unit = {
    UpdateEntries(definitions => {
      val inference = definitions.allInferences.find(_.id == inferenceId).get
      entryWithContext => {
        val updated = entryWithContext.entry.asOptionalInstanceOf[Theorem] match {
          case Some(theorem) =>
            theorem.clearInference(inference)
          case _ =>
            entryWithContext.entry
        }
        updated
      }
    })
  }
}
