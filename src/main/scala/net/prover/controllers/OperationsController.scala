package net.prover.controllers

import net.prover.books.management.BookStateManager
import net.prover.entries.EntryWithContext
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.refactoring.{ReplaceElidedSteps, ReplaceInference, UpdateEntries}
import net.prover.theorems.ClearInference
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RequestParam, RestController}

@RestController
@RequestMapping(Array("/"))
class OperationsController @Autowired() (implicit bookStateManager: BookStateManager) {

  @GetMapping(value = Array("replaceElidedSteps"))
  def replaceElidedSteps(): Unit = {
    ReplaceElidedSteps()
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
    UpdateEntries(globalContext => {
      val inference = globalContext.definitions.allInferences.find(_.id == inferenceId).get
      entryWithContext => {
        val updated = entryWithContext.entry.asOptionalInstanceOf[Theorem] match {
          case Some(theorem) =>
            ClearInference(theorem, inference)
          case _ =>
            entryWithContext.entry
        }
        updated
      }
    })
  }
}
