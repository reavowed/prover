package net.prover.controllers

import net.prover.books.management.BookStateManager
import net.prover.refactoring.{RederivePremises, ReplaceElidedSteps, ReplaceInference, Reprove}
import net.prover.theorems.{ClearInference, RecalculateReferences}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, PathVariable, RequestMapping, RequestParam, RestController}
import scalaz.Id.Id

import scala.util.Try

@RestController
@RequestMapping(Array("/"))
class OperationsController @Autowired() (implicit bookService: BookService, bookStateManager: BookStateManager) {
  @GetMapping(value = Array("recalculateReferences"))
  def recalculateReferences(): Unit = {
    RecalculateReferences()
  }

  @GetMapping(value = Array("replaceElidedSteps"))
  def replaceElidedSteps(): Unit = {
    ReplaceElidedSteps()
  }

  @GetMapping(Array("/replaceElidedSteps/{bookKey}/{chapterKey}/{theoremKey}"))
  def replaceElidedSteps(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String
  ): Unit = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      Try {
        ReplaceElidedSteps(theoremWithContext)
      }
    }
  }

  @GetMapping(Array("/reprove"))
  def reprove(): Unit = {
    Reprove()
  }

  @GetMapping(Array("/reprove/{bookKey}/{chapterKey}/{theoremKey}"))
  def reprove(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String
  ): Unit = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      Try {
        Reprove(theoremWithContext)
      }
    }
  }

  @GetMapping(value = Array("rederivePremises"))
  def rederivePremises(
    @RequestParam(value = "inference", required = false) inferenceId: String
  ): Unit = {
    (Option(inferenceId) match {
      case Some(inferenceId) =>
        for {
          theorem <- bookStateManager.globalContext.allTheorems.find(_.theorem.id == inferenceId)
            .orNotFound(s"Theorem with id ${inferenceId}")
          _ <- bookService.modifyTheorem[Id](
            theorem.chapterWithContext.bookWithContext.bookKey,
            theorem.chapterWithContext.chapterKey,
            theorem.entryKey)(RederivePremises(_))
        } yield "Updated"
      case None =>
        Try { RederivePremises() }
    }).toResponseEntity
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
    ClearInference(inferenceId)
  }
}
