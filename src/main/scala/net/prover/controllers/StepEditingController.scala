package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepEditingController @Autowired() (val bookService: BookService) extends BookModification {

  @PostMapping(value = Array("/highlightedInference"))
  def setHighlightedInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody inferenceId: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Elided](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        inference <- findInference(inferenceId)(stepProvingContext)
      } yield step.copy(highlightedInference = Some(inference))
    }.toResponseEntity
  }
  @PostMapping(value = Array("/description"))
  def setDescription(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody description: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Elided](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, _) =>
      Success(step.copy(description = Some(description)))
    }.toResponseEntity
  }

  @PostMapping(value = Array("/createTargets"))
  def createTargets(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Assertion](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, _) =>
      val targetStatements = step.pendingPremises.values.map(_.statement).toSeq
      Success(targetStatements.map(Step.Target(_)) :+ step)
    }.toResponseEntity
  }

  @PutMapping(value = Array("/boundVariable"))
  def renameBoundVariableForStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody boundVariableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.WithVariable](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, _) =>
      Success(step.replaceVariableName(boundVariableName))
    }.toResponseEntity
  }

  @PutMapping(value = Array(
    "/boundVariables/{boundVariableIndex}",
    "/boundVariables/{statementPath}/{boundVariableIndex}"))
  def renameBoundVariableInStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable(value = "statementPath", required = false) statementPath: PathData,
    @PathVariable("boundVariableIndex") boundVariableIndex: Int,
    @RequestBody boundVariableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.WithTopLevelStatement](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step,_ ) =>
      step.updateStatement(s => s.renameBoundVariable(boundVariableName, boundVariableIndex, Option(statementPath).map(_.indexes).getOrElse(Nil)).orNotFound(s"Bound variable $boundVariableIndex at $statementPath"))
    }.toResponseEntity
  }
}
