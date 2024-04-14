package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepWrappingController @Autowired() (val bookService: BookService) {
  @PostMapping(value = Array("/introduceSubproof"))
  def introduceSubproof(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody name: String
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { stepWithContext =>
      Success(Seq(Step.SubproofStep(name, Seq(stepWithContext.step))))
    }.toResponseEntity
  }

  @PostMapping(value = Array("/elide"))
  def elide(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.modifyStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { stepWithContext =>
      Success(Step.ElidedStep(Seq(stepWithContext.step), None, None))
    }.toResponseEntity
  }
}
