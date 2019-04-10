package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/{stepPath}"))
class StepWrappingController @Autowired() (val bookService: BookService) extends BookModification {
  @PostMapping(value = Array("/introduceSubproof"))
  def introduceSubproof(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody name: String
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepPath) { (step, _, _, _) =>
      Success(Seq(Step.SubProof(name, Seq(step))))
    }.toResponseEntity
  }

  @PostMapping(value = Array("/elide"))
  def elide(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step](bookKey, chapterKey, theoremKey, stepPath) { (step, _, _) =>
      Success(Step.Elided(Seq(step), None))
    }.toResponseEntity
  }
}