package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.definitions.BinaryJoiner
import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.{Premise, Step, StepProvingContext}
import net.prover.util.Swapper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepTransitivityController @Autowired() (val bookService: BookService) extends BookModification with ChainingStepEditing {

  def suggestTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    swapper: Swapper
  ): ResponseEntity[_] = {
    def getPremises[T <: Expression](joiner: BinaryJoiner[T], lhs: T, rhs: T)(implicit stepProvingContext: StepProvingContext): Try[Seq[Premise.SingleLinePremise]] = {
      Success(stepProvingContext.allPremises.mapCollect { p =>
        for {
          (premiseLhs, premiseRhs) <- joiner.unapply(p.statement)
          if swapper.getSource(lhs, rhs) == swapper.getSource(premiseLhs, premiseRhs)
        } yield p
      })
    }
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      result <- withRelation(step.statement, getPremises(_, _, _)(stepProvingContext), getPremises(_, _, _)(stepProvingContext))(stepProvingContext)
    } yield result).toResponseEntity
  }


  @GetMapping(value = Array("/suggestTransitivityFromPremiseLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestTransitivityFromPremiseLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    suggestTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, Swapper.DontSwap)
  }
  @GetMapping(value = Array("/suggestTransitivityFromPremiseRight"), produces = Array("application/json;charset=UTF-8"))
  def suggestTransitivityFromPremiseRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    suggestTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, Swapper.Swap)
  }

  private def addPremise(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    serializedPremiseStatement: String,
    swapper: Swapper
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingStepsCommon {
      override def createSteps[T <: Expression : ChainingMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
          premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
          (premiseLhs, premiseRhs) <- targetJoiner.unapply(premise.statement) orBadRequest "Premise was not transitive statement"
          (premiseSource, premiseResult) = swapper.swapSourceAndResult(premiseLhs, premiseRhs)
          (targetSource, targetResult) = swapper.swapSourceAndResult(targetLhs, targetRhs)
          _ <- (premiseSource == targetSource).orBadRequest("Premise did not match target")
          (targetLhs, targetRhs) = swapper.swapSourceAndResult(premiseResult, targetResult)
          premiseChainingDefinition = ChainingStepDefinition(premiseLhs, premiseRhs, targetJoiner, None)
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetJoiner)
          (firstStep, secondStep) = swapper.swapSourceAndResult(premiseChainingDefinition, targetStepDefinition)
        } yield (firstStep, secondStep, Nil)
      }
    })
  }

  @PostMapping(value = Array("/premiseLeft"), produces = Array("application/json;charset=UTF-8"))
  def addPremiseLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    addPremise(bookKey, chapterKey, theoremKey, proofIndex, stepPath, serializedPremiseStatement, Swapper.DontSwap)
  }
  @PostMapping(value = Array("/premiseRight"), produces = Array("application/json;charset=UTF-8"))
  def addPremiseRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    addPremise(bookKey, chapterKey, theoremKey, proofIndex, stepPath, serializedPremiseStatement, Swapper.Swap)
  }
}
