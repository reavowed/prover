package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (val bookService: BookService) extends BookModification {

  @PostMapping(value = Array("/{stepPath}/clear"))
  def clearStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepPath)((step, _, _) =>
      Success(step.provenStatement.map(s => Step.Target(s)).toSeq)
    ).toResponseEntity
  }

  @DeleteMapping(value = Array("/{stepPath}"))
  def deleteStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepPath)((step, _, _) =>
      // Deleting naming steps is confusing, just clear them
      Success(step.asOptionalInstanceOf[Step.Naming]
        .flatMap(namingStep => namingStep.provenStatement)
        .map(s => Step.Target(s))
        .toSeq)
    ).toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/move"))
  def moveStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("direction") direction: String
  ): ResponseEntity[_] = {
    def moveInTheorem(theorem: Theorem): Option[Try[Theorem]] = {
      stepPath.indexes match {
        case Nil =>
          None
        case Seq(0) if direction == "up" =>
          Some(Failure(BadRequestException("Cannot move step upwards if it's the first step")))
        case init :+ containerIndex :+ 0 if direction == "up" =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(containerIndex).flatMap { outerStep =>
              outerStep.extractSubstep(0).map(_.orBadRequest(s"Could not extract step $stepPath from outer step"))
            }.mapMap { case (outerStep, innerStep) =>
              steps.take(containerIndex) ++ Seq(innerStep, outerStep) ++ steps.drop(containerIndex + 1)
            }
          })
        case init :+ last =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(last).map { step =>
              direction match {
                case "up" =>
                  Success((steps.take(last - 1) :+ step :+ steps(last - 1)) ++ steps.drop(last + 1))
                case "down" =>
                  Success((steps.take(last) ++ steps.lift(last + 1).toSeq :+ step) ++ steps.drop(last + 2))
                case _ =>
                  Failure(BadRequestException(s"Unrecognised direction $direction"))
              }
            }
          })
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, _) =>
      moveInTheorem(theorem).orNotFound(s"Step $stepPath").flatten
    }.toResponseEntity
  }


  @PostMapping(value = Array("/{stepPath}/moveIntoNext"))
  def moveStepIntoNext(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    def moveInTheorem(theorem: Theorem): Option[Try[Theorem]] = {
      stepPath.indexes match {
        case Nil =>
          None
        case init :+ last =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
                after match {
                  case (following: Step.WithSubsteps) +: remaining =>
                    val updatedStep = following.modifyStepForInsertion(step)
                    Success(before ++ Seq(following.replaceSubsteps(updatedStep +: following.substeps)) ++ remaining)
                  case _ =>
                    Failure(BadRequestException("No valid following step to insert into"))
                }

            }
          })
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, _) =>
      moveInTheorem(theorem).orNotFound(s"Step $stepPath").flatten
    }.toResponseEntity
  }
}
