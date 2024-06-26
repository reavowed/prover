package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model.ExpressionParsingContext
import net.prover.model.entries.Theorem
import net.prover.model.proof.Step
import net.prover.util.FunctorTypes._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._
import scalaz.Id.Id

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (val bookService: BookService) extends ParameterValidation {

  @PutMapping(value = Array("/variables"))
  def updateVariables(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @RequestBody serializedNewVariables: String
  ): ResponseEntity[_] = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      import theoremWithContext._
      for {
        newVariables <- getVariableDefinitions(serializedNewVariables)
        _ <- (newVariables.statements.length == theorem.variableDefinitions.statements.length).orBadRequest("Cannot change number of statement variables")
        _ <- (newVariables.terms.length == theorem.variableDefinitions.terms.length).orBadRequest("Cannot change number of term variables")
      } yield theorem.copy(variableDefinitions = newVariables)
    }.toResponseEntity
  }
  @PutMapping(value = Array("/premises"))
  def updatePremises(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @RequestBody serializedNewPremises: Seq[String]
  ): ResponseEntity[_] = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      import theoremWithContext._
      for {
        newPremises <- getPremises(serializedNewPremises)
      } yield theorem.copy(premises = newPremises)
    }.toResponseEntity
  }
  @PutMapping(value = Array("/conclusion"))
  def updateConclusion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @RequestBody serializedNewConclusion: String
  ): ResponseEntity[_] = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      import theoremWithContext._
      for {
        newConclusion <- getStatement(serializedNewConclusion, "conclusion")
      } yield theorem.copy(conclusion = newConclusion)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/proofs"))
  def createProof(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @RequestBody(required = false) proofIndexToCopy: java.lang.Integer
  ): ResponseEntity[_] = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      import theoremWithContext._
      for {
        newProof <- Option(proofIndexToCopy) match {
          case Some(proofIndex) =>
            theorem.proofs.lift(proofIndex).orBadRequest(s"Invalid proof index $proofIndex")
          case None =>
            Success(Theorem.Proof(Seq(Step.TargetStep(theorem.conclusion))))
        }
      } yield theorem.copy(proofs = theorem.proofs :+ newProof)
    }.toResponseEntity
  }

  @DeleteMapping(value = Array("/proofs/{proofIndex}"))
  def deleteProof(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int
  ): ResponseEntity[_] = {
    bookService.modifyTheorem[Id](bookKey, chapterKey, theoremKey) { theoremWithContext =>
      import theoremWithContext._
      if (theorem.proofs.length == 1)
        Failure(BadRequestException("Cannot delete the only proof on a theorem"))
      else
        theorem.proofs.splitAtIndexIfValid(proofIndex)
          .map { case (before, _, after) => theorem.copy(proofs = before ++ after) }
          .orNotFound(s"Proof $proofIndex")
    }.toResponseEntity
  }
}
