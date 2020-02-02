package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model.entries.Theorem
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (val bookService: BookService) extends BookModification {

  @PostMapping(value = Array("/proofs"))
  def createProof(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @RequestBody(required = false) proofIndexToCopy: java.lang.Integer
  ): ResponseEntity[_] = {
    bookService.modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, _) =>
      for {
        newProof <- Option(proofIndexToCopy) match {
          case Some(proofIndex) =>
            theorem.proofs.lift(proofIndex).orBadRequest(s"Invalid proof index $proofIndex")
          case None =>
            Success(Theorem.Proof(Seq(Step.Target(theorem.conclusion))))
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
    bookService.modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, _) =>
      if (theorem.proofs.length == 1)
        Failure(BadRequestException("Cannot delete the only proof on a theorem"))
      else
        theorem.proofs.splitAtIndexIfValid(proofIndex)
          .map { case (before, _, after) => theorem.copy(proofs = before ++ after) }
          .orNotFound(s"Proof $proofIndex")
    }.toResponseEntity
  }
}
