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
    @PathVariable("theoremKey") theoremKey: String
  ): ResponseEntity[_] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, _) =>
      Success(theorem.copy(proofs = theorem.proofs :+ Theorem.Proof(Seq(Step.Target(theorem.conclusion)))))
    }.toResponseEntity
  }
  @DeleteMapping(value = Array("/proofs/{proofIndex}"))
  def deleteProof(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int
  ): ResponseEntity[_] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, _) =>
      if (theorem.proofs.length == 1)
        Failure(BadRequestException("Cannot delete the only proof on a theorem"))
      else
        theorem.proofs.splitAtIndexIfValid(proofIndex)
          .map { case (before, _, after) => theorem.copy(proofs = before ++ after) }
          .orNotFound(s"Proof $proofIndex")
    }.toResponseEntity
  }
}
