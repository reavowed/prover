package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.{GetMapping, PathVariable, PostMapping, RequestBody, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepTransitivityController @Autowired() (val bookService: BookService) extends BookModification with TransitivityEditing {
  @GetMapping(value = Array("/suggestTransitivityFromPremiseLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestTransitivityFromPremiseLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      (relation, lhs, _) <- getRelation(step.statement)(stepProvingContext)
    } yield {
      implicit val spc = stepProvingContext
      stepProvingContext.allPremisesSimplestFirst.mapCollect { p =>
        for {
          (premiseLhs, _) <- relation.unapply(p.statement)
          if premiseLhs == lhs
        } yield p
      }
    }).toResponseEntity
  }
  @GetMapping(value = Array("/suggestTransitivityFromPremiseRight"), produces = Array("application/json;charset=UTF-8"))
  def suggestTransitivityFromPremiseRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      (relation, _, rhs) <- getRelation(step.statement)(stepProvingContext)
    } yield {
      implicit val spc = stepProvingContext
      stepProvingContext.allPremisesSimplestFirst.mapCollect { p =>
        for {
          (_, premiseRhs) <- relation.unapply(p.statement)
          if premiseRhs == rhs
        } yield p
      }
    }).toResponseEntity
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, relation, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        (premiseLhs, premiseRhs) <- relation.unapply(premise.statement) orBadRequest "Premise was not transitive statement"
        _ <- (premiseLhs == targetLhs).orBadRequest("Premise LHS did not match target LHS")
      } yield (relation, None, relation, Some(Step.Target(relation(premiseRhs, targetRhs))), premiseRhs, Nil)
    }
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, relation, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        (premiseLhs, premiseRhs) <- relation.unapply(premise.statement) orBadRequest "Premise was not transitive statement"
        _ <- (premiseRhs == targetRhs).orBadRequest("Premise LHS did not match target LHS")
      } yield (relation, Some(Step.Target(relation(targetLhs, premiseLhs))), relation, None, premiseLhs, Nil)
    }
  }
}
