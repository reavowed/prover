package net.prover.controllers

import net.prover.controllers.models.{ExtractRequest, ExtractWithPremiseRequest, PathData}
import net.prover.model.ProvingContext
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, SubstatementExtractor}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.{GetMapping, PathVariable, PostMapping, RequestBody, RequestMapping, RequestParam, RestController}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepExtractionController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch {

  @GetMapping(value = Array("/suggestFacts"), produces = Array("application/json;charset=UTF-8"))
  def suggestFacts(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
    } yield {
      filterInferences(provingContext.facts, searchText).map(_.summary).reverse.take(10)
    }).toResponseEntity
  }

  @PostMapping(value = Array("/extract"), produces = Array("application/json;charset=UTF-8"))
  def extract(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody request: ExtractRequest
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      val extractor = new SubstatementExtractor()(stepProvingContext)
      def fromPremise(serializedPremiseStatement: String) = for {
        premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
        premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        result <- extractor.extractFromPremise(premise.statement, step.statement) orBadRequest s"Could not extract statement ${step.statement}"
      } yield result
      def fromFact(inferenceId: String) = for {
        fact <- stepProvingContext.provingContext.facts.find(_.id == inferenceId).orBadRequest(s"Could not find inference ${request.inferenceId}")
        result <- extractor.extractFromFact(fact, step.statement) orBadRequest s"Could not extract statement ${step.statement}"
      } yield result
      for {
        (newStep, target) <- (request.inferenceId.map(fromFact) orElse request.serializedPremiseStatement.map(fromPremise) orBadRequest "Either fact or premise must be provided").flatten
      } yield Seq(target, newStep)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/extractAutomatically"), produces = Array("application/json;charset=UTF-8"))
  def extractAutomatically(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStepAndAddBeforeTransitivity[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      val extractor = new SubstatementExtractor()(stepProvingContext)
      for {
        (newStep, target) <- extractor.extract(step.statement).map((_, None)) orBadRequest s"Could not extract statement ${step.statement}"
      } yield (newStep, target.toSeq)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/extractWithPremise"), produces = Array("application/json;charset=UTF-8"))
  def extractWithPremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody request: ExtractWithPremiseRequest
  ): ResponseEntity[_] = {
    replaceStepAndAddBeforeTransitivity[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        premiseStatement <- Statement.parser(stepProvingContext).parseFromString(request.serializedPremiseStatement, "premise statement").recoverWithBadRequest
        premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        fact <- stepProvingContext.provingContext.facts.find(_.id == request.inferenceId).orBadRequest(s"Could not find inference ${request.inferenceId}")
        newStep <- new SubstatementExtractor()(stepProvingContext).extractFromFactWithPremise(fact, premise.statement).orBadRequest(s"Could not extract statement ${step.statement}")
      } yield (step, Seq(newStep))
    }.toResponseEntity
  }
}
