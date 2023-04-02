package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model._
import net.prover.model.proof._
import net.prover.model.unwrapping.{GeneralizationUnwrapper, UnwrappedStatement}
import net.prover.proving.fromExistingStatement.{SuggestExistingStatementsForCurrentTarget, SuggestExistingStatementsForNewTarget}
import net.prover.proving.{FindInference, ProveCurrentTarget, ProveNewTarget}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepProvingController @Autowired() (implicit val bookService: BookService) extends InferenceSearch {
  @GetMapping(value = Array("/possibleInferencesForCurrentTarget"), produces = Array("application/json;charset=UTF-8"))
  def getPossibleInferencesForCurrentTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath).map(implicit stepWithContext => {
      val possibleUnwrappedTargetStatements = UnwrappedStatement.getUnwrappedStatements(stepWithContext.step.statement)

      def findPossibleInference(inference: Inference): Option[PossibleInferenceWithTargets] = {
        val possibleTargets = for {
          possibleUnwrappedTargetStatement <- possibleUnwrappedTargetStatements
          possibleConclusions = stepWithContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
            .filter(_.conclusion.calculateSubstitutions(possibleUnwrappedTargetStatement.statement).nonEmpty)
            .map(e => PossibleConclusionWithoutPremises(e.conclusion, e.extractionInferences.map(_.id), e.additionalVariableNames))
          if possibleConclusions.nonEmpty
        } yield PossibleTarget(
          possibleUnwrappedTargetStatement.statement,
          possibleUnwrappedTargetStatement.definitionSymbols,
          possibleUnwrappedTargetStatement.unwrappers.ofType[GeneralizationUnwrapper].map(_.variableName).map(Seq(_)),
          possibleConclusions)

        if (possibleTargets.nonEmpty)
          Some(PossibleInferenceWithTargets(inference.summary, possibleTargets))
        else
          None
      }

      def getConclusionComplexity(possibleConclusion: PossibleConclusion): Int = possibleConclusion.conclusion.structuralComplexity

      getPossibleInferences(
        stepWithContext.availableEntries.allInferences,
        searchText,
        findPossibleInference,
        getConclusionComplexity)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/possiblePremisesForCurrentTarget"), produces = Array("application/json;charset=UTF-8"))
  def getPossiblePremisesForCurrentTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String,
    @RequestParam("targetUnwrappers") targetUnwrappers: Array[String],
    @RequestParam("conclusionExtractionInferenceIds") conclusionExtractionInferenceIds: Array[String]
  ): ResponseEntity[_] = {
    bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath).flatMap(implicit stepWithContext =>
      for {
        inference <- FindInference(inferenceId)
        possibleTarget <- UnwrappedStatement.getUnwrappedStatements(stepWithContext.step.statement).find(_.unwrappers.map(_.definitionSymbol) == targetUnwrappers.toSeq).orBadRequest(s"Could not find target with unwrappers ${targetUnwrappers.mkString(", ")}")
        inferenceExtraction <- stepWithContext.provingContext.inferenceExtractionsByInferenceId(inference.id).find(_.extractionInferences.map(_.id) == conclusionExtractionInferenceIds.toSeq).orBadRequest(s"Could not find extraction option with inference ids ${conclusionExtractionInferenceIds.mkString(", ")}")
      } yield PossibleConclusionWithPremises.fromExtractionWithTarget(inferenceExtraction, possibleTarget.statement)(possibleTarget.unwrappers.enhanceStepProvingContext)
    ).toResponseEntity
  }

  @GetMapping(value = Array("/possibleInferencesForNewTarget"), produces = Array("application/json;charset=UTF-8"))
  def getPossibleInferencesForNewTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath).map(implicit stepWithContext =>
      filterInferences(stepWithContext.provingContext.availableEntries.allInferences, searchText)
        .reverse
        .take(10)
        .map { inference =>
          val possibleConclusions = stepWithContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
            .map(PossibleConclusionWithPremises.fromExtraction(_, None))
          PossibleInferenceWithConclusions(inference.summary, possibleConclusions)
        }
    ).toResponseEntity
  }

  @GetMapping(value = Array("/possibleConclusionsForCurrentTargetByPremise"), produces = Array("application/json;charset=UTF-8"))
  def getPossibleConclusionsForCurrentTargetByPremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("serializedPremiseStatement") serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    SuggestExistingStatementsForCurrentTarget(bookKey, chapterKey, theoremKey, proofIndex, stepPath, serializedPremiseStatement).toResponseEntity
  }

  @GetMapping(value = Array("/possibleConclusionsForNewTargetByPremise"), produces = Array("application/json;charset=UTF-8"))
  def getPossibleConclusionsForNewTargetByPremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("serializedPremiseStatement") serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    SuggestExistingStatementsForNewTarget(bookKey, chapterKey, theoremKey, proofIndex, stepPath, serializedPremiseStatement).toResponseEntity
  }

  @PutMapping
  def proveCurrentTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    ProveCurrentTarget(bookKey, chapterKey, theoremKey, proofIndex, stepReference, definition).toResponseEntity
  }

  @PostMapping(value = Array("/newTarget"))
  def addNewTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    ProveNewTarget(bookKey, chapterKey, theoremKey, proofIndex, stepReference, definition).toResponseEntity
  }
}
