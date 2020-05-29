package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.VariableTracker
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepProvingController @Autowired() (val bookService: BookService) extends BookModification with StepCreation with InferenceSearch {
  def createStep(
    definition: StepDefinition,
    getConclusionOption: (ExpressionParsingContext, Substitutions) => Try[Option[Statement]],
    unwrappers: Seq[Unwrapper])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Step, Seq[Step.Target])] = {
    def withInference(inferenceId: String) = createAssertionStepForInference(inferenceId, getConclusionOption, definition, unwrappers)
    def withPremise(serializedPremiseStatement: String) = {
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        extraction <- SubstatementExtractor.getPremiseExtractions(premiseStatement).find(_.extractionInferences == extractionInferences).orBadRequest("Could not find extraction with given inferences")
        substitutions <- definition.substitutions.parse(extraction.variableDefinitions)
        epc = ExpressionParsingContext.withDefinitions(extraction.variableDefinitions)
        conclusionOption <- getConclusionOption(epc, substitutions)
        newTargetStatementsOption <- definition.parseIntendedPremiseStatements(epc)
        substitutedNewTargetStatementsOption <- newTargetStatementsOption.map(_.map(_.applySubstitutions(substitutions)).traverseOption.orBadRequest("Could not apply substitutions to intended new targets")).swap
        (result, stepOption, extractionTargets) <- ExtractionHelper.getPremiseExtractionWithPremises(premise, extractionInferences, substitutions, substitutedNewTargetStatementsOption, conclusionOption)
        step <- stepOption.orBadRequest("At least one step must be present")
      } yield (result, step, extractionTargets)
    }
    definition.getFromInferenceOrPremise(withInference, withPremise)
  }

  case class UnwrappedStatement(statement: Statement, unwrappers: Seq[Unwrapper]) {
    def definitionSymbols: Seq[String] = unwrappers.map(_.definitionSymbol)
  }

  def getPossibleTargets(statement: Statement)(implicit stepProvingContext: StepProvingContext): Seq[UnwrappedStatement] = {
    val provingContext = stepProvingContext.provingContext
    def helper(currentUnwrappedStatement: UnwrappedStatement, resultsSoFar: Seq[UnwrappedStatement], variableTracker: VariableTracker): Seq[UnwrappedStatement] = {

      def byGeneralization = for {
        generalizationDefinition <- provingContext.generalizationDefinitionOption
        (specificationInference, _) <- provingContext.specificationInferenceOption
        (variableName, predicate) <- generalizationDefinition.unapply(currentUnwrappedStatement.statement)
        (uniqueVariableName, _, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(variableName)
        newUnwrappedStatement = UnwrappedStatement(predicate, currentUnwrappedStatement.unwrappers :+ GeneralizationUnwrapper(uniqueVariableName, generalizationDefinition, specificationInference))
      } yield (newUnwrappedStatement, newVariableTracker)

      def byDeduction = for {
        deductionDefinition <- provingContext.deductionDefinitionOption
        (deductionEliminationInference, _, _) <- provingContext.deductionEliminationInferenceOption
        (antecedent, consequent) <- deductionDefinition.unapply(currentUnwrappedStatement.statement)
        newUnwrappedStatement = UnwrappedStatement(consequent, currentUnwrappedStatement.unwrappers :+ DeductionUnwrapper(antecedent, deductionDefinition, deductionEliminationInference))
      } yield (newUnwrappedStatement, variableTracker)

      (byGeneralization orElse byDeduction) match {
        case Some((newUnwrappedStatement, newVariableTracker)) =>
          helper(newUnwrappedStatement, resultsSoFar :+ newUnwrappedStatement, newVariableTracker)
        case None =>
          resultsSoFar
      }
    }
    val initialUnwrappedStatement = UnwrappedStatement(statement, Nil)
    helper(initialUnwrappedStatement, Seq(initialUnwrappedStatement), VariableTracker.fromStepContext)
  }

  @GetMapping(value = Array("/possibleInferencesForCurrentTarget"), produces = Array("application/json;charset=UTF-8"))
  def getPossibleInferencesForCurrentTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val spc = stepProvingContext

      val possibleUnwrappedTargetStatements = getPossibleTargets(step.statement)

      def findPossibleInference(inference: Inference): Option[PossibleInferenceWithTargets] = {
        val possibleTargets = for {
          possibleUnwrappedTargetStatement <- possibleUnwrappedTargetStatements
          possibleConclusions = spc.provingContext.inferenceExtractionsByInferenceId(inference.id)
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
        stepProvingContext.provingContext.entryContext.allInferences,
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
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      inference <- findInference(inferenceId)(stepProvingContext)
      possibleTarget <- getPossibleTargets(step.statement)(stepProvingContext).find(_.unwrappers.map(_.definitionSymbol) == targetUnwrappers.toSeq).orBadRequest(s"Could not find target with unwrappers ${targetUnwrappers.mkString(", ")}")
      inferenceExtraction <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(inference.id).find(_.extractionInferences.map(_.id) == conclusionExtractionInferenceIds.toSeq).orBadRequest(s"Could not find extraction option with inference ids ${conclusionExtractionInferenceIds.mkString(", ")}")
    } yield PossibleConclusionWithPremises.fromExtractionWithTarget(inferenceExtraction, possibleTarget.statement)(stepProvingContext.updateStepContext(possibleTarget.unwrappers.enhanceContext))).toResponseEntity
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
    (for {
      (_, stepProvingContext) <- bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val spc = stepProvingContext
      filterInferences(stepProvingContext.provingContext.entryContext.allInferences, searchText)
        .reverse
        .take(10)
        .map { inference =>
          val possibleConclusions = spc.provingContext.inferenceExtractionsByInferenceId(inference.id)
            .map(PossibleConclusionWithPremises.fromExtraction(_, None))
          PossibleInferenceWithConclusions(inference.summary, possibleConclusions)
        }
    }).toResponseEntity
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
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
      premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
      baseSubstitutions <- premise.statement.calculateSubstitutions(premise.statement)(stepProvingContext.stepContext).orBadRequest(s"Somehow failed to calculate base substitutions for premise '${premise.statement}'")
    } yield {
      implicit val spc = stepProvingContext
      SubstatementExtractor.getPremiseExtractions(premise.statement)
        .flatMap(PossibleConclusionWithPremises.fromExtractionWithSubstitutions(_, _.calculateSubstitutions(step.statement, baseSubstitutions)))
    }).toResponseEntity
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
    (for {
      (_, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
      premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
      baseSubstitutions <- premise.statement.calculateSubstitutions(premise.statement)(stepProvingContext.stepContext).orBadRequest(s"Somehow failed to calculate base substitutions for premise '$premiseStatement'")
    } yield {
      implicit val spc = stepProvingContext
      SubstatementExtractor.getPremiseExtractions(premise.statement)
        .map(PossibleConclusionWithPremises.fromExtraction(_, Some(baseSubstitutions))(stepProvingContext))
    }).toResponseEntity
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
    replaceStepAndAddBeforeTransitivity[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (targetStep, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        (targetStatement, unwrappers) <- getPossibleTargets(targetStep.statement).find(_.definitionSymbols == definition.wrappingSymbols).map(x => (x.statement, x.unwrappers)).orBadRequest("Invalid wrapping symbols")
        (result, newStep, targets) <- createStep(definition, (_, _) => Success(Some(targetStatement)), unwrappers)
        _ <- (result == targetStep.statement).orBadRequest("Conclusion was incorrect")
      } yield (newStep, targets)
    }.toResponseEntity
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
    addBeforeTransitivity[Step](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { stepProvingContext =>
      implicit val spc = stepProvingContext
      for {
        (_, newStep, targets) <- createStep(
          definition,
          definition.parseIntendedConclusion,
          Nil)
      } yield targets :+ newStep
    }.toResponseEntity
  }
}
