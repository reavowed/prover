package net.prover.controllers

import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models._
import net.prover.model.ExpressionParsingContext.TermVariableValidator
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.SubstatementExtractor.VariableTracker
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.collection.{SortedSet, TraversableLike}
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
        substitutions <- definition.substitutions.parse()
        epc = ExpressionParsingContext(
          implicitly,
          TermVariableValidator.LimitedList(VariableTracker.fromStepContext.baseVariableNames ++ definition.additionalVariableNames.toSeq.flatten),
          stepProvingContext.stepContext.boundVariableLists.map(_.zipWithIndex))
        conclusionOption <- getConclusionOption(epc, substitutions)
        newTargetStatementsOption <- definition.parseIntendedPremiseStatements(epc)
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        ExtractionApplication(result, _, extractionSteps, extractionPremises, extractionTargets) <- ExtractionHelper.applyExtractions(premise, extractionInferences, substitutions, newTargetStatementsOption, conclusionOption, PremiseFinder.findPremiseStepsOrTargets)
        extractionStep = Step.Elided.ifNecessary(extractionSteps, "Extracted").get
        finalStep = Step.Elided.ifNecessary(extractionPremises :+ extractionStep, "Extracted").get
      } yield (result, finalStep, extractionTargets)
    }
    definition.getFromInferenceOrPremise(withInference, withPremise)
  }

  val NumberOfSuggestionsToReturn = 10

  case class InferenceWithMaximumPossibleComplexity(inference: Inference, maximumPossibleComplexity: Int, index: Int)
  case class PossibleInferenceWithMaximumMatchingComplexity(possibleInference: PossibleInference, maximumMatchingComplexity: Int, minimumExtractionDepth: Int, index: Int)
  object PossibleInferenceWithMaximumMatchingComplexity {
    implicit val ordering: Ordering[PossibleInferenceWithMaximumMatchingComplexity] = Ordering.by(
      (i: PossibleInferenceWithMaximumMatchingComplexity) => (i.maximumMatchingComplexity, i.minimumExtractionDepth, i.index))(
      Ordering.Tuple3(Ordering.Int.reverse, Ordering.Int, Ordering.Int))
  }

  object +: {
    def unapply[T,Coll <: TraversableLike[T, Coll]](
      t: Coll with TraversableLike[T, Coll]): Option[(T, Coll)] =
      if(t.isEmpty) None
      else Some(t.head -> t.tail)
  }
  object Empty {
    def unapply[T,Coll <: TraversableLike[T, Coll]](
      t: Coll with TraversableLike[T, Coll]): Option[Unit] =
      if (t.isEmpty) Some(())
      else None
  }

  case class UnwrappedStatement(statement: Statement, unwrappers: Seq[Unwrapper]) {
    def definitionSymbols: Seq[String] = unwrappers.map(_.definitionSymbol)
  }

  def getPossibleTargets(statement: Statement)(implicit stepProvingContext: StepProvingContext): Seq[UnwrappedStatement] = {
    val provingContext = stepProvingContext.provingContext
    def helper(currentUnwrappedStatement: UnwrappedStatement, resultsSoFar: Seq[UnwrappedStatement], variableTracker: VariableTracker): Seq[UnwrappedStatement] = {
      (currentUnwrappedStatement.statement, provingContext.specificationInferenceOption, provingContext.deductionEliminationInferenceOption) match {
        case (generalizationStatement @ DefinedStatement(Seq(predicate: Statement), definition), Some((specificationInference, _, _, _)), _) if provingContext.generalizationDefinitionOption.contains(definition) =>
          val (variableName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(generalizationStatement.boundVariableNames.head)
          val newUnwrappedStatement = UnwrappedStatement(predicate, currentUnwrappedStatement.unwrappers :+ GeneralizationUnwrapper(variableName, definition, specificationInference))
          helper(newUnwrappedStatement, resultsSoFar :+ newUnwrappedStatement, newVariableTracker)
        case (DefinedStatement(Seq(antecedent: Statement, consequent: Statement), definition), _, Some((deductionEliminationInference, _ , _))) if provingContext.deductionDefinitionOption.contains(definition) =>
          val newUnwrappedStatement = UnwrappedStatement(consequent, currentUnwrappedStatement.unwrappers :+ DeductionUnwrapper(antecedent, definition, deductionEliminationInference))
          helper(newUnwrappedStatement, resultsSoFar :+ newUnwrappedStatement, variableTracker)
        case _ =>
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

      def findPossibleInference(inferenceWithComplexity: InferenceWithMaximumPossibleComplexity): Option[PossibleInferenceWithMaximumMatchingComplexity] = {
        import inferenceWithComplexity._
        val possibleTargets = for {
          possibleUnwrappedTargetStatement <- possibleUnwrappedTargetStatements
          possibleConclusions = spc.provingContext.extractionOptionsByInferenceId(inference.id)
            .filter(_.conclusion.calculateSubstitutions(possibleUnwrappedTargetStatement.statement).nonEmpty)
            .map(e => PossibleConclusion(e.conclusion, e.extractionInferences.map(_.id), e.additionalVariableNames))
          if possibleConclusions.nonEmpty
        } yield PossibleTarget(
          possibleUnwrappedTargetStatement.statement,
          possibleUnwrappedTargetStatement.definitionSymbols,
          possibleUnwrappedTargetStatement.unwrappers.ofType[GeneralizationUnwrapper].map(_.variableName).map(Seq(_)),
          possibleConclusions)

        if (possibleTargets.nonEmpty)
          Some(PossibleInferenceWithMaximumMatchingComplexity(
            PossibleInference(inference.summary, Some(possibleTargets), None),
            possibleTargets.flatMap(_.possibleConclusions.map(_.conclusion.structuralComplexity)).max,
            possibleTargets.flatMap(_.possibleConclusions.map(_.extractionInferenceIds.length)).min,
            index))
        else
          None
      }

      @scala.annotation.tailrec
      def recursivelyFindInferences(
        matchingInferences: Seq[InferenceWithMaximumPossibleComplexity],
        matchedInferences: Seq[PossibleInferenceWithMaximumMatchingComplexity],
        queuedInferences: SortedSet[PossibleInferenceWithMaximumMatchingComplexity]
      ): Seq[PossibleInference] = {
        if (matchedInferences.size >= NumberOfSuggestionsToReturn) { // We've already found the required number of matches
          matchedInferences.map(_.possibleInference)
        } else (matchingInferences, queuedInferences) match {
          case (matchHead +: _, queueHead +: queueTail) if queueHead.maximumMatchingComplexity > matchHead.maximumPossibleComplexity =>
            recursivelyFindInferences(matchingInferences, matchedInferences :+ queueHead, queueTail)
          case (matchHead +: matchTail, _) =>
            findPossibleInference(matchHead) match {
              case Some(possibleInferenceWithComplexity) =>
                recursivelyFindInferences(matchTail, matchedInferences, queuedInferences + possibleInferenceWithComplexity)
              case None =>
                recursivelyFindInferences(matchTail, matchedInferences, queuedInferences)
            }
          case (Empty(_), _) =>
            (matchedInferences ++ queuedInferences.take(NumberOfSuggestionsToReturn - matchedInferences.length)).map(_.possibleInference)
        }
      }

      val matchingInferences = filterInferences(stepProvingContext.provingContext.entryContext.allInferences, searchText)
        .mapWithIndex((i, index) => InferenceWithMaximumPossibleComplexity(i, i.conclusion.structuralComplexity, index))
        .sortBy(_.maximumPossibleComplexity)(Ordering[Int].reverse)

      recursivelyFindInferences(
        matchingInferences,
        Nil,
        SortedSet.empty)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/possiblePremisesForCurrentTarget"), produces = Array("application/json;charset=UTF-8"))
  def getPossibleInferencesForCurrentTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String,
    @RequestParam("target") serializedTargetStatement: String,
    @RequestParam("conclusion") serializedConclusionStatement: String
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      inference <- findInference(inferenceId)(stepProvingContext)
      possibleTarget <- getPossibleTargets(step.statement)(stepProvingContext).find(_.statement.serialized == serializedTargetStatement).orBadRequest(s"Could not find target $serializedTargetStatement")
      extractionOption <- stepProvingContext.provingContext.extractionOptionsByInferenceId(inference.id).find(_.conclusion.serialized == serializedConclusionStatement).orBadRequest(s"Could not find extraction option with conclusion $serializedConclusionStatement")
    } yield PossibleConclusionWithPremises.fromExtractionOptionWithTarget(extractionOption, possibleTarget.statement)(StepProvingContext.updateStepContext(possibleTarget.unwrappers.enhanceContext)(stepProvingContext))).toResponseEntity
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
          val possibleConclusions = spc.provingContext.extractionOptionsByInferenceId(inference.id)
            .map(PossibleConclusionWithPremises.fromExtractionOption(_, None))
          PossibleInference(inference.summary, None, Some(possibleConclusions))
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
      SubstatementExtractor.getExtractionOptions(premise.statement)
        .flatMap(PossibleConclusionWithPremises.fromExtractionOptionWithSubstitutions(_, _.calculateSubstitutions(step.statement, baseSubstitutions)))
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
      SubstatementExtractor.getExtractionOptions(premise.statement)
        .map(PossibleConclusionWithPremises.fromExtractionOption(_, Some(baseSubstitutions))(stepProvingContext))
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
