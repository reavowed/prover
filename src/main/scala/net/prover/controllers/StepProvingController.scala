package net.prover.controllers

import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models.{PathData, PossibleConclusion, PossibleInference, StepDefinition}
import net.prover.model.ExpressionParsingContext.TermVariableValidator
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.VariableTracker
import net.prover.model.proof._
import net.prover.model.{ExpressionParsingContext, Inference, ProvingContext, Substitutions}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.collection.{SortedSet, TraversableLike}
import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepProvingController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch {

  def rewriteFromConclusion(conclusion: Statement, rewriteInferenceId: Option[String])(implicit stepProvingContext: StepProvingContext): Try[(Option[Step.Assertion], Statement)] = {
    rewriteInferenceId.map { rewriteInferenceid =>
      for {
        (rewriteInference, rewriteInferencePremise) <- stepProvingContext.provingContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
        rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(conclusion).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
        substitutedPremise <- rewriteInferencePremise.applySubstitutions(rewriteSubstitutions).orBadRequest("Could not apply substitutions from rewrite inference")
      } yield (Some(Step.Assertion(conclusion, rewriteInference.summary, Seq(Premise.Pending(substitutedPremise)), rewriteSubstitutions)), substitutedPremise)
    }.getOrElse(Success((None, conclusion)))
  }
  def rewriteFromPremise(premise: Statement, rewriteInferenceId: Option[String])(implicit provingContext: ProvingContext, stepContext: StepContext): Try[(Option[Step.Assertion], Statement)] = {
    rewriteInferenceId.map { rewriteInferenceid =>
      for {
        (rewriteInference, rewriteInferencePremise) <- provingContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
        rewriteSubstitutions <- rewriteInferencePremise.calculateSubstitutions(premise).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
        substitutedConclusion <- rewriteInference.conclusion.applySubstitutions(rewriteSubstitutions).orBadRequest("Could not apply substitutions from rewrite inference")
      } yield (Some(Step.Assertion(substitutedConclusion, rewriteInference.summary, Seq(Premise.Pending(premise)), rewriteSubstitutions)), substitutedConclusion)
    }.getOrElse(Success((None, premise)))
  }

  def createStep(definition: StepDefinition, getTargetOption: Option[(ExpressionParsingContext, Substitutions) => Try[Statement]])(implicit stepProvingContext: StepProvingContext): Try[(Statement, Step, Seq[Step.Target])] = {
    def withInference(inferenceId: String) = {
      for {
        inference <- findInference(inferenceId)
        substitutions <- definition.substitutions.parse()
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        epc = ExpressionParsingContext(implicitly, TermVariableValidator.LimitedList(VariableTracker.fromInference(inference).baseVariableNames ++ definition.additionalVariableNames.toSeq.flatten), Nil)
        targetOption <- getTargetOption.map(f => f(epc, substitutions)).swap
        (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
        ExtractionApplication(result, mainPremise, extractionSteps, extractionPremises, extractionTargets) <-
          ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions, targetOption, PremiseFinder.findPremiseStepsOrTargets)
        mainAssertionWithCorrectConclusion = mainAssertion.copy(statement = mainPremise)
        extractionStep = Step.Elided.ifNecessary(mainAssertionWithCorrectConclusion +: extractionSteps, inference).get
        finalStep = Step.Elided.ifNecessary(mainPremises ++ extractionPremises :+ extractionStep, inference).get
      } yield (result, finalStep, mainTargets ++ extractionTargets)
    }
    def withPremise(serializedPremiseStatement: String) = {
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
        substitutions <- definition.substitutions.parse()
        epc = ExpressionParsingContext(
          implicitly,
          TermVariableValidator.LimitedList(VariableTracker.fromStepContext.baseVariableNames ++ definition.additionalVariableNames.toSeq.flatten),
          stepProvingContext.stepContext.boundVariableLists.map(_.zipWithIndex))
        targetOption <- getTargetOption.map(f => f(epc, substitutions)).swap
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        ExtractionApplication(result, _, extractionSteps, extractionPremises, extractionTargets) <- ExtractionHelper.applyExtractions(premise, extractionInferences, substitutions, targetOption, PremiseFinder.findPremiseStepsOrTargets)
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

      def findPossibleInference(inferenceWithComplexity: InferenceWithMaximumPossibleComplexity): Option[PossibleInferenceWithMaximumMatchingComplexity] = {
        import inferenceWithComplexity._
        val possibleConclusions = SubstatementExtractor.getExtractionOptions(inference)
          .mapCollect(PossibleConclusion.fromExtractionOptionWithTarget(_, step.statement))
        if (possibleConclusions.nonEmpty)
          Some(PossibleInferenceWithMaximumMatchingComplexity(
            PossibleInference(inference.summary, possibleConclusions),
            possibleConclusions.map(_.conclusion.structuralComplexity).max,
            possibleConclusions.map(_.extractionInferenceIds.length).min,
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
          val possibleConclusions = SubstatementExtractor.getExtractionOptions(inference)
            .map(PossibleConclusion.fromExtractionOption(_, None))
          PossibleInference(inference.summary, possibleConclusions)
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
        .flatMap(PossibleConclusion.fromExtractionOptionWithSubstitutions(_, _.calculateSubstitutions(step.statement, baseSubstitutions)))
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
        .map(PossibleConclusion.fromExtractionOption(_, Some(baseSubstitutions))(stepProvingContext))
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
        (result, newStep, targets) <- createStep(definition, Some((_, _) => Success(targetStep.statement)))
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
    replaceStepAndAddBeforeTransitivity[Step](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        (_, newStep, targets) <- createStep(
          definition,
          definition.serializedConclusionStatement.map(s =>
            (expressionParsingContext: ExpressionParsingContext, substitutions: Substitutions) =>
              for {
                conclusionStatement <- Statement.parser(expressionParsingContext).parseFromString(s, "conclusion").recoverWithBadRequest
                substitutedConclusionStatement <- conclusionStatement.applySubstitutions(substitutions).orBadRequest("Could not apply substitutions to intended conclusion")
              } yield substitutedConclusionStatement))
      } yield (step, targets :+ newStep)
    }.toResponseEntity
  }
}
