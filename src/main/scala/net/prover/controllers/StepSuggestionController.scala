package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model._
import net.prover.model.definitions.BinaryJoiner
import net.prover.model.expressions._
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch {
  @GetMapping(value = Array("/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
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
      def findPossibleInference(inference: Inference): Option[PossibleInference] = {
        val possibleConclusions = SubstatementExtractor.getExtractionOptions(inference)
          .mapCollect(PossibleConclusion.fromExtractionOptionWithTarget(_, step.statement, inference.premises))
        if (possibleConclusions.nonEmpty)
          Some(PossibleInference(inference.summary, possibleConclusions))
        else
          None
      }

      filterInferences(stepProvingContext.provingContext.entryContext.inferences, searchText)
        .sortBy(_.conclusion.structuralComplexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .mapCollect(findPossibleInference)
        .matchingFirst(_.possibleConclusions.exists(!_.conclusion.isInstanceOf[StatementVariable]))
        .take(10)
        .toSeq
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestInferencesForPremise"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForPremise(
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
      filterInferences(stepProvingContext.provingContext.entryContext.inferences, searchText)
        .reverse
        .take(10)
        .map { inference =>
          val possibleConclusions = SubstatementExtractor.getExtractionOptions(inference)
            .map(PossibleConclusion.fromExtractionOption(_, None, inference.premises))
          PossibleInference(inference.summary, possibleConclusions)
        }
    }).toResponseEntity
  }

  private def suggestInferencesForTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    searchText: String)(
    getSourceTerm: (BinaryJoiner[_ <: Expression], Statement, StepContext) => Option[Expression]
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      (targetSource, relation) <- stepProvingContext.provingContext.definedBinaryStatements.mapFind { relation =>
        getSourceTerm(relation, step.statement, stepProvingContext.stepContext).map(_ -> relation)
      }.orBadRequest("Target step is not a binary relation")
    } yield {
      import stepProvingContext._
      implicit val spc = stepProvingContext

      def getSubstitutions(extractionResult: Statement): Option[Substitutions.Possible] = {
        def asEquality: Option[Substitutions.Possible] = {
          for {
            equality <- provingContext.equalityOption
            conclusionSource <- getSourceTerm(equality.relation, extractionResult, stepContext)
            substitutions <- conclusionSource.calculateSubstitutions(targetSource) orElse
              (targetSource.getTerms().map(_._1).toSet diff targetSource.asOptionalInstanceOf[Term].toSet).toSeq.mapCollect(conclusionSource.calculateSubstitutions).single
          } yield substitutions
        }
        def asMatchingRelation: Option[Substitutions.Possible] = {
          for {
            conclusionSource <- getSourceTerm(relation, extractionResult, stepContext)
            substitutions <- conclusionSource.calculateSubstitutions(targetSource)
          } yield substitutions
        }
        asEquality orElse asMatchingRelation
      }

      def getPossibleInference(inference: Inference): Option[PossibleInference] = {
        val possibleConclusions = SubstatementExtractor.getExtractionOptions(inference)
          .mapCollect(PossibleConclusion.fromExtractionOptionWithSubstitutions(_, getSubstitutions, inference.premises))
        if (possibleConclusions.nonEmpty) {
          Some(PossibleInference(inference.summary, possibleConclusions))
        } else {
          None
        }
      }

      filterInferences(provingContext.entryContext.inferences, searchText)
        .sortBy(_.conclusion.structuralComplexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .mapCollect(getPossibleInference)
        .take(10)
        .toSeq
    }).toResponseEntity

  }

  @GetMapping(value = Array("/suggestInferencesForTransitivityFromLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForTransitivityFromLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    suggestInferencesForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText)(_.unapply(_)(_).map(_._1))
  }

  @GetMapping(value = Array("/suggestInferencesForTransitivityFromRight"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForTransitivityFromRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    suggestInferencesForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText)(_.unapply(_)(_).map(_._2))
  }

  @GetMapping(value = Array("/suggestImmediateNamingPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestImmediateNamingPremises(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    (for {
      (_, stepProvingContext) <- bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val spc = stepProvingContext
      for {
        (_, Seq(singleNamingPremise: DefinedStatement), _) <- ProofHelper.findNamingInferences(stepProvingContext.provingContext.entryContext)
        if singleNamingPremise.scopedBoundVariableNames.single.nonEmpty
        premise <- stepProvingContext.allPremisesSimplestFirst
        if singleNamingPremise.calculateSubstitutions(premise.statement).nonEmpty
      } yield premise
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestNamingInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestNamingInferences(
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
      ProofHelper.findNamingInferences(stepProvingContext.provingContext.entryContext)
        .filter(_._1.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { case (inference, namingPremises, _) =>
          inference.conclusion.calculateSubstitutions(step.statement)
            .map(s => PossibleInference(
              inference.summary,
              Seq(PossibleConclusion(
                inference.conclusion,
                PossiblePremise.fromAvailablePremises(namingPremises, Some(s)),
                Some(SuggestedSubstitutions(s)),
                inference.requiredSubstitutions,
                Nil))))
        }
        .take(10)
    }).toResponseEntity
  }

  @PostMapping(value = Array("/suggestSubstitutions"), produces = Array("application/json;charset=UTF-8"))
  def suggestSubstitutions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody substitutionRequest: SubstitutionRequest
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      inference <- findInference(substitutionRequest.inferenceId)(stepProvingContext)
      premises <- substitutionRequest.serializedPremises.map { case (i, v) => Statement.parser(stepProvingContext).parseFromString(v, s"Premise $i").recoverWithBadRequest.map(i -> _) }.traverseTry.map(_.toMap)
      conclusionSubstitutions <- if (substitutionRequest.withConclusion)
        inference.conclusion.calculateSubstitutions(step.statement)(stepProvingContext.stepContext).orBadRequest("Could not calculate substitutions for inference conclusion")
      else
        Try(Substitutions.Possible.empty)
      substitutions <- premises.foldLeft(Try(conclusionSubstitutions)) { case (substitutionsSoFarTry, (index, statement)) =>
        for {
          substitutionsSoFar <- substitutionsSoFarTry
          premise <- inference.premises.lift(index.toInt).orBadRequest(s"Invalid premise index $index")
          nextSubstitutions <- premise.calculateSubstitutions(statement, substitutionsSoFar)(stepProvingContext.stepContext).orBadRequest(s"Could not calculate substitutions for premise $statement")
        } yield nextSubstitutions
      }
    } yield SuggestedSubstitutions(substitutions)(stepProvingContext.stepContext)).toResponseEntity
  }

  @GetMapping(value = Array("/extractionSuggestions"), produces = Array("application/json;charset=UTF-8"))
  def getExtractionSuggestions(
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
      premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
    } yield {
      SubstatementExtractor.getExtractionOptions(premise.statement)(stepProvingContext)
        .flatMap(PossibleConclusion.fromExtractionOptionWithTarget(_, step.statement)(stepProvingContext))
    }).toResponseEntity
  }
}
