package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model._
import net.prover.model.definitions.BinaryJoiner
import net.prover.model.expressions._
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.collection.{SeqLike, SortedSet, TraversableLike}
import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch {


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
        if singleNamingPremise.boundVariableNames.single.nonEmpty
        premise <- stepProvingContext.allPremises
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
              None,
              Some(Seq(PossibleConclusionWithPremises(
                inference.conclusion,
                PossiblePremise.fromAvailablePremises(namingPremises, Some(s)),
                Some(SuggestedSubstitutions(s)),
                inference.requiredSubstitutions,
                Nil,
                Nil)))))
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

}
