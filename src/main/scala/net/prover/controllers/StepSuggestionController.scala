package net.prover.controllers

import net.prover._
import net.prover.controllers.models._
import net.prover.model.expressions._
import net.prover.model.proof._
import net.prover.structure.BookService
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

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
        (_, Seq(singleNamingPremise: DefinedStatement), _, _, _) <- ProofHelper.findNamingInferences(stepProvingContext.provingContext.entryContext)
        if singleNamingPremise.boundVariableNames.single.nonEmpty
        premise <- stepProvingContext.allPremises
        if PossibleSubstitutionCalculator.calculatePossibleSubstitutions(singleNamingPremise, premise.statement).nonEmpty
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
      val filter = inferenceFilter(searchText.toLowerCase)
      ProofHelper.findNamingInferences(stepProvingContext.provingContext.entryContext)
        .filter(x => filter(x._1))
        .reverse
        .mapCollect { case (inference, namingPremises, _, _, _) =>
          PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inference.conclusion, step.statement)
            .map(s => PossibleInferenceWithConclusions(
              inference.summary,
              Seq(PossibleConclusionWithPremises(
                inference.conclusion,
                PossiblePremise.fromAvailablePremises(namingPremises, Some(s), inference.variableDefinitions),
                Some(SuggestedSubstitutions(inference.variableDefinitions, s)),
                inference.variableDefinitions,
                Nil,
                Nil))))
        }
        .take(10)
    }).toResponseEntity
  }
}
