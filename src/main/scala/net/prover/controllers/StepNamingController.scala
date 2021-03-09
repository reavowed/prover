package net.prover.controllers

import net.prover._
import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises, PossibleInferenceWithConclusions, StepDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof._
import net.prover.old.OldSubstitutionApplier
import net.prover.structure.BookService
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepNamingController @Autowired() (val bookService: BookService) extends BookModification with StepCreation with InferenceSearch {

  @GetMapping(value = Array("/suggestInferencesForNamingByInference"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForNamingByInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (_, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val spc = stepProvingContext
      filterInferences(stepProvingContext.provingContext.entryContext.allInferences, searchText)
          .iterator
          .mapCollect { inference =>
            val conclusions = for {
              inferenceExtraction <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
              if ProofHelper.findNamingInferences.exists { case (_, initialPremises, _, _, _) =>
                initialPremises.single.exists(PossibleSubstitutionCalculator.calculatePossibleSubstitutions(_, inferenceExtraction.conclusion)(SubstitutionContext.outsideProof).nonEmpty)
              }
            } yield PossibleConclusionWithPremises.fromExtraction(inferenceExtraction, None)
            if (conclusions.nonEmpty) {
              Some(PossibleInferenceWithConclusions(inference.summary, conclusions))
            } else {
              None
            }
          }
          .take(NumberOfSuggestionsToReturn)
          .toList
    }).toResponseEntity
  }

  @PostMapping(value = Array("/introduceNamingByInference"))
  def introduceNamingByInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (targetStep, stepProvingContext) =>
      implicit val spc = stepProvingContext

      def getNamingWrapper(premiseStatement: Statement, resultStatement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Statement, Statement, SubstitutionContext, Step => Step.Naming)] = {
        for {
          variableName <- premiseStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single)
          (namingInference, namingInferenceAssumption, substitutionsAfterPremise, generalizationDefinition, deductionDefinition) <- ProofHelper.findNamingInferences.mapFind {
            case (i, Seq(singlePremise), a, generalizationDefinition, deductionDefinition) =>
              PossibleSubstitutionCalculator.calculatePossibleSubstitutions(singlePremise, premiseStatement).map { s => (i, a, s, generalizationDefinition, deductionDefinition) }
            case _ =>
              None
          }
          substitutionsAfterConclusion <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(namingInference.conclusion, resultStatement, substitutionsAfterPremise)
          substitutions <- substitutionsAfterConclusion.confirmTotality(namingInference.variableDefinitions)
          substitutedAssumption <- OldSubstitutionApplier.applySubstitutionsInsideStep(namingInferenceAssumption, substitutions).toOption
          substitutedConclusion <- OldSubstitutionApplier.applySubstitutionsInsideStep(namingInference.conclusion, substitutions).toOption
        } yield (
          substitutedAssumption,
          substitutedConclusion,
          SubstitutionContext.withExtraParameter,
          (step: Step) => Step.Naming(
            variableName,
            substitutedAssumption,
            targetStep.statement,
            Seq(step),
            namingInference.summary,
            Seq(Premise.Pending(premiseStatement)),
            substitutions,
            generalizationDefinition,
            deductionDefinition))
      }

      def recurseNamingWrappers(currentAssumption: Statement, currentConclusion: Statement)(implicit currentContext: SubstitutionContext): Step = {
        SimplificationFinder.getSimplifications(currentAssumption).mapCollect(getNamingWrapper(_, currentConclusion)(currentContext)).single match {
          case Some((newAssumption, newConclusion, newContext, wrapper)) =>
            wrapper(recurseNamingWrappers(newAssumption, newConclusion)(newContext))
          case None =>
            Step.Target(currentConclusion)
        }
      }

      for {
        inferenceId <- definition.inferenceId.orBadRequest("Inference id must be provided")
        (conclusion, assertionStep, targetSteps) <- createAssertionStepForInference(inferenceId, definition.parseIntendedConclusion, definition, Nil)
        (mainNamingAssumption, mainNamingConclusion, mainNamingContext, mainNamingWrapper) <- getNamingWrapper(conclusion, targetStep.statement).orBadRequest("Could not find naming step to apply")
        innerStep = recurseNamingWrappers(mainNamingAssumption, mainNamingConclusion)(mainNamingContext)
        namingStep = mainNamingWrapper(innerStep)
      } yield targetSteps :+ assertionStep :+ namingStep
    }.toResponseEntity
  }

}
