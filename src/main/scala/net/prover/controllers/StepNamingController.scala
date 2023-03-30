package net.prover.controllers

import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises, PossibleInferenceWithConclusions, StepDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.{Premise, ProofHelper, SimplificationFinder, Step, SubstitutionContext}
import net.prover.model._
import net.prover.proving.CreateAssertionStep
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepNamingController @Autowired() (val bookService: BookService) extends InferenceSearch {

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
      stepWithContext <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val stepProvingContext = stepWithContext.stepProvingContext
      filterInferences(stepProvingContext.provingContext.availableEntries.allInferences, searchText)
          .iterator
          .mapCollect { inference =>
            val conclusions = for {
              inferenceExtraction <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
              if ProofHelper.findNamingInferences.exists { case (_, initialPremises, _, _, _) =>
                initialPremises.single.exists(_.calculateSubstitutions(inferenceExtraction.conclusion)(SubstitutionContext.outsideProof).nonEmpty)
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
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { stepWithContext =>
      import stepWithContext.step
      import stepWithContext.stepProvingContext

      def getNamingWrapper(premiseStatement: Statement, resultStatement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Statement, Statement, SubstitutionContext, Step => Step.Naming)] = {
        for {
          variableName <- premiseStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single)
          (namingInference, namingInferenceAssumption, substitutionsAfterPremise, generalizationDefinition, deductionDefinition) <- ProofHelper.findNamingInferences.mapFind {
            case (i, Seq(singlePremise), a, generalizationDefinition, deductionDefinition) =>
              singlePremise.calculateSubstitutions(premiseStatement).map { s => (i, a, s, generalizationDefinition, deductionDefinition) }
            case _ =>
              None
          }
          substitutionsAfterConclusion <- namingInference.conclusion.calculateSubstitutions(resultStatement, substitutionsAfterPremise)
          substitutions <- substitutionsAfterConclusion.confirmTotality(namingInference.variableDefinitions)
          substitutedAssumption <- namingInferenceAssumption.applySubstitutions(substitutions, 1, substitutionContext.externalDepth)
          substitutedConclusion <- namingInference.conclusion.applySubstitutions(substitutions, 1, substitutionContext.externalDepth)
        } yield (
          substitutedAssumption,
          substitutedConclusion,
          SubstitutionContext.withExtraParameter,
          (substep: Step) => Step.Naming(
            variableName,
            substitutedAssumption,
            step.statement,
            Seq(substep),
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
        (conclusion, assertionStep, targetSteps) <- CreateAssertionStep(inferenceId, definition.parseIntendedConclusion, definition, Nil)
        (mainNamingAssumption, mainNamingConclusion, mainNamingContext, mainNamingWrapper) <- getNamingWrapper(conclusion, step.statement).orBadRequest("Could not find naming step to apply")
        innerStep = recurseNamingWrappers(mainNamingAssumption, mainNamingConclusion)(mainNamingContext)
        namingStep = mainNamingWrapper(innerStep)
      } yield targetSteps :+ assertionStep :+ namingStep
    }.toResponseEntity
  }

}
