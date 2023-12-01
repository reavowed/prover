package net.prover.controllers

import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises, PossibleInferenceWithConclusions, StepDefinition}
import net.prover.model._
import net.prover.model.definitions.NamingInference
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof._
import net.prover.proving.CreateAssertionStep
import net.prover.proving.suggestions.InferenceFilter
import net.prover.proving.suggestions.SuggestInferences.NumberOfSuggestionsToReturn
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepNamingController @Autowired() (val bookService: BookService) {

  @GetMapping(value = Array("/suggestInferencesForNamingByInference"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForNamingByInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath).map { implicit stepWithContext =>
      stepWithContext.provingContext.availableEntries.allInferences.filter(InferenceFilter(searchText).apply(_))
        .iterator
        .mapCollect { inference =>
          val conclusions = for {
            inferenceExtraction <- stepWithContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
            if ProofHelper.findNamingInferences.exists { case NamingInference(_, initialPremises, _, _, _) =>
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
    }.toResponseEntity
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
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { implicit stepWithContext =>
      import stepWithContext.step
      def getNamingWrapper(premiseStatement: Statement, resultStatement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Statement, Statement, SubstitutionContext, Step => Step.Naming)] = {
        for {
          variableName <- premiseStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single)
          (namingInference, substitutionsAfterPremise) <- ProofHelper.findNamingInferences.mapFind {
            case namingInference @ NamingInference(i, Seq(singlePremise), a, generalizationDefinition, deductionDefinition) =>
              singlePremise.calculateSubstitutions(premiseStatement).map { namingInference -> _ }
            case _ =>
              None
          }
          substitutionsAfterConclusion <- namingInference.baseInference.conclusion.calculateSubstitutions(resultStatement, substitutionsAfterPremise)
          substitutions <- substitutionsAfterConclusion.confirmTotality(namingInference.baseInference.variableDefinitions)
          substitutedAssumption <- namingInference.assumption.applySubstitutions(substitutions, 1, substitutionContext.externalDepth)
          substitutedConclusion <- namingInference.baseInference.conclusion.applySubstitutions(substitutions, 1, substitutionContext.externalDepth)
        } yield (
          substitutedAssumption,
          substitutedConclusion,
          SubstitutionContext.withExtraParameter,
          (substep: Step) => Step.Naming(
            variableName,
            substitutedAssumption,
            step.statement,
            Seq(substep),
            namingInference.baseInference.summary,
            Seq(Premise.Pending(premiseStatement)),
            substitutions,
            namingInference.generalizationDefinition,
            namingInference.deductionDefinition))
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
        (assertionStep, targetSteps) <- CreateAssertionStep(inferenceId, definition.parseIntendedConclusion, definition, Nil)
        (mainNamingAssumption, mainNamingConclusion, mainNamingContext, mainNamingWrapper) <- getNamingWrapper(assertionStep.statement, step.statement).orBadRequest("Could not find naming step to apply")
        innerStep = recurseNamingWrappers(mainNamingAssumption, mainNamingConclusion)(mainNamingContext)
        namingStep = mainNamingWrapper(innerStep)
      } yield targetSteps :+ assertionStep :+ namingStep
    }.toResponseEntity
  }

}
