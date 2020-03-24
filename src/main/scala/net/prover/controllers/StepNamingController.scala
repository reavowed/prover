package net.prover.controllers

import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises, PossibleInference, StepDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.{Premise, ProofHelper, SimplificationFinder, Step, SubstitutionContext}
import net.prover.model._
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
          .mapCollect { inference =>
            val conclusions = for {
              extractionOption <- stepProvingContext.provingContext.extractionOptionsByInferenceId(inference.id)
              if ProofHelper.findNamingInferences.exists(_._1.conclusion.calculateSubstitutions(extractionOption.conclusion)(SubstitutionContext.outsideProof).nonEmpty)
            } yield PossibleConclusionWithPremises.fromExtractionOption(extractionOption, None)
            if (conclusions.nonEmpty) {
              Some(PossibleInference(inference.summary, None, Some(conclusions)))
            } else {
              None
            }
          }
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
          (namingInference, namingInferenceAssumption, substitutionsAfterPremise) <- ProofHelper.findNamingInferences.mapFind {
            case (i, Seq(singlePremise), a) =>
              singlePremise.calculateSubstitutions(premiseStatement).map { s => (i, a, s) }
            case _ =>
              None
          }
          substitutionsAfterConclusion <- namingInference.conclusion.calculateSubstitutions(resultStatement, substitutionsAfterPremise)
          substitutions <- substitutionsAfterConclusion.confirmTotality
          substitutedAssumption <- namingInferenceAssumption.applySubstitutions(substitutions, 1, substitutionContext.externalDepth)
          substitutedConclusion <- namingInference.conclusion.applySubstitutions(substitutions, 1, substitutionContext.externalDepth)
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
            substitutions))
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
