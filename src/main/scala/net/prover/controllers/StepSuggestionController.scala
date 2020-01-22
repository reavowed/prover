package net.prover.controllers

import monocle.Lens
import net.prover.controllers.models.{PathData, SubstitutionRequest}
import net.prover.model._
import net.prover.model.definitions.Transitivity
import net.prover.model.expressions.{DefinedStatement, Expression, Statement, StatementVariable, Term}
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch {

  case class SuggestedSubstitutions(
    statements: Map[String, Statement],
    terms: Map[String, Term],
    predicates: Map[(String, Int), Statement],
    functions: Map[(String, Int), Term],
    predicateApplications: Map[(String, Int), Seq[Statement]],
    functionApplications: Map[(String, Int), Seq[Term]])
  {
    def confirmTotality: Option[Substitutions] = {
      if (predicateApplications.isEmpty && functionApplications.isEmpty)
        Some(Substitutions(statements, terms, predicates, functions))
      else
        None
    }
  }

  object SuggestedSubstitutions {
    val empty = SuggestedSubstitutions(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    def apply(possibleSubstitutions: Substitutions.Possible)(implicit stepContext: StepContext): SuggestedSubstitutions = {
      val plainSubstitutions = possibleSubstitutions.stripApplications()
      def filterApplications[T <: Expression](
        applicationsMap: Map[(String, Int), Seq[(Seq[Term], T, Int)]],
        lens: Lens[Substitutions.Possible, Map[(String, Int), T]],
        applicationLens: Lens[Substitutions.Possible, Map[(String, Int), Seq[(Seq[Term], T, Int)]]]
      ): Map[(String, Int), Seq[T]] = {
        applicationsMap
          .map { case ((name, arity), applications) =>
            val results = applications
              .find { case (arguments, _, _) => (0 until arity).forall(i => arguments(i).applySubstitutions(plainSubstitutions).nonEmpty) }
              .map { case (arguments, value, depth) =>
                value.calculateApplicatives(arguments, plainSubstitutions, 0, depth, stepContext.externalDepth).map(_._1.asInstanceOf[T]).toSeq
              }
              .getOrElse(Nil)

            ((name, arity), results)
          }
          .filter { case (_, applications) => applications.nonEmpty }
      }

      SuggestedSubstitutions(
        possibleSubstitutions.statements,
        possibleSubstitutions.terms,
        possibleSubstitutions.predicates,
        possibleSubstitutions.functions,
        filterApplications(possibleSubstitutions.predicateApplications, Substitutions.Possible.predicatesLens, Substitutions.Possible.predicateApplicationsLens),
        filterApplications(possibleSubstitutions.functionApplications, Substitutions.Possible.functionsLens, Substitutions.Possible.functionApplicationsLens))
    }
  }

  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[SuggestedSubstitutions],
    rewriteInference: Option[Inference.Summary],
    conclusion: Statement)

  case class PossibleInference(
    inference: Inference.Summary,
    possibleConclusions: Seq[PossibleConclusion])

  case class PossibleConclusion(
    conclusion: Statement,
    possiblePremises: Seq[PossiblePremise],
    substitutions: Option[SuggestedSubstitutions],
    requiredSubstitutions: Substitutions.Required,
    extractionInferenceIds: Seq[String])

  case class PossiblePremise(
    premise: Statement,
    possibleMatches: Seq[PossiblePremiseMatch])

  case class PossiblePremiseMatch(matchingPremise: Statement, substitutions: SuggestedSubstitutions)

  def getPossiblePremises(
    premises: Seq[Statement],
    substitutions: Option[Substitutions.Possible])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[PossiblePremise] = {
    premises.map { premise =>
      val matches = stepProvingContext.allPremisesSimplestLast.map(_.statement).mapCollect { availablePremise =>
        premise.calculateSubstitutions(availablePremise, substitutions.getOrElse(Substitutions.Possible.empty))
          .map(s => PossiblePremiseMatch(availablePremise, SuggestedSubstitutions(s)))
      }
      PossiblePremise(premise, matches)
    }
  }

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
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val spc = stepProvingContext
      def findPossibleInference(inference: Inference): Option[PossibleInference] = {
        val possibleConclusions = for {
          extractionOption <- SubstatementExtractor.getExtractionOptions(inference)
          conclusion = extractionOption.extractionResult
          conclusionSubstitutions <- conclusion.calculateSubstitutions(step.statement).toSeq
          premises = inference.premises ++ extractionOption.premises
          possiblePremises = getPossiblePremises(premises, Some(conclusionSubstitutions))
        } yield PossibleConclusion(
          conclusion,
          possiblePremises,
          Some(SuggestedSubstitutions(conclusionSubstitutions)),
          (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether,
          extractionOption.inferences.map(_.id))
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
      (_, stepProvingContext) <- findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield {
      implicit val spc = stepProvingContext
      filterInferences(stepProvingContext.provingContext.entryContext.inferences, searchText)
        .reverse
        .take(10)
        .map { inference =>
          val possibleConclusions = for {
            extractionOption <- SubstatementExtractor.getExtractionOptions(inference)
            conclusion = extractionOption.extractionResult
            premises = inference.premises ++ extractionOption.premises
            possiblePremises = getPossiblePremises(premises, None)
          } yield PossibleConclusion(
            conclusion,
            possiblePremises,
            None,
            (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether,
            extractionOption.inferences.map(_.id))
          PossibleInference(inference.summary, possibleConclusions)
        }
    }).toResponseEntity
  }

  private def calculateSubstitutionsForTermOrSubTerm(targetTerm: Term, conclusionTerm: Term)(implicit stepContext: StepContext): Option[Substitutions.Possible] = {
    conclusionTerm.calculateSubstitutions(targetTerm) orElse
      (targetTerm.getTerms().map(_._1).toSet - targetTerm).toSeq.mapCollect(conclusionTerm.calculateSubstitutions).single
  }

  private def suggestInferencesForTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    searchText: String)(
    f: (Transitivity, Statement, StepContext) => Option[Term]
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      (targetSource, transitivityDefinition) <- stepProvingContext.provingContext.transitivityDefinitions.mapFind { case (_, definition) =>
        for {
          lhs <- f(definition, step.statement, stepProvingContext.stepContext)
        } yield (lhs, definition)
      }.orBadRequest("Target step is not a transitive statement")
    } yield {
      import stepProvingContext._
      implicit val spc = stepProvingContext

      def getPossibleInference(inference: Inference): Option[PossibleInference] = {
        val possibleConclusions = for {
          extractionOption <- SubstatementExtractor.getExtractionOptions(inference)
          conclusion = extractionOption.extractionResult
          conclusionSource <- f(transitivityDefinition, conclusion, stepProvingContext.stepContext).toSeq
          conclusionSubstitutions <- calculateSubstitutionsForTermOrSubTerm(targetSource, conclusionSource).toSeq
          premises = inference.premises ++ extractionOption.premises
          possiblePremises = getPossiblePremises(premises, Some(conclusionSubstitutions))
        } yield PossibleConclusion(
          conclusion,
          possiblePremises,
          Some(SuggestedSubstitutions(conclusionSubstitutions)),
          (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether,
          extractionOption.inferences.map(_.id))
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
    suggestInferencesForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText)(_.relation.unapply(_)(_).map(_._1))
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
    suggestInferencesForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText)(_.relation.unapply(_)(_).map(_._2))
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
      (_, stepProvingContext) <- findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
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
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
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
                getPossiblePremises(namingPremises, Some(s)),
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
      (step, stepProvingContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
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
