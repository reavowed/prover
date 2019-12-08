package net.prover.controllers

import monocle.Lens
import net.prover.controllers.models.{PathData, SubstitutionRequest}
import net.prover.model._
import net.prover.model.definitions.Transitivity
import net.prover.model.entries.Theorem
import net.prover.model.expressions.{DefinedStatement, Expression, Statement, Term}
import net.prover.model.proof.{ProofHelper, Step, StepContext, StepProvingContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

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
    rewriteInference: Option[Inference.Summary])

  case class PremiseSuggestions(immediateSubstitutions: Option[Substitutions], premiseMatches: Seq[Seq[PossiblePremiseMatch]])
  case class PossiblePremiseMatch(statement: Statement, substitutions: Seq[SuggestedSubstitutions])

  @GetMapping(value = Array("/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
      targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target")
    } yield {
      implicit val implicitStepContext: StepContext = stepContext
      def getSuggestions(inference: Inference): Seq[InferenceSuggestion] = {
        val direct = inference.conclusion.calculateSubstitutions(targetStep.statement).map(SuggestedSubstitutions.apply).map { substitutions =>
          InferenceSuggestion(inference.summary, inference.requiredSubstitutions, Seq(substitutions), None)
        }
        def rewritten = for {
          (rewriteInference, rewritePremise) <- provingContext.rewriteInferences
          rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(targetStep.statement).flatMap(_.confirmTotality).toSeq
          rewrittenTarget <- rewritePremise.applySubstitutions(rewriteSubstitutions).toSeq
          possibleSubstitutions <- inference.conclusion.calculateSubstitutions(rewrittenTarget).toSeq
          suggestedSubstituions = SuggestedSubstitutions(possibleSubstitutions)
        } yield InferenceSuggestion(inference.summary, inference.requiredSubstitutions, Seq(suggestedSubstituions), Some(rewriteInference.summary))
        direct.map(Seq(_)).getOrElse(rewritten)
      }
      filterInferences(provingContext.entryContext.inferences, searchText)
        .sortBy(_.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .flatMap(getSuggestions)
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
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
    } yield {
      filterInferences(entryContext.inferences, searchText)
        .reverse
        .take(10)
        .map { inference => InferenceSuggestion(inference.summary, inference.requiredSubstitutions, Seq(SuggestedSubstitutions.empty), None) }
    }).toResponseEntity
  }

  private def calculateSubstitutionsForTermOrSubTerm(targetTerm: Term, conclusionTerm: Term)(implicit stepContext: StepContext): Option[Seq[Substitutions.Possible]] = {
    conclusionTerm.calculateSubstitutions(targetTerm).map(Seq(_)) orElse
      Some((targetTerm.getTerms(stepContext).map(_._1).toSet - targetTerm).toSeq.mapCollect(conclusionTerm.calculateSubstitutions)).filter(_.nonEmpty)
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
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
      targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target")
      (targetPart, transitivityDefinition) <- provingContext.transitivityDefinitions.mapFind { case (_, definition) =>
        for {
          lhs <- f(definition, targetStep.statement, stepContext)
        } yield (lhs, definition)
      }.orBadRequest("Target step is not a transitive statement")
    } yield {
      implicit val implicitStepContext: StepContext = stepContext
      def getSuggestions(inference: Inference): Seq[InferenceSuggestion] = {
        val direct = for {
          conclusionPart <- f(transitivityDefinition, inference.conclusion, stepContext)
          possibleSubstitutions <- calculateSubstitutionsForTermOrSubTerm(targetPart, conclusionPart)
          suggestedSubstitutions = possibleSubstitutions.map(SuggestedSubstitutions.apply)
        } yield InferenceSuggestion(
          inference.summary,
          inference.requiredSubstitutions,
          suggestedSubstitutions,
          None)
        def rewritten = for {
          (rewriteInference, rewritePremise) <- provingContext.rewriteInferences
          rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(inference.conclusion).flatMap(_.confirmTotality).toSeq
          rewrittenConclusion <- rewritePremise.applySubstitutions(rewriteSubstitutions)(stepContext).toSeq
          conclusionPart <- f(transitivityDefinition, rewrittenConclusion, stepContext).toSeq
          possibleSubstitutions <- calculateSubstitutionsForTermOrSubTerm(targetPart, conclusionPart).toSeq
          suggestedSubstitutions = possibleSubstitutions.map(SuggestedSubstitutions.apply)
        } yield InferenceSuggestion(inference.summary, inference.requiredSubstitutions, suggestedSubstitutions, Some(rewriteInference.summary))
        direct.toSeq ++ rewritten
      }

      filterInferences(provingContext.entryContext.inferences, searchText)
        .sortBy(_.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .flatMap(getSuggestions)
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


  @GetMapping(value = Array("/suggestPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String,
    @RequestParam("withConclusion") withConclusion: Boolean
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
      stepProvingContext = StepProvingContext(stepContext, provingContext)
      inference <- findInference(inferenceId)(stepProvingContext)
      targetOption <- if (withConclusion)
        step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target").map(_.statement).map(Some(_))
      else
        Success(None)
    } yield {
      getPremiseSuggestions(inference.premises, targetOption, inference)(stepProvingContext)
    }).toResponseEntity
  }

  private def suggestPremisesForTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    inferenceId: String)(
    f: (Transitivity, Statement, StepContext) => Option[Term]
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
      stepProvingContext = StepProvingContext(stepContext, provingContext)
      targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target")
      (targetPart, transitivityDefinition) <- provingContext.transitivityDefinitions.mapFind { case (_, definition) =>
        for {
          lhs <- f(definition, targetStep.statement, stepContext)
        } yield (lhs, definition)
      }.orBadRequest("Target step is not a transitive statement")
      inference <- findInference(inferenceId)(stepProvingContext)
      conclusionPart <- f(transitivityDefinition, inference.conclusion, stepContext)
        .orBadRequest("Inference conclusion is not transitive statement")
      possibleSubstitutions <- calculateSubstitutionsForTermOrSubTerm(targetPart, conclusionPart)(stepContext)
        .orBadRequest("Could not calculate any substitutions for this inference")
    } yield {
      val premiseMatches = getPremiseMatches(inference.premises, possibleSubstitutions)(stepProvingContext)
      PremiseSuggestions(None, premiseMatches)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestPremisesForTransitivityFromLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForTransitivityFromLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    suggestPremisesForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, inferenceId)(_.relation.unapply(_)(_).map(_._1))
  }

  @GetMapping(value = Array("/suggestPremisesForTransitivityFromRight"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForTransitivityFromRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    suggestPremisesForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, inferenceId)(_.relation.unapply(_)(_).map(_._2))
  }

  @GetMapping(value = Array("/suggestImmediateNamingPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestNamingPremises(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (_, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
    } yield {
      implicit val stepProvingContext = StepProvingContext(stepContext, provingContext)
      for {
        (_, Seq(singleNamingPremise: DefinedStatement), _) <- ProofHelper.findNamingInferences(provingContext.entryContext)
        if singleNamingPremise.scopedBoundVariableNames.single.nonEmpty
        premise <- stepProvingContext.allPremisesSimplestFirst
        if singleNamingPremise.calculateSubstitutions(premise.statement).nonEmpty
      } yield premise.referencedLine
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
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step.Target](theorem, proofIndex, stepPath)
    } yield {
      implicit val implicitStepContext: StepContext = stepContext
      ProofHelper.findNamingInferences(provingContext.entryContext)
        .filter(_._1.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { case (inference, namingPremises, _) =>
          inference.conclusion.calculateSubstitutions(step.statement).map(SuggestedSubstitutions.apply)
            .map(s => InferenceSuggestion(
              inference.summary.copy(premises = namingPremises),
              inference.requiredSubstitutions,
              Seq(s),
              None))
        }
        .take(10)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestNamingPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForNaming(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step.Target](theorem, proofIndex, stepPath)
      stepProvingContext = StepProvingContext(stepContext, provingContext)
      inference <- findInference(inferenceId)(stepProvingContext)
      (namingPremises, _) <- ProofHelper.getNamingPremisesAndAssumption(inference)(stepProvingContext.provingContext.entryContext).orBadRequest(s"Inference $inferenceId was not naming inference")
    } yield {
      getPremiseSuggestions(
        namingPremises,
        Some(step.statement),
        inference)(
        stepProvingContext)
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
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (step, stepContext) <- findStep[Step.Target](theorem, proofIndex, stepPath)
      stepProvingContext = StepProvingContext(stepContext, provingContext)
      inference <- findInference(substitutionRequest.inferenceId)(stepProvingContext)
      premises <- substitutionRequest.serializedPremises.map { case (i, v) => Statement.parser(stepProvingContext).parseFromString(v, s"Premise $i").recoverWithBadRequest.map(i -> _) }.traverseTry.map(_.toMap)
      conclusionSubstitutions <- if (substitutionRequest.withConclusion)
        inference.conclusion.calculateSubstitutions(step.statement)(stepContext).orBadRequest("Could not calculate substitutions for inference conclusion")
      else
        Try(Substitutions.Possible.empty)
      substitutions <- premises.foldLeft(Try(conclusionSubstitutions)) { case (substitutionsSoFarTry, (index, statement)) =>
        for {
          substitutionsSoFar <- substitutionsSoFarTry
          premise <- inference.premises.lift(index.toInt).orBadRequest(s"Invalid premise index $index")
          nextSubstitutions <- premise.calculateSubstitutions(statement, substitutionsSoFar)(stepContext).orBadRequest(s"Could not calculate substitutions for premise $statement")
        } yield nextSubstitutions
      }
    } yield SuggestedSubstitutions(substitutions)(stepContext)).toResponseEntity
  }

  private def getPremiseSuggestions(
    premises: Seq[Statement],
    targetOption: Option[Statement],
    inference: Inference)(
    implicit stepProvingContext: StepProvingContext
  ): PremiseSuggestions = {
    def possibleConclusionSubstitutions = targetOption
      .map(inference.conclusion.calculateSubstitutions(_).toSeq)
      .getOrElse(Seq(Substitutions.Possible.empty))
    val availablePremises = stepProvingContext.allPremisesSimplestLast.map(_.statement)
    val premiseMatches = getPremiseMatches(premises, possibleConclusionSubstitutions)
    val immediateSubstitutions = targetOption.flatMap(target =>
      premiseMatches.mapFind(_.mapFind(_.substitutions.mapFind { possibleSubstitutions =>
        possibleSubstitutions.confirmTotality.find { substitutions =>
          inference.substitutePremisesAndValidateConclusion(target, substitutions).exists(_.forall(availablePremises.contains))
        }
      })))
    PremiseSuggestions(immediateSubstitutions, premiseMatches)
  }
  private def getPremiseMatches(
    premises: Seq[Statement],
    possibleConclusionSubstitutions: Seq[Substitutions.Possible])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[Seq[PossiblePremiseMatch]] = {
    val availablePremises = stepProvingContext.allPremisesSimplestLast.map(_.statement)
    premises.map { premise =>
      availablePremises.mapCollect { availablePremise =>
        val possibleSubstitutions = possibleConclusionSubstitutions.mapCollect(premise.calculateSubstitutions(availablePremise, _))
        val suggestedSubstitutions = possibleSubstitutions.map(SuggestedSubstitutions.apply)
        if (possibleSubstitutions.nonEmpty) {
          Some(PossiblePremiseMatch(availablePremise, suggestedSubstitutions))
        } else {
          None
        }
      }
    }
  }
}
