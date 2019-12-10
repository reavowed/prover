package net.prover.controllers

import net.prover.controllers.models.{PathData, PremiseRewrite, RewriteRequest}
import net.prover.model.definitions.{Equality, RearrangementStep, Wrapper}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.{Expression, Statement, Term, TypedExpression}
import net.prover.model.proof._
import net.prover.model.{Inference, ProvingContext}
import net.prover.util.Swapper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch with TransitivityEditing {

  case class InferenceRewriteSuggestion(inference: Inference.Summary, reverse: Boolean, source: Term, result: Term, rewriteSuggestions: Seq[RewriteSuggestion])
  case class RewriteSuggestion(path: Seq[Int], result: Term)

  @GetMapping(value = Array("/rewriteSuggestions"), produces = Array("application/json;charset=UTF-8"))
  def getSuggestions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String,
    @RequestParam("expression") serializedExpression: String,
    @RequestParam("pathsAlreadyRewritten") pathsAlreadyRewrittenText: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (_, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
      stepProvingContext = StepProvingContext(stepContext, provingContext)
      expression <- Expression.parser(stepProvingContext).parseFromString(serializedExpression, "expression").recoverWithBadRequest
    } yield {
      implicit val implicitStepContext: StepContext = stepContext
      val pathsAlreadyRewritten = pathsAlreadyRewrittenText.split(',').filter(_.nonEmpty).map(_.split('.').map(_.toInt))
      val filter = inferenceFilter(searchText)
      val termsFunctionsAndPaths = expression.getTerms(stepContext).filter { case (_, _, path) =>
          !pathsAlreadyRewritten.exists(path.startsWith(_))
      }
      val inferences = provingContext.termRewriteInferences.filter { case (i, _, _) => filter(i) }

      def getSuggestions(inference: Inference, source: Term, target: Term, reverse: Boolean): Option[InferenceRewriteSuggestion] = {
        val suggestions = termsFunctionsAndPaths.mapCollect { case (term, _, path) =>
          for {
            substitutions <- source.calculateSubstitutions(term)
            result <- target.applySubstitutions(substitutions.stripApplications())
            _ <- PremiseFinder.findPremiseSteps(inference.premises, substitutions)(stepProvingContext)
          } yield RewriteSuggestion(path, result)
        }
        if (suggestions.nonEmpty)
          Some(InferenceRewriteSuggestion(inference.summary, reverse, source, target, suggestions))
        else
          None
      }

      inferences
          .flatMap { case (inference, left, right) =>
            getSuggestions(inference, left, right, reverse = false).toSeq ++ getSuggestions(inference, right, left, reverse = true).toSeq
          }
        .sortBy(_.inference.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
        .take(10)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/premises"), produces = Array("application/json;charset=UTF-8"))
  def getPremises(
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
    } yield StepProvingContext(stepContext, provingContext).allPremisesSimplestFirst.map(_.statement)).toResponseEntity
  }

  def rewrite[TExpression <: Expression with TypedExpression[TExpression], TStep](
    baseExpression: TExpression,
    rewriteList: Seq[Seq[RewriteRequest]],
    equality: Equality,
    swapper: Swapper)(
    f: (Term, Term, Wrapper[Term, TExpression], Seq[Step], Inference.Summary) => TStep)(
    combine: (TExpression, Seq[TStep], Seq[Inference.Summary]) => TStep)(
    elide: (Seq[TStep], Seq[Inference.Summary]) => Step)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Step, TExpression)] = {
    for {
      (newTarget, steps, inferences) <- rewriteList.foldLeft(Try((baseExpression, Seq.empty[TStep], Seq.empty[Inference.Summary]))) { case (trySoFar, rewrites) =>
        for {
          (currentExpression, stepsSoFar, inferencesSoFar) <- trySoFar
          (newTarget, steps, inferences) <- swapper.reverse(rewrites).foldLeft(Try((currentExpression, Seq.empty[TStep], Seq.empty[Inference.Summary]))) { case (trySoFar, rewrite) =>
            for {
              (currentInnerExpression, stepsSoFar, inferencesSoFar) <- trySoFar
              inference <- findInference(rewrite.inferenceId)
              (inferenceLhs, inferenceRhs) <- equality.unapply(inference.conclusion).orBadRequest("Inference conclusion was not equality")
              (sourceTemplate, targetTemplate) = if (!rewrite.reverse) (inferenceLhs, inferenceRhs) else (inferenceRhs, inferenceLhs)
              (baseTerm, function, _) <- currentInnerExpression.getTerms(stepProvingContext.stepContext).find(_._3 == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
              substitutions <- sourceTemplate.calculateSubstitutions(baseTerm).orBadRequest("Could not find substitutions")
              rewrittenTerm <- targetTemplate.applySubstitutions(substitutions.stripApplications()).orBadRequest("Could not apply substitutions to target")
              (premiseSteps, premises, finalPossibleSubsitutions) <- PremiseFinder.findPremiseSteps(inference.premises, substitutions).orBadRequest("Could not find premises")
              finalSubstitutions <- finalPossibleSubsitutions.confirmTotality.orBadRequest("Substitutions were not complete")
              substitutedConclusion <- inference.conclusion.applySubstitutions(finalSubstitutions).orBadRequest("Could not apply substitutions to inference conclusion")
              assertionStep = Step.Assertion(substitutedConclusion, inference, premises.map(Premise.Pending), finalSubstitutions)
              elidedStep = Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference).get
              reversalStepOption = if (rewrite.reverse != swapper.isReversed) Some((equality.reversal.assertionStep _).tupled(swapper.swap(baseTerm, rewrittenTerm))) else None
              wrapper = Wrapper.fromExpression(function)
              (source, result) = swapper.swap(baseTerm, rewrittenTerm)
              step = f(source, result, wrapper, elidedStep +: reversalStepOption.toSeq, inference)
            } yield (wrapper(rewrittenTerm), stepsSoFar :+ step , inferencesSoFar :+ inference)
          }.map(_.map2(swapper.reverse))
          step = combine(currentExpression, steps, inferences)
        } yield (newTarget, stepsSoFar :+ step , inferencesSoFar ++ inferences)
      }.map(_.map2(swapper.reverse))
      step = elide(steps, inferences)
    } yield (step, newTarget)
  }

  private def elideRewrite(steps: Seq[Step], inferences: Seq[Inference.Summary]) = {
    inferences.single match {
      case Some(inference) =>
        Step.Elided.ifNecessary(steps, inference).get
      case None =>
        Step.Elided.ifNecessary(steps, "Rewritten").get
    }
  }

  @PostMapping(value = Array("/rewrite"), produces = Array("application/json;charset=UTF-8"))
  def rewriteManually(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody rewrites: Seq[Seq[RewriteRequest]]
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        (step, newTarget) <- rewrite(step.statement, rewrites, equality, Swapper.swap) { (result, target, wrapper, steps, inference) =>
          val substitutionStep = equality.substitution.assertionStep(result, target, wrapper)
          Step.Elided.ifNecessary(steps :+ substitutionStep, inference).get
        } { (_, steps, inferences) =>
          elideRewrite(steps, inferences)
        }(elideRewrite)
      } yield Seq(Step.Target(newTarget), step)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rewriteAutomatically"), produces = Array("application/json;charset=UTF-8"))
  def rewriteAutomatically(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      EqualityRewriter.rewrite(step.statement)(stepProvingContext)
        .orBadRequest(s"Could not rewrite statement ${step.statement}")
        .map(Seq(_))
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rewritePremise"), produces = Array("application/json;charset=UTF-8"))
  def rewritePremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody premiseRewrite: PremiseRewrite
  ): ResponseEntity[_] = {
    replaceStepAndAddBeforeTransitivity[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        premiseStatement <- Statement.parser.parseFromString(premiseRewrite.serializedPremise, "premise").recoverWithBadRequest
        (newStep, _) <- rewrite(premiseStatement, premiseRewrite.rewrites, equality, Swapper.dontSwap) { (result, target, wrapper, steps, inference) =>
          val substitutionStep = equality.substitution.assertionStep(result, target, wrapper)
          Step.Elided.ifNecessary(steps :+ substitutionStep, inference).get
        } { (_, steps, inferences) =>
          elideRewrite(steps, inferences)
        }(elideRewrite)
      } yield (step, Seq(newStep))
    }.toResponseEntity
  }

  def substituteForRearrangement(
    equality: Equality)(
    target: Term,
    result: Term,
    wrapper: Wrapper[Term, Term],
    steps: Seq[Step],
    inference: Inference.Summary)(
    implicit substitutionContext: SubstitutionContext
  ): RearrangementStep = {
    val expansionStepOption = equality.expansion.assertionStepIfNecessary(target, result, wrapper)
    RearrangementStep(wrapper(result), steps ++ expansionStepOption.toSeq, inference)
  }

  def rewriteForTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    rewrites: Seq[Seq[RewriteRequest]],
    swapper: Swapper
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        if equality.transitivity == transitivity
        (step, intermediateTerm) <- rewrite(targetLhs, rewrites, equality, swapper)(substituteForRearrangement(equality)) { (term, steps, inferences) =>
          val transitivitySteps = equality.addTransitivityToRearrangement(term, steps)
          val elider = (steps: Seq[Step]) => inferences.single match {
            case Some(inference) =>
              Step.Elided.ifNecessary(steps, inference)
            case None =>
              Step.Elided.ifNecessary(steps, "Rewritten")
          }
          RearrangementStep(term, transitivitySteps, elider)
        } { (steps, inferences) =>
          val transitivitySteps = equality.addTransitivityToRearrangement(targetLhs, steps)
          inferences.single match {
            case Some(inference) =>
              Step.Elided.ifNecessary(transitivitySteps, inference).get
            case None =>
              Step.Elided.ifNecessary(transitivitySteps, "Rewritten").get
          }
        }
      } yield (step, Step.Target(equality(intermediateTerm, targetRhs)), intermediateTerm, Nil)
    }
  }

  @PostMapping(value = Array("/rewriteLeft"), produces = Array("application/json;charset=UTF-8"))
  def rewriteLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody rewrites: Seq[Seq[RewriteRequest]]
  ): ResponseEntity[_] = {
    rewriteForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, rewrites, Swapper.dontSwap)
  }

  @PostMapping(value = Array("/rewriteRight"), produces = Array("application/json;charset=UTF-8"))
  def rewriteRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody rewrites: Seq[Seq[RewriteRequest]]
  ): ResponseEntity[_] = {
    rewriteForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, rewrites, Swapper.swap)
  }
}
