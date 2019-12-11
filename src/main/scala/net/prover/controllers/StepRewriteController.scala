package net.prover.controllers

import net.prover.controllers.models.{PathData, PremiseRewrite, RewriteRequest}
import net.prover.model.Inference
import net.prover.model.definitions.{Equality, RearrangementStep, Wrapper}
import net.prover.model.expressions.{Expression, Statement, Term, TypedExpression}
import net.prover.model.proof._
import net.prover.util.Swapper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch with TransitivityEditing {

  case class InferenceRewriteSuggestion(inference: Inference.Summary, reverse: Boolean, source: Term, result: Term, rewriteSuggestions: Seq[InferenceRewritePath])
  case class InferenceRewritePath(path: Seq[Int], result: Term)
  case class PremiseSuggestion(reference: PreviousLineReference, statement: Statement, rewriteSuggestions: Seq[PremiseRewritePath])
  case class PremiseRewritePath(path: Seq[Int], reverse: Boolean, result: Term)

  private def getTermsFunctionsAndPaths(expression: Expression, pathsAlreadyRewrittenText: String)(implicit stepContext: StepContext): Seq[(Term, Expression, Seq[Int])] = {
    val pathsAlreadyRewritten = pathsAlreadyRewrittenText.split(',').filter(_.nonEmpty).map(_.split('.').map(_.toInt))
    expression.getTerms(stepContext).filter { case (_, _, path) =>
      !pathsAlreadyRewritten.exists(path.startsWith(_))
    }
  }

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
    (for {
      (_, stepProvingContext) <- findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      expression <- Expression.parser(stepProvingContext).parseFromString(serializedExpression, "expression").recoverWithBadRequest
    } yield {
      implicit val spc = stepProvingContext
      val filter = inferenceFilter(searchText)
      val termsFunctionsAndPaths = getTermsFunctionsAndPaths(expression, pathsAlreadyRewrittenText)
      val inferences = stepProvingContext.provingContext.termRewriteInferences.filter { case (i, _, _) => filter(i) }

      def getSuggestions(inference: Inference, source: Term, target: Term, reverse: Boolean): Option[InferenceRewriteSuggestion] = {
        val suggestions = termsFunctionsAndPaths.mapCollect { case (term, _, path) =>
          for {
            substitutions <- source.calculateSubstitutions(term)
            result <- target.applySubstitutions(substitutions.stripApplications())
            _ <- PremiseFinder.findPremiseSteps(inference.premises, substitutions)(stepProvingContext)
          } yield InferenceRewritePath(path, result)
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

  @GetMapping(value = Array("/rewritePremiseSuggestions"), produces = Array("application/json;charset=UTF-8"))
  def getPremiseSuggestions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("expression") serializedExpression: String,
    @RequestParam("pathsAlreadyRewritten") pathsAlreadyRewrittenText: String
  ): ResponseEntity[_] = {
    (for {
      (_, stepProvingContext) <- findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      expression <- Expression.parser(stepProvingContext).parseFromString(serializedExpression, "expression").recoverWithBadRequest
      equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
    } yield {
      implicit val spc = stepProvingContext
      val termsFunctionsAndPaths = getTermsFunctionsAndPaths(expression, pathsAlreadyRewrittenText)
      stepProvingContext.allPremisesSimplestFirst.mapCollect { p =>
        for {
          (lhs, rhs) <- equality.unapply(p.statement)
          forward = termsFunctionsAndPaths.filter(_._1 == lhs).map(_._3).map(PremiseRewritePath(_, reverse = false, rhs))
          reverse = termsFunctionsAndPaths.filter(_._1 == rhs).map(_._3).map(PremiseRewritePath(_, reverse = true, lhs))
          total = forward ++ reverse
          if total.nonEmpty
        } yield PremiseSuggestion(p.referencedLine, p.statement, total)
      }
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
    (for {
      (_, stepProvingContext) <- findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield stepProvingContext.allPremisesSimplestFirst).toResponseEntity
  }

  def rewrite[TExpression <: Expression with TypedExpression[TExpression], TStep](
    baseExpression: TExpression,
    rewriteList: Seq[Seq[RewriteRequest]],
    equality: Equality,
    swapper: Swapper)(
    f: (Term, Term, Wrapper[Term, TExpression], Seq[Step], Option[Inference.Summary], Option[Inference.Summary]) => TStep)(
    combine: (TExpression, Seq[TStep], Seq[Inference.Summary]) => TStep)(
    elide: (Seq[TStep], Seq[Inference.Summary]) => Step)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Step, TExpression)] = {
    for {
      (newTarget, steps, inferences) <- rewriteList.foldLeft(Try((baseExpression, Seq.empty[TStep], Seq.empty[Inference.Summary]))) { case (trySoFar, rewrites) =>
        for {
          (currentExpression, stepsSoFar, inferencesSoFar) <- trySoFar
          (newTarget, steps, inferences) <- swapper.reverse(rewrites).foldLeft(Try((currentExpression, Seq.empty[TStep], Seq.empty[Inference.Summary]))) { case (trySoFar, rewrite) =>

            def applyInference(inferenceId: String, currentInnerExpression: TExpression, baseTerm: Term): Try[(Term, Option[Step], Option[Inference.Summary])] = {
              for {
                inference <- findInference(inferenceId)
                (inferenceLhs, inferenceRhs) <- equality.unapply(inference.conclusion).orBadRequest("Inference conclusion was not equality")
                (sourceTemplate, targetTemplate) = if (!rewrite.reverse) (inferenceLhs, inferenceRhs) else (inferenceRhs, inferenceLhs)
                substitutions <- sourceTemplate.calculateSubstitutions(baseTerm).orBadRequest("Could not find substitutions")
                rewrittenTerm <- targetTemplate.applySubstitutions(substitutions.stripApplications()).orBadRequest("Could not apply substitutions to target")
                (premiseSteps, premises, finalPossibleSubstitutions) <- PremiseFinder.findPremiseSteps(inference.premises, substitutions).orBadRequest("Could not find premises")
                finalSubstitutions <- finalPossibleSubstitutions.confirmTotality.orBadRequest("Substitutions were not complete")
                substitutedConclusion <- inference.conclusion.applySubstitutions(finalSubstitutions).orBadRequest("Could not apply substitutions to inference conclusion")
                assertionStep = Step.Assertion(substitutedConclusion, inference, premises.map(Premise.Pending), finalSubstitutions)
                elidedStep = Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference).get
              } yield (rewrittenTerm, Some(elidedStep), Some(inference))
            }
            def applyPremise(serializedPremiseStatement: String, currentInnerExpression: TExpression, baseTerm: Term): Try[(Term, Option[Step], Option[Inference.Summary])] = {
              for {
                premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
                (premiseLhs, premiseRhs) <- equality.unapply(premiseStatement).orBadRequest("Premise was not equality")
                (sourceTerm, rewrittenTerm) = if (!rewrite.reverse) (premiseLhs, premiseRhs) else (premiseRhs, premiseLhs)
                _ <- (sourceTerm == baseTerm).orBadRequest("Premise did not match term at path")
              } yield (rewrittenTerm, None, None)
            }

            for {
              (currentInnerExpression, stepsSoFar, inferencesSoFar) <- trySoFar
              (baseTerm, function, _) <- currentInnerExpression.getTerms(stepProvingContext.stepContext).find(_._3 == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
              (rewrittenTerm, rewriteStepOption, inferenceOption) <- ((rewrite.inferenceId.map(applyInference(_, currentInnerExpression, baseTerm)) orElse
                rewrite.serializedPremiseStatement.map(applyPremise(_, currentInnerExpression, baseTerm))) orBadRequest
                "Neither inference nor premise supplied").flatten
              reverse = rewrite.reverse != swapper.isReversed
              reversalStepOption = if (reverse) Some((equality.reversal.assertionStep _).tupled(swapper.swap(baseTerm, rewrittenTerm))) else None
              wrapper = Wrapper.fromExpression(function)
              (source, result) = swapper.swap(baseTerm, rewrittenTerm)
              step = f(source, result, wrapper, rewriteStepOption.toSeq ++ reversalStepOption.toSeq, inferenceOption, Some(equality.reversal.inference).filter(_ => reverse))
            } yield (wrapper(rewrittenTerm), stepsSoFar :+ step , inferencesSoFar ++ inferenceOption.toSeq)
          }.map(_.map2(swapper.reverse))
          step = combine(currentExpression, steps, inferences)
        } yield (newTarget, stepsSoFar :+ step , inferencesSoFar ++ inferences)
      }.map(_.map2(swapper.reverse))
      step = elide(steps, inferences)
    } yield (step, newTarget)
  }

  private def elideRewrite(steps: Seq[Step], inferences: Seq[Inference.Summary]) = {
    Step.Elided.ifNecessary(steps, inferences.single, "Rewritten").get
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
        (step, newTarget) <- rewrite(step.statement, rewrites, equality, Swapper.swap) { (result, target, wrapper, steps, inference, _) =>
          val substitutionStep = equality.substitution.assertionStep(result, target, wrapper)
          Step.Elided.ifNecessary(steps :+ substitutionStep, inference.getOrElse(equality.substitution.inference)).get
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
        (newStep, _) <- rewrite(premiseStatement, premiseRewrite.rewrites, equality, Swapper.dontSwap) { (result, target, wrapper, steps, inference, _) =>
          val substitutionStep = equality.substitution.assertionStep(result, target, wrapper)
          Step.Elided.ifNecessary(steps :+ substitutionStep, inference.getOrElse(equality.substitution.inference)).get
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
    inference: Option[Inference.Summary],
    fallbackInference: Option[Inference.Summary])(
    implicit substitutionContext: SubstitutionContext
  ): RearrangementStep = {
    val expansionStepOption = equality.expansion.assertionStepIfNecessary(target, result, wrapper)
    RearrangementStep(wrapper(result), steps ++ expansionStepOption.toSeq, inference orElse Some(equality.expansion.inference).filter(_ => !wrapper.isIdentity) orElse fallbackInference, "Rewritten")
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
      } yield (Some(step), Some(Step.Target(equality(intermediateTerm, targetRhs))), intermediateTerm, Nil)
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
