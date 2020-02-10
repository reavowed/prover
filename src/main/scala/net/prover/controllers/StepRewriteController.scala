package net.prover.controllers

import net.prover.controllers.models.{PathData, PremiseRewrite, RewriteRequest}
import net.prover.model.Inference
import net.prover.model.definitions._
import net.prover.model.expressions._
import net.prover.model.proof._
import net.prover.util.Swapper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch with ChainingStepEditing {

  case class InferenceRewriteSuggestion(inference: Inference.Summary, reverse: Boolean, source: Term, result: Term, rewriteSuggestions: Seq[InferenceRewritePath])
  case class InferenceRewritePath(path: Seq[Int], result: Term)
  case class PremiseSuggestion(reference: PreviousLineReference, statement: Statement, rewriteSuggestions: Seq[PremiseRewritePath])
  case class PremiseRewritePath(path: Seq[Int], reverse: Boolean, result: Term)

  private def getTermsFunctionsAndPaths(expression: Expression, pathsAlreadyRewrittenText: String)(implicit stepContext: StepContext): Seq[(Term, Expression, Seq[Int])] = {
    val pathsAlreadyRewritten = pathsAlreadyRewrittenText.split(',').filter(_.nonEmpty).map(_.split('.').map(_.toInt))
    expression.getTerms().filter { case (_, _, path) =>
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
      (_, stepProvingContext) <- bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      expression <- Expression.parser(stepProvingContext).parseFromString(serializedExpression, "expression").recoverWithBadRequest
    } yield {
      implicit val spc = stepProvingContext
      val termsFunctionsAndPaths = getTermsFunctionsAndPaths(expression, pathsAlreadyRewrittenText)

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

      val filter = inferenceFilter(searchText)
      stepProvingContext.provingContext.termRewriteInferences
        .filter { case (i, _, _) => filter(i) }
        .sortBy(_._1.conclusion.structuralComplexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .flatMap { case (inference, left, right) =>
          getSuggestions(inference, left, right, reverse = false).toSeq ++ getSuggestions(inference, right, left, reverse = true).toSeq
        }
        .take(10)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/rewritePremiseSuggestions"), produces = Array("application/json;charset=UTF-8"))
  def getRewritePremiseSuggestions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("expression") serializedExpression: String,
    @RequestParam("pathsAlreadyRewritten") pathsAlreadyRewrittenText: String
  ): ResponseEntity[_] = {
    (for {
      (_, stepProvingContext) <- bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      expression <- Expression.parser(stepProvingContext).parseFromString(serializedExpression, "expression").recoverWithBadRequest
      equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
    } yield {
      implicit val spc = stepProvingContext
      val termsFunctionsAndPaths = getTermsFunctionsAndPaths(expression, pathsAlreadyRewrittenText)
      stepProvingContext.allPremises.mapCollect { p =>
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
      (_, stepProvingContext) <- bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield stepProvingContext.allPremises).toResponseEntity
  }

  def rewrite[TExpression <: Expression with TypedExpression[TExpression], TStep](
    baseExpression: TExpression,
    rewriteList: Seq[Seq[RewriteRequest]],
    equality: Equality,
    swapper: Swapper)(
    f: (Term, Term, Wrapper[Term, TExpression], Seq[Step], Option[Inference.Summary], Option[Inference.Summary]) => (TStep, Option[Inference.Summary]))(
    combine: (TExpression, TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => TStep)(
    elide: (TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => Step)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Step, TExpression)] = {
    for {
      (newTarget, steps, inferences) <- rewriteList.foldLeft(Try((baseExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrites) =>
        for {
          (currentExpression, stepsSoFar, inferencesSoFar) <- trySoFar
          (newTarget, steps, inferences) <- swapper.reverse(rewrites).foldLeft(Try((currentExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrite) =>
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
              (baseTerm, function, _) <- currentInnerExpression.getTerms().find(_._3 == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
              (rewrittenTerm, rewriteStepOption, inferenceOption) <- ((rewrite.inferenceId.map(applyInference(_, currentInnerExpression, baseTerm)) orElse
                rewrite.serializedPremiseStatement.map(applyPremise(_, currentInnerExpression, baseTerm))) orBadRequest
                "Neither inference nor premise supplied").flatten
              reverse = rewrite.reverse != swapper.isReversed
              reversalStepOption = if (reverse) Some((equality.reversal.assertionStep _).tupled(swapper.swapSourceAndResult(baseTerm, rewrittenTerm))) else None
              wrapper = Wrapper.fromExpression(function)
              (source, result) = swapper.swapSourceAndResult(baseTerm, rewrittenTerm)
              (step, resultInferenceOption) = f(source, result, wrapper, rewriteStepOption.toSeq ++ reversalStepOption.toSeq, inferenceOption, Some(equality.reversal.inference).filter(_ => reverse))
            } yield (wrapper(rewrittenTerm), stepsSoFar :+ step, inferencesSoFar :+ resultInferenceOption)
          }.map(_.map2(swapper.reverse))
          step = combine(currentExpression, newTarget, steps, inferences)
        } yield (newTarget, stepsSoFar :+ step , inferencesSoFar ++ inferences)
      }.map(_.map2(swapper.reverse))
      step = elide(newTarget, steps, inferences)
    } yield (step, newTarget)
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
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        (step, newTarget) <- rewrite(step.statement, rewrites, equality, Swapper.Swap) { (result, target, wrapper, steps, inferenceOption, _) =>
          val substitutionStep = equality.substitution.assertionStep(result, target, wrapper)
          val inference = inferenceOption.getOrElse(equality.substitution.inference.summary)
          (Step.Elided.ifNecessary(steps :+ substitutionStep, inference).get, Some(inference))
        } {
          (_, _, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).get
        } {
          (_, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).get
        }
        targetStepOption = if (stepProvingContext.allPremises.exists(_.statement == newTarget)) None else Some(Step.Target(newTarget))
      } yield targetStepOption.toSeq :+ step
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
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
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
        (newStep, _) <- rewrite(premiseStatement, premiseRewrite.rewrites, equality, Swapper.DontSwap) { (result, target, wrapper, steps, inferenceOption, _) =>
          val substitutionStep = equality.substitution.assertionStep(result, target, wrapper)
          val inference = inferenceOption.getOrElse(equality.substitution.inference.summary)
          (Step.Elided.ifNecessary(steps :+ substitutionStep, inference).get, Some(inference))
        } {
          (_, _, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).get
        } {
          (_, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).get
        }
      } yield (step, Seq(newStep))
    }.toResponseEntity
  }

  def expandForRearrangement[TExpression <: Expression](
    expansion: Expansion[TExpression])(
    target: Term,
    result: Term,
    wrapper: Wrapper[Term, TExpression],
    steps: Seq[Step],
    inference: Option[Inference.Summary],
    fallbackInference: Option[Inference.Summary])(
    implicit substitutionContext: SubstitutionContext
  ): (RearrangementStep[TExpression], Option[Inference.Summary]) = {
    val expansionStepOption = expansion.assertionStepIfNecessary(target, result, wrapper)
    val inferenceOption = inference orElse Some(expansion.inference.summary).filter(_ => expansionStepOption.nonEmpty) orElse fallbackInference
    (RearrangementStep(wrapper(result), steps ++ expansionStepOption.toSeq, EqualityRewriter.rewriteElider(inferenceOption)), inferenceOption)
  }

  def addTransitivityForRewrite[TExpression <: Expression](
    sourceExpression: TExpression,
    transitivity: Transitivity[TExpression],
    swapper: Swapper)(
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepContext: StepContext
  ): Step = {
    val transitivitySteps = transitivity.addToRearrangement(swapper.getSource(sourceExpression, rewrittenExpression), rearrangementSteps)
    EqualityRewriter.optionalRewriteElider(inferences)(transitivitySteps).get
  }

  def addRearrangementTransitivityForRewrite[TExpression <: Expression](
    transitivity: Transitivity[TExpression],
    swapper: Swapper)(
    sourceExpression: TExpression,
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepContext: StepContext
  ): RearrangementStep[TExpression] = {
    val (sourceTerm, targetTerm) = swapper.swapSourceAndResult(sourceExpression, rewrittenExpression)
    val transitivitySteps = transitivity.addToRearrangement(sourceTerm, rearrangementSteps)
    RearrangementStep(targetTerm, transitivitySteps, EqualityRewriter.optionalRewriteElider(inferences))
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingSteps {
      def createStepsWithExpansion[T <: Expression with TypedExpression[T] : ChainingMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
          expansion <- stepProvingContext.provingContext.expansions.ofType[Expansion[T]]
            .find(e => e.sourceJoiner == equality.relation && e.resultJoiner == targetJoiner)
            .orBadRequest("No applicable expansion found")
          transitivity <- stepProvingContext.provingContext.transitivities.ofType[Transitivity[T]].find(_.statement == targetJoiner)
            .orBadRequest("No applicable transitivity found")
          (sourceTerm, destinationTerm) = swapper.swapSourceAndResult(targetLhs, targetRhs)
          (rewriteStep, intermediateTerm) <- rewrite(sourceTerm, rewrites, equality, swapper)(
            expandForRearrangement(expansion))(
            addRearrangementTransitivityForRewrite(transitivity, swapper))(
            addTransitivityForRewrite(sourceTerm, transitivity, swapper))
          (targetLhs, targetRhs) = swapper.swapSourceAndResult(intermediateTerm, destinationTerm)
          (rewriteLhs, rewriteRhs) = swapper.swapSourceAndResult(sourceTerm, intermediateTerm)
          rewriteStepDefinition = ChainingStepDefinition(rewriteLhs, rewriteRhs, targetJoiner, Some(rewriteStep))
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetJoiner)
          (firstStep, secondStep) = swapper.swapSourceAndResult(rewriteStepDefinition, targetStepDefinition)
        } yield (firstStep, secondStep, Nil)
      }
      def createStepsWithSubstitution(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
          transitivity <- stepProvingContext.provingContext.transitivities.ofType[Transitivity[Term]].find(_.statement == targetRelation)
            .orBadRequest("No applicable transitivity found")
          (sourceTerm, destinationTerm) = swapper.swapSourceAndResult(targetLhs, targetRhs)
          (rewriteStep, intermediateTerm) <- rewrite(sourceTerm, rewrites, equality, swapper)(
            expandForRearrangement(equality.expansion))(
            addRearrangementTransitivityForRewrite(equality.transitivity, swapper))(
            addTransitivityForRewrite(sourceTerm, transitivity, swapper))
          (targetLhs, targetRhs) = swapper.swapSourceAndResult(intermediateTerm, destinationTerm)
          (rewriteLhs, rewriteRhs) = swapper.swapSourceAndResult(sourceTerm, intermediateTerm)
          rewriteStepDefinition = ChainingStepDefinition(rewriteLhs, rewriteRhs, equality.relation, Some(rewriteStep))
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetRelation)
          (firstStep, secondStep) = swapper.swapSourceAndResult(rewriteStepDefinition, targetStepDefinition)
        } yield (firstStep, secondStep, Nil)
      }

      override def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.Target])] = {
        createStepsWithExpansion(targetConnective, targetLhs, targetRhs, stepProvingContext)
      }

      override def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
        createStepsWithExpansion(targetRelation, targetLhs, targetRhs, stepProvingContext) orElse
          createStepsWithSubstitution(targetRelation, targetLhs, targetRhs, stepProvingContext)
      }
    })
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
    rewriteForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, rewrites, Swapper.DontSwap)
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
    rewriteForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, rewrites, Swapper.Swap)
  }
}
