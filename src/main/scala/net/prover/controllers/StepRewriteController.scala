package net.prover.controllers

import net.prover.controllers.StepRewriteController._
import net.prover.controllers.models.{DeductionUnwrapper, GeneralizationUnwrapper, PathData, PremiseRewrite, RewriteRequest, Unwrapper}
import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions._
import net.prover.model.proof._
import net.prover.util.Direction
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch with ChainingStepEditing {

  private def getReplacementPossibilities[T <: Expression : ReplacementMethods](expression: T)(implicit stepProvingContext: StepProvingContext): Seq[ReplacementPossibility[T]] = {
    ReplacementMethods[T].getReplacementPossibilitiesFromOuterExpression(expression, Nil, Nil)
  }

  private def getReplacementPossibilities(expression: Expression, pathsAlreadyRewrittenText: String)(implicit stepProvingContext: StepProvingContext): Seq[ReplacementPossibility[_ <: Expression]] = {
    val pathsAlreadyRewritten = pathsAlreadyRewrittenText.split(',').filter(_.nonEmpty).map(_.split('.').map(_.toInt))
    (expression match {
      case statement: Statement =>
        getReplacementPossibilities(statement)
      case term: Term =>
        getReplacementPossibilities(term)
    }) filter { case ReplacementPossibility(_, _,  _, path, _) =>
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
      val replacementPossibilities = getReplacementPossibilities(expression, pathsAlreadyRewrittenText)
      def getSuggestions(inference: Inference, source: Term, target: Term, reverse: Boolean): Option[InferenceRewriteSuggestion] = {
        val suggestions = replacementPossibilities.mapCollect { case ReplacementPossibility(term, _, depth, path, unwrappers) =>
          for {
            substitutions <- source.calculateSubstitutions(term)(SubstitutionContext.withExtraParameters(unwrappers.depth))
            result <- target.applySubstitutions(substitutions.stripApplications())(SubstitutionContext.withExtraParameters(unwrappers.depth)).map(_.insertExternalParameters(depth))
            _ <- PremiseFinder.findPremiseStepsForStatementsBySubstituting(inference.premises, substitutions)(StepProvingContext.updateStepContext(unwrappers.enhanceContext))
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
        .toList
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
      val replacementPossibilities = getReplacementPossibilities(expression, pathsAlreadyRewrittenText)
      stepProvingContext.allPremises.mapCollect { p =>
        for {
          (lhs, rhs) <- equality.unapply(p.statement)
          forward = replacementPossibilities.filter(_.term == lhs).map(_.path).map(PremiseRewritePath(_, reverse = false, rhs))
          reverse = replacementPossibilities.filter(_.term == rhs).map(_.path).map(PremiseRewritePath(_, reverse = true, lhs))
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

  def getRewriteStepForInference[TExpression <: Expression with TypedExpression[TExpression] : ReplacementMethods](
    inferenceId: String,
    baseTerm: Term,
    rewrite: RewriteRequest,
    unwrappers: Seq[Unwrapper],
    wrapperExpression: TExpression,
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Term, Term, Option[Step], Option[Inference.Summary], Seq[Unwrapper], TExpression)] = {
    for {
      inference <- findInference(inferenceId)
      (inferenceLhs, inferenceRhs) <- equality.unapply(inference.conclusion).orBadRequest("Inference conclusion was not equality")
      (sourceTemplate, targetTemplate) = if (!rewrite.reverse) (inferenceLhs, inferenceRhs) else (inferenceRhs, inferenceLhs)
      unwrappedStepContext = unwrappers.enhanceContext(implicitly)
      substitutions <- sourceTemplate.calculateSubstitutions(baseTerm)(unwrappedStepContext).orBadRequest("Could not find substitutions")
      (premiseSteps, premises, _) <- PremiseFinder.findPremiseStepsForStatementsBySubstituting(inference.premises, substitutions)(StepProvingContext(unwrappedStepContext, implicitly))
        .orBadRequest("Could not find premises")
      (removedUnwrappers, removedSource, removedPremises, removedWrapperExpression) = ReplacementMethods[TExpression].removeUnwrappers(baseTerm, premises, wrapperExpression, unwrappers)
      removedUnwrappedStepContext = removedUnwrappers.enhanceContext(implicitly)
      finalSubstitutionsAfterSource <- sourceTemplate.calculateSubstitutions(removedSource)(removedUnwrappedStepContext).orBadRequest("Could not find substitutions")
      finalSubstitutionsAfterPremises <- inference.premises.zip(removedPremises).foldLeft(Try(finalSubstitutionsAfterSource)) { case (s, (ip, p)) => s.flatMap(ip.calculateSubstitutions(p, _)(removedUnwrappedStepContext).orBadRequest("Could not find substitutions"))}
      finalSubstitutions <- finalSubstitutionsAfterPremises.confirmTotality.orBadRequest("Substitutions were not complete")
      rewrittenTerm <- targetTemplate.applySubstitutions(finalSubstitutions)(unwrappedStepContext).orBadRequest("Could not apply substitutions to target")
      substitutedConclusion <- inference.conclusion.applySubstitutions(finalSubstitutions)(unwrappedStepContext).orBadRequest("Could not apply substitutions to inference conclusion")
      assertionStep = Step.Assertion(substitutedConclusion, inference, removedPremises.map(Premise.Pending), finalSubstitutions)
      elidedStep = Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference).get
    } yield (removedSource, rewrittenTerm, Some(elidedStep), Some(inference), removedUnwrappers, removedWrapperExpression)
  }

  def getRewriteStepForPremise[TExpression <: Expression with TypedExpression[TExpression]](
    serializedPremiseStatement: String,
    baseTerm: Term,
    rewrite: RewriteRequest,
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext
  ): Try[Term] = {
    for {
      premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
      (premiseLhs, premiseRhs) <- equality.unapply(premiseStatement).orBadRequest("Premise was not equality")
      (sourceTerm, rewrittenTerm) = if (!rewrite.reverse) (premiseLhs, premiseRhs) else (premiseRhs, premiseLhs)
      _ <- (sourceTerm == baseTerm).orBadRequest("Premise did not match term at path")
    } yield rewrittenTerm
  }


  def rewrite[TExpression <: Expression with TypedExpression[TExpression] : ReplacementMethods, TStep](
    baseExpression: TExpression,
    rewriteList: Seq[Seq[RewriteRequest]],
    equality: Equality,
    direction: Direction)(
    applyRewrite: (Term, Term, Seq[Unwrapper], Wrapper[Term, TExpression], Seq[Step], Option[Inference.Summary], Option[Inference.Summary]) => Try[(TStep, Option[Inference.Summary], Wrapper[Term, TExpression])])(
    combine: (TExpression, TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => TStep)(
    elide: (TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => Step)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Step, TExpression)] = {
    for {
      (newTarget, steps, inferences) <- rewriteList.foldLeft(Try((baseExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrites) =>
        for {
          (currentExpression, stepsSoFar, inferencesSoFar) <- trySoFar
          (newTarget, steps, inferences) <- direction.reverseIfNecessary(rewrites).foldLeft(Try((currentExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrite) =>
            for {
              (currentInnerExpression, stepsSoFar, inferencesSoFar) <- trySoFar
              replacementPossibilities = getReplacementPossibilities(currentInnerExpression)
              ReplacementPossibility(baseTerm, function, _, _, baseUnwrappers) <- replacementPossibilities.find(_.path == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
              (term, rewrittenTerm, rewriteStepOption, inferenceOption, unwrappers, wrapperExpression) <- ((rewrite.inferenceId.map(getRewriteStepForInference(_, baseTerm, rewrite, baseUnwrappers, function, equality)) orElse
                rewrite.serializedPremiseStatement.map(getRewriteStepForPremise(_, baseTerm, rewrite, equality).map((baseTerm, _, None, None, baseUnwrappers, function)))) orBadRequest
                "Neither inference nor premise supplied").flatten
              wrapper = Wrapper.fromExpression(wrapperExpression)
              reverse = rewrite.reverse != direction.isReversed
              (source, result) = direction.swapSourceAndResult(term, rewrittenTerm)
              reversalStepOption = if (reverse) Some(equality.reversal.assertionStep(source, result)(unwrappers.enhanceContext(implicitly))) else None
              (step, resultInferenceOption, updatedWrapper) <- applyRewrite(source, result, unwrappers, wrapper, rewriteStepOption.toSeq ++ reversalStepOption.toSeq, inferenceOption, Some(equality.reversal.inference).filter(_ => reverse))
            } yield (updatedWrapper(rewrittenTerm), stepsSoFar :+ step, inferencesSoFar :+ resultInferenceOption)
          }.map(_.map2(direction.reverseIfNecessary))
          step = combine(currentExpression, newTarget, steps, inferences)
        } yield (newTarget, stepsSoFar :+ step , inferencesSoFar ++ inferences)
      }.map(_.map2(direction.reverseIfNecessary))
      step = elide(newTarget, steps, inferences)
    } yield (step, newTarget)
  }

  def substituteForRewrite(
    equality: Equality)(
    source: Term,
    result: Term,
    unwrappers: Seq[Unwrapper],
    wrapper: Wrapper[Term, Statement],
    steps: Seq[Step],
    rewriteInferenceOption: Option[Inference.Summary],
    fallbackInferenceOption: Option[Inference.Summary])(
    implicit stepContext: StepContext
  ): Try[(Step, Option[Inference.Summary], Wrapper[Term, Statement])] = {
    val substitutionStep = equality.substitution.assertionStep(source, result, wrapper)(unwrappers.enhanceContext(implicitly))
    val inference = rewriteInferenceOption.getOrElse(equality.substitution.inference.summary)
    val (updatedSteps, updatedWrapper) = unwrappers.rewrap(wrapper(source)(unwrappers.enhanceContext(implicitly)), steps :+ substitutionStep, wrapper, inference)
    val finalStep = if (updatedWrapper != wrapper)
      Step.Elided(updatedSteps, Some(inference), None)
    else
      Step.Elided.ifNecessary(updatedSteps, inference).get
    Success((finalStep, Some(inference), updatedWrapper))
  }

  def expandForRearrangement[TExpression <: Expression : ReplacementMethods](
    expansion: Expansion[TExpression])(
    source: Term,
    result: Term,
    unwrappers: Seq[Unwrapper],
    wrapper: Wrapper[Term, TExpression],
    steps: Seq[Step],
    rewriteInferenceOption: Option[Inference.Summary],
    fallbackInferenceOption: Option[Inference.Summary])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(RearrangementStep[TExpression], Option[Inference.Summary], Wrapper[Term, TExpression])] = {
    val expansionStepOption = expansion.assertionStepIfNecessary(source, result, wrapper)(unwrappers.enhanceContext(implicitly))
    val inferenceOption = rewriteInferenceOption orElse Some(expansion.inference.summary).filter(_ => expansionStepOption.nonEmpty) orElse fallbackInferenceOption
    for {
      (updatedSteps, updatedWrapper) <- ReplacementMethods[TExpression].rewrapWithDistribution(unwrappers, expansion.resultJoiner, source, result, steps ++ expansionStepOption.toSeq, wrapper, inferenceOption)
    } yield (RearrangementStep(updatedWrapper(result), updatedSteps, EqualityRewriter.rewriteElider(inferenceOption)), inferenceOption, updatedWrapper)
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
        (step, newTarget) <- rewrite(step.statement, rewrites, equality, Direction.Reverse)(substituteForRewrite(equality)) {
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
        (newStep, _) <- rewrite(premiseStatement, premiseRewrite.rewrites, equality, Direction.Forward)(substituteForRewrite(equality)){
          (_, _, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).get
        } {
          (_, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).get
        }
      } yield (step, Seq(newStep))
    }.toResponseEntity
  }

  def addRearrangementTransitivityForRewrite[TExpression <: Expression](
    transitivity: Transitivity[TExpression],
    direction: Direction)(
    sourceExpression: TExpression,
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepContext: StepContext
  ): RearrangementStep[TExpression] = {
    val (sourceTerm, targetTerm) = direction.swapSourceAndResult(sourceExpression, rewrittenExpression)
    val transitivitySteps = transitivity.addToRearrangement(sourceTerm, rearrangementSteps)
    RearrangementStep(targetTerm, transitivitySteps, EqualityRewriter.optionalRewriteElider(inferences))
  }

  def addTransitivityForRewrite[TExpression <: Expression](
    sourceExpression: TExpression,
    transitivity: Transitivity[TExpression],
    direction: Direction)(
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepContext: StepContext
  ): Step = {
    val transitivitySteps = transitivity.addToRearrangement(direction.getSource(sourceExpression, rewrittenExpression), rearrangementSteps)
    EqualityRewriter.optionalRewriteElider(inferences)(transitivitySteps).get
  }

  def rewriteForTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    rewrites: Seq[Seq[RewriteRequest]],
    direction: Direction
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingSteps {
      def createStepsWithExpansion[T <: Expression with TypedExpression[T] : ChainingMethods : ReplacementMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
          expansion <- stepProvingContext.provingContext.expansions.ofType[Expansion[T]]
            .find(e => e.sourceJoiner == equality.relation && e.resultJoiner == targetJoiner)
            .orBadRequest("No applicable expansion found")
          transitivity <- stepProvingContext.provingContext.transitivities.ofType[Transitivity[T]].find(_.statement == targetJoiner)
            .orBadRequest("No applicable transitivity found")
          (sourceTerm, destinationTerm) = direction.swapSourceAndResult(targetLhs, targetRhs)
          (rewriteStep, intermediateTerm) <- rewrite(sourceTerm, rewrites, equality, direction)(
            expandForRearrangement(expansion))(
            addRearrangementTransitivityForRewrite(transitivity, direction))(
            addTransitivityForRewrite(sourceTerm, transitivity, direction))
          (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediateTerm, destinationTerm)
          (rewriteLhs, rewriteRhs) = direction.swapSourceAndResult(sourceTerm, intermediateTerm)
          rewriteStepDefinition = ChainingStepDefinition(rewriteLhs, rewriteRhs, targetJoiner, Some(rewriteStep))
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetJoiner)
          (firstStep, secondStep) = direction.swapSourceAndResult(rewriteStepDefinition, targetStepDefinition)
        } yield (firstStep, secondStep, Nil)
      }
      def createStepsWithSubstitution(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
          transitivity <- stepProvingContext.provingContext.transitivities.ofType[Transitivity[Term]].find(_.statement == targetRelation)
            .orBadRequest("No applicable transitivity found")
          (sourceTerm, destinationTerm) = direction.swapSourceAndResult(targetLhs, targetRhs)
          (rewriteStep, intermediateTerm) <- rewrite(sourceTerm, rewrites, equality, direction)(
            expandForRearrangement(equality.expansion))(
            addRearrangementTransitivityForRewrite(equality.transitivity, direction))(
            addTransitivityForRewrite(sourceTerm, transitivity, direction))
          (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediateTerm, destinationTerm)
          (rewriteLhs, rewriteRhs) = direction.swapSourceAndResult(sourceTerm, intermediateTerm)
          rewriteStepDefinition = ChainingStepDefinition(rewriteLhs, rewriteRhs, equality.relation, Some(rewriteStep))
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetRelation)
          (firstStep, secondStep) = direction.swapSourceAndResult(rewriteStepDefinition, targetStepDefinition)
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
    rewriteForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, rewrites, Direction.Forward)
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
    rewriteForTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, rewrites, Direction.Reverse)
  }
}

object StepRewriteController {
  case class InferenceRewriteSuggestion(inference: Inference.Summary, reverse: Boolean, source: Term, result: Term, rewriteSuggestions: Seq[InferenceRewritePath])
  case class InferenceRewritePath(path: Seq[Int], result: Term)
  case class PremiseSuggestion(reference: PreviousLineReference, statement: Statement, rewriteSuggestions: Seq[PremiseRewritePath])
  case class PremiseRewritePath(path: Seq[Int], reverse: Boolean, result: Term)

  case class ReplacementPossibility[T <: Expression](term: Term, function: T, depth: Int, path: Seq[Int], unwrappers: Seq[Unwrapper])

  trait ReplacementMethods[T <: Expression] {
    def getReplacementPossibilitiesFromOuterExpression(t: T, path: Seq[Int], unwrappers: Seq[Unwrapper])(implicit stepProvingContext: StepProvingContext): Seq[ReplacementPossibility[T]]

    def rewrapWithDistribution(unwrappers: Seq[Unwrapper], joiner: BinaryJoiner[T], source: Term, result: Term, steps: Seq[Step], wrapper: Wrapper[Term, T], inferenceOption: Option[Inference])(implicit stepProvingContext: StepProvingContext): Try[(Seq[Step], Wrapper[Term, T])]
    def removeUnwrappers(source: Term, premises: Seq[Statement], wrapperExpression: T, unwrappers: Seq[Unwrapper])(implicit stepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], T)
  }
  object ReplacementMethods {
    def apply[T <: Expression](implicit replacementMethods: ReplacementMethods[T]) = replacementMethods

    implicit def fromExpressionType[T <: Expression](implicit expressionType: ExpressionType[T]): ReplacementMethods[T] = {
      expressionType match {
        case ExpressionType.StatementType =>
          StatementReplacementMethods.asInstanceOf[ReplacementMethods[T]]
        case ExpressionType.TermType =>
          TermReplacementMethods.asInstanceOf[ReplacementMethods[T]]
      }
    }

    object StatementReplacementMethods extends ReplacementMethods[Statement] {
      def getReplacementPossibilitiesFromExpression(statement: Statement, path: Seq[Int], unwrappers: Seq[Unwrapper], wrapper: Statement => Statement)(implicit stepProvingContext: StepProvingContext): Seq[ReplacementPossibility[Statement]] = {
        statement.getTerms() map { case (term, predicate, depth, innerPath) =>
          ReplacementPossibility[Statement](term, wrapper(predicate), depth, path ++ innerPath, unwrappers)
        }
      }
      override def getReplacementPossibilitiesFromOuterExpression(statement: Statement, path: Seq[Int], unwrappers: Seq[Unwrapper])(implicit stepProvingContext: StepProvingContext): Seq[ReplacementPossibility[Statement]] = {
        (statement, stepProvingContext.provingContext.deductionEliminationInferenceOption, stepProvingContext.provingContext.specificationInferenceOption) match {
          case (generalizedStatement @ DefinedStatement(Seq(predicate: Statement), definition), _, Some((specificationInference, _, _, _)))
            if stepProvingContext.provingContext.entryContext.generalizationDefinitionOption.contains(definition)
          =>
            val unwrapper = GeneralizationUnwrapper(generalizedStatement.boundVariableNames.head, definition, specificationInference)
            getReplacementPossibilitiesFromOuterExpression(predicate, path :+ 0, unwrappers :+ unwrapper)(StepProvingContext.updateStepContext(unwrapper.enhanceContext))
          case (DefinedStatement(Seq(antecedent: Statement, consequent: Statement), definition), Some((deductionEliminationInference, _, _)), _)
            if stepProvingContext.provingContext.entryContext.deductionDefinitionOption.contains(definition)
          =>
            val unwrapper = DeductionUnwrapper(antecedent, definition, deductionEliminationInference)
            getReplacementPossibilitiesFromExpression(antecedent, path :+ 0, unwrappers, definition(_, consequent)) ++ getReplacementPossibilitiesFromOuterExpression(consequent, path :+ 1, unwrappers :+ unwrapper)(StepProvingContext.updateStepContext(unwrapper.enhanceContext))
          case _ =>
            getReplacementPossibilitiesFromExpression(statement, path, unwrappers, identity)
        }
      }
      override def rewrapWithDistribution(unwrappers: Seq[Unwrapper], joiner: BinaryJoiner[Statement], source: Term, result: Term, steps: Seq[Step], wrapper: Wrapper[Term, Statement], inferenceOption: Option[Inference])(implicit stepProvingContext: StepProvingContext): Try[(Seq[Step], Wrapper[Term, Statement])] = {
        unwrappers.rewrapWithDistribution(joiner, source, result, steps, wrapper, inferenceOption)
      }
      def removeUnwrappers(source: Term, premises: Seq[Statement], wrapperStatement: Statement, unwrappers: Seq[Unwrapper])(implicit stepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], Statement) = {
        unwrappers.removeUnneeded(source, premises, wrapperStatement)
      }
    }
    object TermReplacementMethods extends ReplacementMethods[Term] {
      override def getReplacementPossibilitiesFromOuterExpression(term: Term, path: Seq[Int], unwrappers: Seq[Unwrapper])(implicit stepProvingContext: StepProvingContext): Seq[ReplacementPossibility[Term]] = {
        term.getTerms() map { case (innerTerm, function, depth, innerPath) =>
          ReplacementPossibility[Term](innerTerm, function, depth, path ++ innerPath, unwrappers)
        }
      }
      override def rewrapWithDistribution(unwrappers: Seq[Unwrapper], joiner: BinaryJoiner[Term], source: Term, result: Term, steps: Seq[Step], wrapper: Wrapper[Term, Term], inferenceOption: Option[Inference])(implicit stepProvingContext: StepProvingContext): Try[(Seq[Step], Wrapper[Term, Term])] = {
        if (unwrappers.nonEmpty) {
          Failure(new Exception("Unwrappers for term somehow"))
        } else {
          Success((steps, wrapper))
        }
      }
      def removeUnwrappers(source: Term, premises: Seq[Statement], wrapperTerm: Term, unwrappers: Seq[Unwrapper])(implicit stepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], Term) = {
        (Nil, source, premises, wrapperTerm)
      }
    }
  }
}
