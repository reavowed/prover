package net.prover.controllers

import net.prover.controllers.StepRewriteController._
import net.prover.controllers.models._
import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions._
import net.prover.model.proof.EqualityRewriter.{RewriteMethods, RewritePossibility}
import net.prover.model.proof._
import net.prover.model.unwrapping.Unwrapper
import net.prover.proving.FindInference
import net.prover.proving.extraction.{ExtractionApplier, ExtractionDefinition}
import net.prover.proving.premiseFinding.DerivationFinder
import net.prover.proving.stepReplacement.InsertStepBeforeChain
import net.prover.proving.suggestions.InferenceFilter
import net.prover.proving.suggestions.SuggestInferences.NumberOfSuggestionsToReturn
import net.prover.util.Direction
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (implicit val bookService: BookService) extends ChainingStepEditing {

  private def getRewritePossibilities[T <: Expression : RewriteMethods](expression: T)(implicit stepWithContext: StepWithContext): Seq[RewritePossibility[T]] = {
    RewriteMethods[T].getRewritePossibilitiesFromOuterExpression(expression, Nil, Nil)
  }

  private def getRewritePossibilities(expression: Expression, pathsAlreadyRewrittenText: String)(implicit stepWithContext: StepWithContext): Seq[RewritePossibility[_ <: Expression]] = {
    val pathsAlreadyRewritten = pathsAlreadyRewrittenText.split(',').filter(_.nonEmpty).map(_.split('.').map(_.toInt))
    (expression match {
      case statement: Statement =>
        getRewritePossibilities(statement)
      case term: Term =>
        getRewritePossibilities(term)
    }) filter { case RewritePossibility(_, _,  _, path, _, _) =>
      !pathsAlreadyRewritten.exists(path.startsWith(_))
    }
  }

  implicit val seqOrdering: Ordering[Seq[Int]] =
    (x: Seq[Int], y: Seq[Int]) => {
      val res0 = Ordering[Int].compare(x.length, y.length)
      if (res0 != 0)
        res0
      else
        Ordering.Implicits.seqOrdering[Seq, Int].compare(x, y)
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
    bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath).flatMap(implicit stepWithContext =>
      for {
        expression <- Expression.parser.parseFromString(serializedExpression, "expression").recoverWithBadRequest
      } yield {
        val replacementPossibilities = getRewritePossibilities(expression, pathsAlreadyRewrittenText)

        def getRewritePath(termRewriteInference: TermRewriteInference, replacementPossibility: RewritePossibility[_ <: Expression]): Option[(Term, Term, Seq[Int])] = {
          import replacementPossibility.{depth, path, term, unwrappers}
          for {
            substitutionsAfterLhs <- termRewriteInference.lhs.calculateSubstitutions(term)(SubstitutionContext.withExtraParameters(unwrappers.depth))
            (_, substitutionsAfterPremises) <- DerivationFinder.findDerivationsForStatementsBySubstituting(termRewriteInference.premises, substitutionsAfterLhs)(unwrappers.enhanceStepProvingContext)
            substitutions <- substitutionsAfterPremises.confirmTotality(termRewriteInference.variableDefinitions)
            result <- termRewriteInference.rhs.applySubstitutions(substitutions)(SubstitutionContext.withExtraParameters(unwrappers.depth)).map(_.insertExternalParameters(depth))
          } yield (term, result, path)
        }

        def getSuggestionsForInference(termRewriteInference: TermRewriteInference): Seq[InferenceRewriteSuggestion] = {
          val paths = replacementPossibilities.mapCollect(getRewritePath(termRewriteInference, _))
          if (paths.nonEmpty) {
            paths
              .groupBy(x => (x._1, x._2))
              .mapValues(_.map(_._3))
              .toSeq
              .sortBy(_._2.min)
              .map { case ((source, result), paths) =>
                InferenceRewriteSuggestion(
                  termRewriteInference.baseInference.summary,
                  termRewriteInference.extractionDefinition.serialized,
                  source,
                  result,
                  paths)
              }
          } else
            Nil
        }

        def getSuggestionsForInferenceBatch(rewriteInferences: Seq[TermRewriteInference]): Seq[InferenceRewriteSuggestion] = {
          rewriteInferences
            .flatMap(getSuggestionsForInference)
            .sortBy(_.source.complexity)(Ordering[(Int, Int)].reverse)
        }

        stepWithContext.provingContext.prospectiveTermRewriteInferences
          .filter(InferenceFilter(searchText).apply)
          .groupBy(_.lhs.structuralComplexity).toSeq
          .sortBy(_._1)(Ordering[Int].reverse)
          .iterator
          .map(_._2)
          .flatMap(getSuggestionsForInferenceBatch)
          .take(NumberOfSuggestionsToReturn)
          .toList
      }
    ).toResponseEntity
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
    bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath).flatMap(implicit stepWithContext =>
      for {
        expression <- Expression.parser.parseFromString(serializedExpression, "expression").recoverWithBadRequest
        equality <- stepWithContext.provingContext.equalityOption.orBadRequest("No equality found")
      } yield {
        val replacementPossibilities = getRewritePossibilities(expression, pathsAlreadyRewrittenText)
        val premisesWithReferencedLines = stepWithContext.stepProvingContext.allPremises.map(p => (p.statement, Some(p.referencedLine)))
        val extractedPremises = stepWithContext.stepProvingContext.knownStatementsFromPremises.map(_.statement).filter(s => !premisesWithReferencedLines.exists(_._1 == s)).map(_ -> None)
        (premisesWithReferencedLines ++ extractedPremises).mapCollect { case (statement, reference) =>
          for {
            (lhs, rhs) <- equality.unapply(statement)
            results = replacementPossibilities.filter(p => p.term == lhs.insertExternalParameters(p.unwrappers.depth)).map(_.path).map(PremiseRewritePath(_, rhs))
            if results.nonEmpty
          } yield PremiseSuggestion(statement, reference, results)
        }
      }
    ).toResponseEntity
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
      stepWithContext <- bookService.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
    } yield stepWithContext.stepProvingContext.allPremises).toResponseEntity
  }

  def getRewriteStepForInference[TExpression <: Expression with TypedExpression[TExpression] : RewriteMethods](
    inferenceId: String,
    baseTerm: Term,
    rewrite: RewriteRequest,
    unwrappers: Seq[Unwrapper],
    wrapperExpression: TExpression,
    direction: Direction,
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Term, Term, Seq[Step], Option[Inference.Summary], Option[Inference.Summary], Seq[Unwrapper], TExpression)] = {
    val extractionDefinition = rewrite.extractionDefinition.reverseIfNecessary(direction, equality)
    for {
      inferenceExtraction <- stepProvingContext.provingContext.findInferenceExtraction(inferenceId, extractionDefinition).orBadRequest(s"Could not find extraction")
      inference = inferenceExtraction.inference
      (lhs, rhs) <- equality.unapply(inferenceExtraction.conclusion).orBadRequest("Rewrite conclusion was not equality")
      (sourceTemplate, targetTemplate) = direction.swapSourceAndResult(lhs, rhs)
      substitutions <- sourceTemplate.calculateSubstitutions(baseTerm).orBadRequest("Could not find substitutions")
      (knownPremises, _) <- DerivationFinder.findDerivationsForStatementsBySubstituting(inferenceExtraction.premises, substitutions)
        .orBadRequest("Could not find premises")
      (removedUnwrappers, removedSource, removedPremises, removedWrapperExpression) = RewriteMethods[TExpression].removeUnwrappers(baseTerm, knownPremises.map(_.statement), wrapperExpression, unwrappers)
      removedUnwrappedStepContext = removedUnwrappers.enhanceStepContext(implicitly)
      finalSubstitutionsAfterSource <- sourceTemplate.calculateSubstitutions(removedSource)(removedUnwrappedStepContext).orBadRequest("Could not find substitutions")
      finalSubstitutionsAfterPremises <- inference.premises.zip(removedPremises).foldLeft(Try(finalSubstitutionsAfterSource)) { case (s, (ip, p)) => s.flatMap(ip.calculateSubstitutions(p, _)(removedUnwrappedStepContext).orBadRequest("Could not find substitutions"))}
      finalSubstitutions <- finalSubstitutionsAfterPremises.confirmTotality(inferenceExtraction.variableDefinitions).orBadRequest("Substitutions were not complete")
      rewrittenTerm <- targetTemplate.applySubstitutions(finalSubstitutions).orBadRequest("Could not apply substitutions to target")
      extractionStep <- ExtractionApplier.getInferenceExtractionStepWithoutPremises(inferenceExtraction, finalSubstitutions).orBadRequest("Could not apply extraction")
      elidedStep = Step.ElidedStep.ifNecessary((knownPremises.flatMap(_.derivation) :+ extractionStep).distinctBy(_.statement), inference).get
    } yield (removedSource, rewrittenTerm, Seq(elidedStep), Some(inference), None, removedUnwrappers, removedWrapperExpression)
  }

  def getRewriteStepForPremise[TExpression <: Expression with TypedExpression[TExpression] : RewriteMethods](
    serializedPremiseStatement: String,
    baseTerm: Term,
    rewrite: RewriteRequest,
    unwrappers: Seq[Unwrapper],
    wrapperExpression: TExpression,
    direction: Direction,
    equality: Equality)(
    implicit stepWithContext: StepWithContext
  ): Try[(Term, Term, Seq[Step], Option[Inference.Summary], Option[Inference.Summary], Seq[Unwrapper], TExpression)] = {
    for {
      premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest.map(_.insertExternalParameters(unwrappers.depth))
      (removedUnwrappers, removedSource, Seq(removedPremiseStatement), removedWrapperExpression) = RewriteMethods[TExpression].removeUnwrappers(baseTerm, Seq(premiseStatement), wrapperExpression, unwrappers)
      (premiseLhs, premiseRhs) <- equality.unapply(removedPremiseStatement).orBadRequest("Premise was not equality")
      _ <- (removedSource == premiseLhs).orBadRequest("Premise did not match term at path")
      statementToFind = (equality.apply _).tupled(direction.swapSourceAndResult(premiseLhs, premiseRhs))
      knownStatement <- stepWithContext.stepProvingContext.knownStatementsFromPremisesBySerializedStatement.get(statementToFind.serializedForHash).orBadRequest(s"Could not find premise ${statementToFind.toString}")
    } yield (premiseLhs, premiseRhs, knownStatement.derivation, None, knownStatement.derivation.flatMap(_.inferences).toSet.single.map(_.summary), removedUnwrappers, removedWrapperExpression)
  }

  def rewrite[TExpression <: Expression with TypedExpression[TExpression] : RewriteMethods, TStep](
    baseExpression: TExpression,
    rewriteList: Seq[Seq[RewriteRequest]],
    equality: Equality,
    direction: Direction)(
    applyRewrite: (Term, Term, Seq[Unwrapper], Wrapper[Term, TExpression], Seq[Step], Option[Inference.Summary], Option[Inference.Summary]) => Try[(TStep, Option[Inference.Summary], Wrapper[Term, TExpression])])(
    combine: (TExpression, TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => Try[TStep])(
    elide: (TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => Try[Option[Step]])(
    implicit stepWithContext: StepWithContext
  ): Try[(Option[Step], TExpression)] = {
    for {
      (newTarget, steps, inferences) <- rewriteList.foldLeft(Try((baseExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrites) =>
        for {
          (currentExpression, stepsSoFar, inferencesSoFar) <- trySoFar
          (newTarget, steps, inferences) <- direction.reverseIfNecessary(rewrites).foldLeft(Try((currentExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrite) =>
            for {
              (currentInnerExpression, stepsSoFar, inferencesSoFar) <- trySoFar
              rewritePossibilities = getRewritePossibilities(currentInnerExpression)
              RewritePossibility(baseTerm, function, _, _, baseUnwrappers, replacementStepContext) <- rewritePossibilities.find(_.path == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
              (term, rewrittenTerm, rewriteSteps, inferenceOption, fallbackInferenceOption, unwrappers, wrapperExpression) <- ((rewrite.inferenceId.map(getRewriteStepForInference(_, baseTerm, rewrite, baseUnwrappers, function, direction, equality)(implicitly, stepWithContext.stepProvingContext.withStepContext(replacementStepContext))) orElse
                rewrite.serializedPremiseStatement.map(getRewriteStepForPremise(_, baseTerm, rewrite, baseUnwrappers, function, direction, equality))) orBadRequest
                "Neither inference nor premise supplied").flatten
              wrapper = Wrapper.fromExpression(wrapperExpression)
              (source, result) = direction.swapSourceAndResult(term, rewrittenTerm)
              (step, resultInferenceOption, updatedWrapper) <- applyRewrite(source, result, unwrappers, wrapper, rewriteSteps, inferenceOption, fallbackInferenceOption)
            } yield (updatedWrapper(rewrittenTerm), stepsSoFar :+ step, inferencesSoFar :+ resultInferenceOption)
          }.map(_.map2(direction.reverseIfNecessary))
          step <- combine(currentExpression, newTarget, steps, inferences)
        } yield (newTarget, stepsSoFar :+ step , inferencesSoFar ++ inferences)
      }.map(_.map2(direction.reverseIfNecessary))
      stepOption <- elide(newTarget, steps, inferences)
    } yield (stepOption, newTarget)
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
    implicit stepWithContext: StepWithContext
  ): Try[(Step, Option[Inference.Summary], Wrapper[Term, Statement])] = {
    val substitutionStep = equality.substitution.assertionStep(source, result, wrapper)(unwrappers.enhanceStepContext(implicitly))
    val inference = rewriteInferenceOption.getOrElse(equality.substitution.inference.summary)
    val enhancedWrapper = unwrappers.enhanceWrapper(wrapper)
    val extractionStepOption = unwrappers.getTargetExtraction(wrapper(source)(unwrappers.enhanceStepContext(implicitly)))._2
    val rewriteStep = EqualityRewriter.rewriteElider(Some(inference))(steps :+ substitutionStep).get
    val extractionSteps = extractionStepOption.toSeq :+ rewriteStep
    val updatedSteps = unwrappers.rewrap(extractionSteps)
    val finalStep = if (enhancedWrapper != wrapper)
      Step.ElidedStep(updatedSteps, Some(inference), None)
    else
      Step.ElidedStep.ifNecessary(updatedSteps, inference).get
    Success((finalStep, Some(inference), enhancedWrapper))
  }

  def expandForRearrangement[TExpression <: Expression : RewriteMethods](
    expansion: Expansion[TExpression])(
    source: Term,
    result: Term,
    unwrappers: Seq[Unwrapper],
    wrapper: Wrapper[Term, TExpression],
    steps: Seq[Step],
    rewriteInferenceOption: Option[Inference.Summary],
    fallbackInferenceOption: Option[Inference.Summary])(
    implicit stepWithContext: StepWithContext
  ): Try[(RearrangementStep[TExpression], Option[Inference.Summary], Wrapper[Term, TExpression])] = {
    val expansionStepOption = expansion.assertionStepIfNecessary(source, result, wrapper)(unwrappers.enhanceStepContext(implicitly))
    val inferenceOption = rewriteInferenceOption orElse Some(expansion.inference.summary).filter(_ => expansionStepOption.nonEmpty) orElse fallbackInferenceOption
    for {
      (updatedSteps, updatedWrapper) <- RewriteMethods[TExpression].rewrapWithDistribution(unwrappers, expansion.resultJoiner, source, result, steps ++ expansionStepOption.toSeq, wrapper, inferenceOption)
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
    bookService.replaceStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { implicit stepWithContext =>
      for {
        equality <- stepWithContext.provingContext.equalityOption.orBadRequest("No equality found")
        (step, newTarget) <- rewrite(stepWithContext.step.statement, rewrites, equality, Direction.Reverse)(substituteForRewrite(equality)) {
          (_, _, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).orServerError("Failed to get elided step")
        } {
          (_, steps, inferences) => Success(EqualityRewriter.optionalRewriteElider(inferences)(steps))
        }
        targetStepOption = if (stepWithContext.stepProvingContext.allPremises.exists(_.statement == newTarget)) None else Some(Step.TargetStep(newTarget))
      } yield targetStepOption.toSeq ++ step.toSeq
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
    bookService.replaceStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { implicit stepWithContext =>
      import stepWithContext.step
      EqualityRewriter.rewrite(step.statement)(stepWithContext)
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
    InsertStepBeforeChain(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { implicit stepWithContext =>
      for {
        equality <- stepWithContext.provingContext.equalityOption.orBadRequest("No equality found")
        premiseStatement <- Statement.parser.parseFromString(premiseRewrite.serializedPremise, "premise").recoverWithBadRequest
        (newStepOption, _) <- rewrite(premiseStatement, premiseRewrite.rewrites, equality, Direction.Forward)(substituteForRewrite(equality)){
          (_, _, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).orServerError("Failed to get elided step")
        } {
          (_, steps, inferences) => Success(EqualityRewriter.optionalRewriteElider(inferences)(steps))
        }
      } yield newStepOption.toSeq
    }.toResponseEntity
  }

  def addRearrangementTransitivityForExpansionRewrite[TExpression <: Expression](
    joiner: BinaryJoiner[TExpression],
    direction: Direction)(
    sourceExpression: TExpression,
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepWithContext: StepWithContext
  ): Try[RearrangementStep[TExpression]] = {
    val (sourceTerm, targetTerm) = direction.swapSourceAndResult(sourceExpression, rewrittenExpression)
    for {
      transitivitySteps <- Transitivity.addToRearrangement(sourceTerm, joiner, rearrangementSteps).orBadRequest(s"Could not find transitivity for ${joiner.symbol}")
    } yield RearrangementStep(targetTerm, transitivitySteps, EqualityRewriter.optionalRewriteElider(inferences))
  }

  def addRearrangementTransitivityForSubstitutionRewrite[TExpression <: Expression](
    transitivity: Transitivity[TExpression],
    direction: Direction)(
    sourceExpression: TExpression,
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepWithContext: StepWithContext
  ): Try[RearrangementStep[TExpression]] = {
    val (sourceTerm, targetTerm) = direction.swapSourceAndResult(sourceExpression, rewrittenExpression)
    val transitivitySteps = transitivity.addToRearrangement(sourceTerm, rearrangementSteps)
    Success(RearrangementStep(targetTerm, transitivitySteps, EqualityRewriter.optionalRewriteElider(inferences)))
  }

  def addTransitivityForRewrite[TExpression <: Expression](
    sourceExpression: TExpression,
    joiner: BinaryJoiner[TExpression],
    direction: Direction)(
    rewrittenExpression: TExpression,
    rearrangementSteps: Seq[RearrangementStep[TExpression]],
    inferences: Seq[Option[Inference.Summary]])(
    implicit stepWithContext: StepWithContext
  ): Try[Option[Step]] = {
    for {
      transitivitySteps <- Transitivity.addToRearrangement(direction.getSource(sourceExpression, rewrittenExpression), joiner, rearrangementSteps).orBadRequest(s"Could not find transitivity for ${joiner.symbol}")
    } yield EqualityRewriter.optionalRewriteElider(inferences)(transitivitySteps)
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
      def createStepsWithExpansion[T <: Expression with TypedExpression[T] : ChainingMethods : RewriteMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T)(implicit  stepWithContext: StepWithContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.TargetStep])] = {
        for {
          equality <- stepWithContext.provingContext.equalityOption.orBadRequest("No equality found")
          expansion <- stepWithContext.provingContext.expansions.ofType[Expansion[T]]
            .find(e => e.sourceJoiner == equality.relation && e.resultJoiner == targetJoiner)
            .orBadRequest("No applicable expansion found")
          (sourceTerm, destinationTerm) = direction.swapSourceAndResult(targetLhs, targetRhs)
          (rewriteStep, intermediateTerm) <- rewrite(sourceTerm, rewrites, equality, direction)(
            expandForRearrangement(expansion))(
            addRearrangementTransitivityForExpansionRewrite(targetJoiner, direction))(
            addTransitivityForRewrite(sourceTerm, targetJoiner, direction))
          (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediateTerm, destinationTerm)
          (rewriteLhs, rewriteRhs) = direction.swapSourceAndResult(sourceTerm, intermediateTerm)
          rewriteStepDefinition = ChainingStepDefinition(rewriteLhs, rewriteRhs, targetJoiner, rewriteStep)
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetJoiner)
          (firstStep, secondStep) = direction.swapSourceAndResult(rewriteStepDefinition, targetStepDefinition)
        } yield (firstStep, secondStep, Nil)
      }
      def createStepsWithSubstitution(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.TargetStep])] = {
        for {
          equality <- stepWithContext.provingContext.equalityOption.orBadRequest("No equality found")
          (sourceTerm, destinationTerm) = direction.swapSourceAndResult(targetLhs, targetRhs)
          (rewriteStep, intermediateTerm) <- rewrite(sourceTerm, rewrites, equality, direction)(
            expandForRearrangement(equality.expansion))(
            addRearrangementTransitivityForSubstitutionRewrite(equality.transitivity, direction))(
            addTransitivityForRewrite(sourceTerm, targetRelation, direction))
          (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediateTerm, destinationTerm)
          (rewriteLhs, rewriteRhs) = direction.swapSourceAndResult(sourceTerm, intermediateTerm)
          rewriteStepDefinition = ChainingStepDefinition(rewriteLhs, rewriteRhs, equality.relation, rewriteStep)
          targetStepDefinition = ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetRelation)
          (firstStep, secondStep) = direction.swapSourceAndResult(rewriteStepDefinition, targetStepDefinition)
        } yield (firstStep, secondStep, Nil)
      }

      override def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.TargetStep])] = {
        createStepsWithExpansion(targetConnective, targetLhs, targetRhs)
      }

      override def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.TargetStep])] = {
        createStepsWithExpansion(targetRelation, targetLhs, targetRhs) orElse
          createStepsWithSubstitution(targetRelation, targetLhs, targetRhs)
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
  val logger: Logger = LoggerFactory.getLogger(StepRewriteController.getClass)

  case class InferenceRewriteSuggestion(inference: Inference.Summary, extractionDefinition: ExtractionDefinition.Serialized, source: Term, result: Term, paths: Seq[Seq[Int]])
  case class PremiseSuggestion(statement: Statement, reference: Option[PreviousLineReference], rewriteSuggestions: Seq[PremiseRewritePath])
  case class PremiseRewritePath(path: Seq[Int], result: Term)
}
