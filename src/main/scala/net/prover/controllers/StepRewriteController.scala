package net.prover.controllers

import net.prover.controllers.StepRewriteController._
import net.prover.controllers.models._
import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions._
import net.prover.model.proof.EqualityRewriter.{RewriteMethods, RewritePossibility}
import net.prover.model.proof._
import net.prover.util.Direction
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch with ChainingStepEditing {

  private def getRewritePossibilities[T <: Expression : RewriteMethods](expression: T)(implicit stepProvingContext: StepProvingContext): Seq[RewritePossibility[T]] = {
    RewriteMethods[T].getRewritePossibilitiesFromOuterExpression(expression, Nil, Nil)
  }

  private def getRewritePossibilities(expression: Expression, pathsAlreadyRewrittenText: String)(implicit stepProvingContext: StepProvingContext): Seq[RewritePossibility[_ <: Expression]] = {
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

  implicit val seqOrdering: Ordering[Seq[Int]] = new Ordering[Seq[Int]] {
    override def compare(x: Seq[Int], y: Seq[Int]): Int = {
      val res0 = Ordering[Int].compare(x.length, y.length)
      if (res0 != 0)
        res0
      else
        Ordering.Implicits.seqDerivedOrdering[Seq, Int].compare(x, y)
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

      val replacementPossibilities = getRewritePossibilities(expression, pathsAlreadyRewrittenText)

      case class InferenceRewriteSuggestionWithMaximumMatchingComplexity(inferenceRewriteSuggestion: InferenceRewriteSuggestion, maximumMatchingComplexity: Int)

      def getRewritePath(termRewriteInference: TermRewriteInference, replacementPossibility: RewritePossibility[_ <: Expression]): Option[(Term, Term, Seq[Int])] = {
        import replacementPossibility._
        for {
          substitutionsAfterLhs <- termRewriteInference.lhs.calculateSubstitutions(term)(SubstitutionContext.withExtraParameters(unwrappers.depth))
          (_, substitutionsAfterPremises) <- PremiseFinder.findDerivationsForStatementsBySubstituting(termRewriteInference.extractionOption.premises, substitutionsAfterLhs)(StepProvingContext.updateStepContext(unwrappers.enhanceContext))
          result <- termRewriteInference.rhs.applySubstitutions(substitutionsAfterPremises.stripApplications())(SubstitutionContext.withExtraParameters(unwrappers.depth)).map(_.insertExternalParameters(depth))
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
                termRewriteInference.inferenceSummary,
                termRewriteInference.extractionOption.extractionInferences.map(_.id),
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

      val filter = inferenceFilter(searchText)
      stepProvingContext.provingContext.prospectiveTermRewriteInferences
        .filter { case TermRewriteInference(i, _, _, _) => filter(i) }
        .groupBy(_.lhs.structuralComplexity).toSeq
        .sortBy(_._1)(Ordering[Int].reverse)
        .iterator
        .map(_._2)
        .flatMap(getSuggestionsForInferenceBatch)
        .take(NumberOfSuggestionsToReturn)
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
      val replacementPossibilities = getRewritePossibilities(expression, pathsAlreadyRewrittenText)
      val premisesWithReferencedLines = stepProvingContext.allPremises.map(p => (p.statement, Some(p.referencedLine)))
      val extractedPremises = stepProvingContext.allPremiseExtractions.map(_.statement).filter(s => !premisesWithReferencedLines.exists(_._1 == s)).map(_ -> None)
      (premisesWithReferencedLines ++ extractedPremises).mapCollect { case (statement, reference) =>
        for {
          (lhs, rhs) <- equality.unapply(statement)
          results = replacementPossibilities.filter(p => p.term == lhs.insertExternalParameters(p.unwrappers.depth)).map(_.path).map(PremiseRewritePath(_, rhs))
          if results.nonEmpty
        } yield PremiseSuggestion(statement, reference, results)
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
    for {
      inference <- findInference(inferenceId)
      extractionInferenceIds = if (!direction.isReversed) {
        rewrite.extractionInferenceIds
      } else if (rewrite.extractionInferenceIds.lastOption.contains(equality.reversal.inference.id)) {
        rewrite.extractionInferenceIds.init
      } else {
        rewrite.extractionInferenceIds :+ equality.reversal.inference.id
      }
      extractionOption <- stepProvingContext.provingContext.extractionOptionsByInferenceId(inference.id).find(_.extractionInferences.map(_.id) == extractionInferenceIds).orBadRequest(s"Could not find extraction for inference ${inference.id}")
      (lhs, rhs) <- equality.unapply(extractionOption.conclusion).orBadRequest("Rewrite conclusion was not equality")
      (sourceTemplate, targetTemplate) = direction.swapSourceAndResult(lhs, rhs)
      substitutions <- sourceTemplate.calculateSubstitutions(baseTerm).orBadRequest("Could not find substitutions")
      (knownPremises, _) <- PremiseFinder.findDerivationsForStatementsBySubstituting(extractionOption.premises, substitutions)
        .orBadRequest("Could not find premises")
      (removedUnwrappers, removedSource, removedPremises, removedWrapperExpression) = RewriteMethods[TExpression].removeUnwrappers(baseTerm, knownPremises.map(_.statement), wrapperExpression, unwrappers)
      removedUnwrappedStepContext = removedUnwrappers.enhanceContext(implicitly)
      finalSubstitutionsAfterSource <- sourceTemplate.calculateSubstitutions(removedSource)(removedUnwrappedStepContext).orBadRequest("Could not find substitutions")
      finalSubstitutionsAfterPremises <- inference.premises.zip(removedPremises).foldLeft(Try(finalSubstitutionsAfterSource)) { case (s, (ip, p)) => s.flatMap(ip.calculateSubstitutions(p, _)(removedUnwrappedStepContext).orBadRequest("Could not find substitutions"))}
      finalSubstitutions <- finalSubstitutionsAfterPremises.confirmTotality.orBadRequest("Substitutions were not complete")
      rewrittenTerm <- targetTemplate.applySubstitutions(finalSubstitutions).orBadRequest("Could not apply substitutions to target")
      extractionStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, finalSubstitutions, extractionOption).orBadRequest("Could not apply extraction")
      elidedStep = Step.Elided.ifNecessary((knownPremises.flatMap(_.derivation) :+ extractionStep).deduplicate.steps, inference).get
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
    implicit stepProvingContext: StepProvingContext
  ): Try[(Term, Term, Seq[Step], Option[Inference.Summary], Option[Inference.Summary], Seq[Unwrapper], TExpression)] = {
    for {
      premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest.map(_.insertExternalParameters(unwrappers.depth))
      (removedUnwrappers, removedSource, Seq(removedPremiseStatement), removedWrapperExpression) = RewriteMethods[TExpression].removeUnwrappers(baseTerm, Seq(premiseStatement), wrapperExpression, unwrappers)
      (premiseLhs, premiseRhs) <- equality.unapply(removedPremiseStatement).orBadRequest("Premise was not equality")
      _ <- (removedSource == premiseLhs).orBadRequest("Premise did not match term at path")
      statementToFind = (equality.apply _).tupled(direction.swapSourceAndResult(premiseLhs, premiseRhs))
      knownStatement <- stepProvingContext.knownStatementsFromPremisesBySerializedStatement.get(statementToFind.serialized).orBadRequest(s"Could not find premise ${statementToFind.toString}")
    } yield (premiseLhs, premiseRhs, knownStatement.derivation.steps, None, knownStatement.derivation.inferences.distinct.single.map(_.summary), removedUnwrappers, removedWrapperExpression)
  }

  def rewrite[TExpression <: Expression with TypedExpression[TExpression] : RewriteMethods, TStep](
    baseExpression: TExpression,
    rewriteList: Seq[Seq[RewriteRequest]],
    equality: Equality,
    direction: Direction)(
    applyRewrite: (Term, Term, Seq[Unwrapper], Wrapper[Term, TExpression], Seq[Step], Option[Inference.Summary], Option[Inference.Summary]) => Try[(TStep, Option[Inference.Summary], Wrapper[Term, TExpression])])(
    combine: (TExpression, TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => Try[TStep])(
    elide: (TExpression, Seq[TStep], Seq[Option[Inference.Summary]]) => Try[Option[Step]])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Option[Step], TExpression)] = {
    for {
      (newTarget, steps, inferences) <- rewriteList.foldLeft(Try((baseExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrites) =>
        for {
          (currentExpression, stepsSoFar, inferencesSoFar) <- trySoFar
          (newTarget, steps, inferences) <- direction.reverseIfNecessary(rewrites).foldLeft(Try((currentExpression, Seq.empty[TStep], Seq.empty[Option[Inference.Summary]]))) { case (trySoFar, rewrite) =>
            for {
              (currentInnerExpression, stepsSoFar, inferencesSoFar) <- trySoFar
              rewritePossibilities = getRewritePossibilities(currentInnerExpression)
              RewritePossibility(baseTerm, function, _, _, baseUnwrappers, replacementStepProvingContext) <- rewritePossibilities.find(_.path == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
              (term, rewrittenTerm, rewriteSteps, inferenceOption, fallbackInferenceOption, unwrappers, wrapperExpression) <- ((rewrite.inferenceId.map(getRewriteStepForInference(_, baseTerm, rewrite, baseUnwrappers, function, direction, equality)(implicitly, replacementStepProvingContext)) orElse
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

  def expandForRearrangement[TExpression <: Expression : RewriteMethods](
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
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        (step, newTarget) <- rewrite(step.statement, rewrites, equality, Direction.Reverse)(substituteForRewrite(equality)) {
          (_, _, steps, inferences) => EqualityRewriter.optionalRewriteElider(inferences)(steps).orServerError("Failed to get elided step")
        } {
          (_, steps, inferences) => Success(EqualityRewriter.optionalRewriteElider(inferences)(steps))
        }
        targetStepOption = if (stepProvingContext.allPremises.exists(_.statement == newTarget)) None else Some(Step.Target(newTarget))
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
    addBeforeTransitivity[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { stepProvingContext =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
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
    implicit stepProvingContext: StepProvingContext
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
    implicit stepProvingContext: StepProvingContext
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
    implicit stepProvingContext: StepProvingContext
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
      def createStepsWithExpansion[T <: Expression with TypedExpression[T] : ChainingMethods : RewriteMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
          expansion <- stepProvingContext.provingContext.expansions.ofType[Expansion[T]]
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
      def createStepsWithSubstitution(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
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
  val logger: Logger = LoggerFactory.getLogger(StepRewriteController.getClass)

  case class InferenceRewriteSuggestion(inference: Inference.Summary, extractionInferenceIds: Seq[String], source: Term, result: Term, paths: Seq[Seq[Int]])
  case class PremiseSuggestion(statement: Statement, reference: Option[PreviousLineReference], rewriteSuggestions: Seq[PremiseRewritePath])
  case class PremiseRewritePath(path: Seq[Int], result: Term)
}
