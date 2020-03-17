package net.prover.controllers

import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models.{PathData, PossibleConclusion, PossibleInference, StepDefinition}
import net.prover.model.ExpressionParsingContext.TermVariableValidator
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.SubstatementExtractor.VariableTracker
import net.prover.model.proof._
import net.prover.model.{ExpressionParsingContext, Inference, Substitutions}
import net.prover.util.Direction
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepChainingController @Autowired() (val bookService: BookService) extends BookModification with ChainingStepEditing with InferenceSearch {
  private def getSubstitutionsWithTermOrSubterm(source: Expression, result: Expression, baseSubstitutions: Substitutions.Possible = Substitutions.Possible.empty)(implicit substitutionContext: SubstitutionContext, stepContext: StepContext): Option[Substitutions.Possible] = {
    source.calculateSubstitutions(result, baseSubstitutions) orElse
      (result.getTerms().map(_._1).toSet diff result.asOptionalInstanceOf[Term].toSet).toSeq.mapCollect(source.calculateSubstitutions(_, baseSubstitutions)).single
  }

  private def suggestInferencesForChaining(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    searchText: String)(
    getSourceTerm: (BinaryJoiner[_ <: Expression], Statement, StepContext) => Option[Expression]
  ): ResponseEntity[_] = {
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      (targetSource, relation) <- stepProvingContext.provingContext.definedBinaryStatements.mapFind { relation =>
        getSourceTerm(relation, step.statement, stepProvingContext.stepContext).map(_ -> relation)
      }.orBadRequest("Target step is not a binary relation")
    } yield {
      import stepProvingContext._
      implicit val spc = stepProvingContext

      def getSubstitutions(extractionResult: Statement): Option[Substitutions.Possible] = {
        def asEquality: Option[Substitutions.Possible] = {
          for {
            equality <- provingContext.equalityOption
            conclusionSource <- getSourceTerm(equality.relation, extractionResult, stepContext)
            substitutions <- getSubstitutionsWithTermOrSubterm(conclusionSource, targetSource)
          } yield substitutions
        }
        def asMatchingRelation: Option[Substitutions.Possible] = {
          for {
            conclusionSource <- getSourceTerm(relation, extractionResult, stepContext)
            substitutions <- conclusionSource.calculateSubstitutions(targetSource)
          } yield substitutions
        }
        asEquality orElse asMatchingRelation
      }

      def getPossibleInference(inference: Inference): Option[PossibleInference] = {
        val possibleConclusions = provingContext.extractionOptionsByInferenceId(inference.id)
          .mapCollect(PossibleConclusion.fromExtractionOptionWithSubstitutions(_, getSubstitutions))
        if (possibleConclusions.nonEmpty) {
          Some(PossibleInference(inference.summary, None, Some(possibleConclusions)))
        } else {
          None
        }
      }

      filterInferences(provingContext.entryContext.allInferences, searchText)
        .sortBy(_.conclusion.structuralComplexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .mapCollect(getPossibleInference)
        .take(10)
        .toSeq
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestInferencesForChainingFromLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForChainingFromLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    suggestInferencesForChaining(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText)(_.unapply(_)(_).map(_._1))
  }

  @GetMapping(value = Array("/suggestInferencesForChainingFromRight"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForChainingFromRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    suggestInferencesForChaining(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText)(_.unapply(_)(_).map(_._2))
  }

  def suggestChainingFromPremise(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    serializedPremiseStatement: String,
    direction: Direction
  ): ResponseEntity[_] = {
    def getPremises[T <: Expression](joiner: BinaryJoiner[T], lhs: T, rhs: T, premise: Statement, baseSubstitutions: Substitutions.Possible)(implicit stepProvingContext: StepProvingContext): Try[Seq[PossibleConclusion]] = {
      Success(SubstatementExtractor.getExtractionOptions(premise)
        .flatMap(PossibleConclusion.fromExtractionOptionWithSubstitutions(_, conclusion => for {
          (conclusionLhs, conclusionRhs) <- joiner.unapply(conclusion)
          substitutions <- getSubstitutionsWithTermOrSubterm(direction.getSource(conclusionLhs, conclusionRhs), direction.getSource(lhs, rhs), baseSubstitutions)
        } yield substitutions)))
    }
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
      premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
      baseSubstitutions <- premise.statement.calculateSubstitutions(premise.statement)(stepProvingContext.stepContext).orBadRequest(s"Somehow failed to calculate base substitutions for premise '${premise.statement}'")
      result <- withRelation(step.statement, getPremises(_, _, _, premise.statement, baseSubstitutions)(stepProvingContext), getPremises(_, _, _, premise.statement, baseSubstitutions)(stepProvingContext))(stepProvingContext)
    } yield result).toResponseEntity
  }


  @GetMapping(value = Array("/suggestChainingFromPremiseLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestChainingFromPremiseLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("serializedPremiseStatement") serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    suggestChainingFromPremise(bookKey, chapterKey, theoremKey, proofIndex, stepPath, serializedPremiseStatement, Direction.Forward)
  }
  @GetMapping(value = Array("/suggestChainingFromPremiseRight"), produces = Array("application/json;charset=UTF-8"))
  def suggestChainingFromPremiseRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("serializedPremiseStatement") serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    suggestChainingFromPremise(bookKey, chapterKey, theoremKey, proofIndex, stepPath, serializedPremiseStatement, Direction.Reverse)
  }

  private def insertChainingAssertion(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    @RequestBody definition: StepDefinition,
    direction: Direction
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingSteps {
      override def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        createSteps(targetConnective, targetLhs, targetRhs, (_, _, relation, lhs, rhs) => Success(ChainingStepDefinition(lhs, rhs, relation, None)))
      }
      override def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        def getExpansion(conclusionSource: Term, targetSource: Term, conclusionRelation: BinaryJoiner[Term], conclusionLhs: Term, conclusionRhs: Term) = for {
          wrapper <- targetSource.getTerms().filter(_._1 == conclusionSource).map(_._2).map(Wrapper.fromExpression).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
          step <-
            if (wrapper.isIdentity)
              Success(ChainingStepDefinition(conclusionLhs, conclusionRhs, conclusionRelation, None))
            else
              for {
                expansionDefinition <- stepProvingContext.provingContext.expansions.ofType[RelationExpansion]
                  .find(e => e.sourceJoiner == conclusionRelation && e.resultJoiner == targetRelation)
                  .orBadRequest("Could not find expansion")
                step = expansionDefinition.assertionStep(conclusionLhs, conclusionRhs, wrapper)
              } yield ChainingStepDefinition(wrapper(conclusionLhs), wrapper(conclusionRhs), targetRelation, Some(step))
        } yield step
        createSteps(targetRelation, targetLhs, targetRhs, getExpansion)
      }

      def createSteps[T <: Expression : ChainingMethods](
        targetRelation: BinaryJoiner[T],
        targetLhs: T,
        targetRhs: T,
        handle: (T, T, BinaryJoiner[T], T, T) => Try[ChainingStepDefinition[T]])(
        implicit stepProvingContext: StepProvingContext
      ): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
        val (targetSource, targetResult) = direction.swapSourceAndResult(targetLhs, targetRhs)
        def getResult(applyExtractions: (Seq[Inference.Summary], Substitutions, ExpressionParsingContext => Try[Option[Statement]]) => Try[(ExtractionApplication, Seq[Step.Assertion], Seq[Step], Seq[Step.Target], Seq[Step] => Step.Elided)]) = {
          for {
            extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
            substitutions <- definition.substitutions.parse()
            getIntendedConclusion = (expressionParsingContext: ExpressionParsingContext) => definition.serializedIntendedConclusionStatement match {
              case Some(serializedIntendedConclusionStatement) =>
                for {
                  conclusionStatement <- Statement.parser(expressionParsingContext).parseFromString(serializedIntendedConclusionStatement, "intended conclusion").recoverWithBadRequest
                  substitutedConclusionStatement <- conclusionStatement.applySubstitutions(substitutions).orBadRequest("Could not apply substitutions to intended conclusion")
                } yield Some(substitutedConclusionStatement)
              case None =>
                Success(None)
            }
            (ExtractionApplication(conclusion, _, extractionSteps, extractionPremises, extractionTargets), additionalAssertions, additionalPremises, additionalTargets, elider) <- applyExtractions(extractionInferences, substitutions, getIntendedConclusion)
            (conclusionRelation, conclusionLhs, conclusionRhs) <- ChainingMethods.getRelation[T](conclusion).orBadRequest("Conclusion was not binary statement")
            conclusionSource = direction.getSource(conclusionLhs, conclusionRhs)
            rewriteChainingDefinition <- handle(conclusionSource, targetSource, conclusionRelation, conclusionLhs, conclusionRhs)
            extractionStep = Step.Elided.ifNecessary(additionalAssertions ++ extractionSteps, elider)
            finalStep = Step.Elided.ifNecessary((additionalPremises ++ extractionPremises ++ extractionStep.toSeq) ++ rewriteChainingDefinition.step.toSeq, elider)
            intermediate = direction.getResult(rewriteChainingDefinition.lhs, rewriteChainingDefinition.rhs)
            updatedChainingDefinition = rewriteChainingDefinition.copy(step = finalStep)
            (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediate, targetResult)
            newTarget = targetRelation(targetLhs, targetRhs)
            newTargetStepOption = if (stepProvingContext.allPremises.exists(_.statement == newTarget)) None else Some(Step.Target(newTarget))
            (firstDefinition, secondDefinition) = direction.swapSourceAndResult(updatedChainingDefinition, ChainingStepDefinition(targetLhs, targetRhs, targetRelation, newTargetStepOption))
          } yield (firstDefinition, secondDefinition, additionalTargets ++ extractionTargets)
        }

        def fromInference(inferenceId: String) = {
          getResult { (extractionInferences, substitutions, getIntendedTarget) =>
            for {
              inference <- findInference(inferenceId)
              epc = ExpressionParsingContext(implicitly, TermVariableValidator.LimitedList(VariableTracker.fromInference(inference).baseVariableNames ++ definition.additionalVariableNames.toSeq.flatten), Nil)
              intendedConclusionOption <- getIntendedTarget(epc)
              intendedPremiseStatementsOption <- definition.parseIntendedPremiseStatements(epc)
              (inferenceToApply, intendedExtractionPremisesOption) <- intendedPremiseStatementsOption match {
                case Some(intendedPremiseStatements) =>
                  for {
                    (intendedInferencePremises, intendedExtractionPremises) <- intendedPremiseStatements.takeAndRemainingIfValid(inference.premises.length).orBadRequest("Not enough intended premises statements provided")
                    _ <- (intendedInferencePremises == inference.premises).orBadRequest("Intended premises did not match inference premises")
                  } yield (inference.copy(premises = intendedInferencePremises), Some(intendedExtractionPremises))
                case None =>
                  Success((inference, None))
              }
              (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inferenceToApply, substitutions).orBadRequest("Could not apply substitutions to inference")
              extractionApplication <- ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions, intendedExtractionPremisesOption, intendedConclusionOption, PremiseFinder.findPremiseStepsOrTargets _)
            } yield (extractionApplication, Seq(mainAssertion), mainPremises, mainTargets, Step.Elided.forInference(inference))
          }
        }
        def fromPremise(serializedPremiseStatement: String) = {
          getResult { (extractionInferences, substitutions, getIntendedConclusion) =>
            for {
              premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
              premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
              epc = ExpressionParsingContext(
                implicitly,
                TermVariableValidator.LimitedList(VariableTracker.fromStepContext.baseVariableNames ++ definition.additionalVariableNames.toSeq.flatten),
                stepProvingContext.stepContext.boundVariableLists.map(_.zipWithIndex))
              intendedConclusionOption <- getIntendedConclusion(epc)
              intendedPremiseStatementsOption <- definition.parseIntendedPremiseStatements(epc)
              extractionApplication <- ExtractionHelper.applyExtractions(premise, extractionInferences, substitutions, intendedPremiseStatementsOption, intendedConclusionOption, PremiseFinder.findPremiseStepsOrTargets _)
            } yield (extractionApplication, Nil, Nil, Nil, Step.Elided.forDescription("Extracted"))
          }
        }
        definition.getFromInferenceOrPremise(fromInference, fromPremise)
      }
    })
  }

  @PostMapping(value = Array("/chainingFromLeft"))
  def addChainingFromLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    insertChainingAssertion(bookKey, chapterKey, theoremKey, proofIndex, stepPath, definition, Direction.Forward)
  }

  @PostMapping(value = Array("/chainingFromRight"))
  def addChainingFromRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    insertChainingAssertion(bookKey, chapterKey, theoremKey, proofIndex, stepPath, definition, Direction.Reverse)
  }

  @PostMapping(value = Array("/transitiveTarget"))
  def addTransitiveTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedExpression: String
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingStepsCommon {
      def createSteps[T <: Expression : ChainingMethods](joiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          intermediateExpression <- ChainingMethods.parser.parseFromString(serializedExpression, "target expression").recoverWithBadRequest
          firstStep = ChainingStepDefinition.forTarget(targetLhs, intermediateExpression, joiner)
          secondStep = ChainingStepDefinition.forTarget(intermediateExpression, targetRhs, joiner)
        } yield (firstStep, secondStep, Nil)
      }
    })
  }
}
