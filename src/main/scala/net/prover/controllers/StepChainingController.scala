package net.prover.controllers

import net.prover.controllers.StepChainingController.ChainedTargetDefinition
import net.prover.controllers.models._
import net.prover.entries.StepWithContext
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof._
import net.prover.model.{ExpressionParsingContext, Inference, ProvingContext, SeqOps, Substitutions}
import net.prover.proving.FindInference
import net.prover.proving.extraction.{ExtractionApplier, ExtractionCalculator}
import net.prover.proving.suggestions.SuggestInferences
import net.prover.util.Direction
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.reflect.ClassTag
import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepChainingController @Autowired() (val bookService: BookService) extends ChainingStepEditing {
  private def getSubstitutionsWithTermOrSubterm(source: Expression, result: Expression, baseSubstitutions: Substitutions.Possible)(implicit stepWithContext: StepWithContext): Option[Substitutions.Possible] = {
    source.calculateSubstitutions(result, baseSubstitutions) orElse
      (result.getTerms().map(_._1).toSet diff result.asOptionalInstanceOf[Term].toSet).toSeq.mapCollect(source.calculateSubstitutions(_, baseSubstitutions)).single
  }

  private def suggestInferencesForChaining(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    searchText: String,
    direction: Direction
  ): ResponseEntity[_] = {

    def withJoiner[T <: Expression, TJoiner <: BinaryJoiner[T] : ClassTag](targetConnective: BinaryJoiner[T], targetLhs: T, targetRhs: T)(implicit stepWithContext: StepWithContext): Try[Seq[PossibleInferenceWithTargets]] = {
      val targetSource = direction.getSource(targetLhs, targetRhs)
      def getSubstitutions(extractionResult: Statement): Option[Substitutions.Possible] = {
        for {
          (conclusionConnective, conclusionSource) <- stepWithContext.provingContext.definedBinaryJoiners.ofType[TJoiner].mapFind(j => j.unapply(extractionResult).map { case (l, r) => (j, direction.getSource(l, r))} )
          if stepWithContext.provingContext.transitivities.exists(t => direction.getSource(t.firstPremiseJoiner, t.secondPremiseJoiner) == conclusionConnective && t.resultJoiner == targetConnective)
          substitutions <- conclusionSource.calculateSubstitutions(targetSource)
        } yield substitutions
      }
      def getPossibleInference(inference: Inference): Option[PossibleInferenceWithTargets] = {
        val possibleConclusions = stepWithContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
          .mapCollect(PossibleConclusionWithPremises.fromExtractionWithSubstitutions(_, getSubstitutions))
        if (possibleConclusions.nonEmpty) {
          Some(PossibleInferenceWithTargets(inference.summary, Seq(PossibleTarget(targetSource, Nil, Nil, possibleConclusions))))
        } else {
          None
        }
      }
      def getConclusionComplexity(possibleConclusion: PossibleConclusion): Int = {
        stepWithContext.provingContext.definedBinaryRelations.ofType[TJoiner]
          .mapFind(j => j.unapply(possibleConclusion.conclusion).map { case (l, r) => direction.getSource(l, r).structuralComplexity })
          .getOrElse(0)
      }
      Success(SuggestInferences(searchText, getPossibleInference, getConclusionComplexity))
    }
    bookService.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      .flatMap(implicit stepWithContext => {
        withRelation(stepWithContext.step.statement, withJoiner[Statement, BinaryConnective], withJoiner[Term, BinaryRelation])
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
    suggestInferencesForChaining(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText, Direction.Forward)
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
    suggestInferencesForChaining(bookKey, chapterKey, theoremKey, proofIndex, stepPath, searchText, Direction.Reverse)
  }

  private def suggestChainingFromPremise(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    serializedPremiseStatement: String,
    direction: Direction
  ): ResponseEntity[_] = {
    def getPremises[T <: Expression](joiner: BinaryJoiner[T], lhs: T, rhs: T, premise: Statement, baseSubstitutions: Substitutions.Possible)(implicit stepWithContext: StepWithContext): Try[Seq[PossibleConclusionWithPremises]] = {
      Success(ExtractionCalculator.getPremiseExtractions(premise)
        .flatMap(PossibleConclusionWithPremises.fromExtractionWithSubstitutions(_, conclusion => for {
          (conclusionLhs, conclusionRhs) <- joiner.unapply(conclusion)
          substitutions <- getSubstitutionsWithTermOrSubterm(direction.getSource(conclusionLhs, conclusionRhs), direction.getSource(lhs, rhs), baseSubstitutions)
        } yield substitutions)))
    }
    bookService.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath).flatMap(implicit stepWithContext =>
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
        premise <- stepWithContext.stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        baseSubstitutions <- premise.statement.calculateSubstitutions(premise.statement).orBadRequest(s"Somehow failed to calculate base substitutions for premise '${premise.statement}'")
        result <- withRelation(stepWithContext.step.statement, getPremises(_, _, _, premise.statement, baseSubstitutions), getPremises(_, _, _, premise.statement, baseSubstitutions))
      } yield result
    ).toResponseEntity
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
    @RequestBody stepDefinition: StepDefinition,
    direction: Direction
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingSteps {
      override def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.TargetStep])] = {
        createSteps(targetConnective, targetLhs, targetRhs, (_, _, relation, lhs, rhs) => Success(ChainingStepDefinition(lhs, rhs, relation, None)))
      }
      override def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.TargetStep])] = {
        def getExpansion(conclusionSource: Term, targetSource: Term, conclusionRelation: BinaryJoiner[Term], conclusionLhs: Term, conclusionRhs: Term) = for {
          wrapper <- targetSource.getTerms().filter(_._1 == conclusionSource).map(_._2).map(Wrapper.fromExpression).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
          step <-
            if (wrapper.isIdentity)
              Success(ChainingStepDefinition(conclusionLhs, conclusionRhs, conclusionRelation, None))
            else
              for {
                expansionDefinition <- stepWithContext.provingContext.expansions.ofType[RelationExpansion]
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
        implicit stepWithContext: StepWithContext
      ): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.TargetStep])] = {
        val (targetSource, targetResult) = direction.swapSourceAndResult(targetLhs, targetRhs)
        def getResult(
          applyExtractions: ((ExpressionParsingContext, Substitutions) => Try[Option[Statement]]) => Try[(Statement, Option[Step], Seq[Step.TargetStep])]
        ): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.TargetStep])] = {
          def getIntendedConclusion(expressionParsingContext: ExpressionParsingContext, substitutions: Substitutions) = stepDefinition.serializedIntendedConclusionStatement match {
            case Some(serializedIntendedConclusionStatement) =>
              for {
                conclusionStatement <- Statement.parser(expressionParsingContext).parseFromString(serializedIntendedConclusionStatement, "intended conclusion").recoverWithBadRequest
                substitutedConclusionStatement <- conclusionStatement.applySubstitutions(substitutions).orBadRequest("Could not apply substitutions to intended conclusion")
              } yield Some(substitutedConclusionStatement)
            case None =>
              Success(None)
          }
          for {
            (extractionResult, extractionStep, targets) <- applyExtractions(getIntendedConclusion)
            (conclusionRelation, conclusionLhs, conclusionRhs) <- ChainingMethods.getJoiner[T](extractionResult).orBadRequest("Conclusion was not binary statement")
            conclusionSource = direction.getSource(conclusionLhs, conclusionRhs)
            rewriteChainingDefinition <- handle(conclusionSource, targetSource, conclusionRelation, conclusionLhs, conclusionRhs)
            intermediate = direction.getResult(rewriteChainingDefinition.lhs, rewriteChainingDefinition.rhs)
            updatedChainingDefinition = rewriteChainingDefinition.copy(step = extractionStep)
            (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediate, targetResult)
            (firstDefinition, secondDefinition) = direction.swapSourceAndResult(updatedChainingDefinition, ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetRelation))
          } yield (firstDefinition, secondDefinition, targets)
        }

        def fromInference(inferenceId: String): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.TargetStep])] = {
          getResult { getIntendedTarget =>
            for {
              inferenceExtraction <- stepWithContext.provingContext.findInferenceExtraction(inferenceId, stepDefinition.extractionDefinition).orBadRequest("Could not find inference extraction")
              inference = inferenceExtraction.inference
              epc = ExpressionParsingContext.withDefinitions(inferenceExtraction.variableDefinitions)
              substitutions <- stepDefinition.substitutions.parse(inferenceExtraction.variableDefinitions)
              intendedConclusionOption <- getIntendedTarget(epc, substitutions)
              intendedPremiseStatementsOption <- stepDefinition.parseIntendedPremiseStatements(epc)
              (inferenceToApply, intendedExtractionPremisesOption) <- intendedPremiseStatementsOption match {
                case Some(intendedPremiseStatements) =>
                  for {
                    (intendedInferencePremises, intendedExtractionPremises) <- intendedPremiseStatements.takeAndRemainingIfValid(inference.premises.length).orBadRequest("Not enough intended premises statements provided")
                    substitutedIntendedExtractionPremises <- intendedExtractionPremises.map(_.applySubstitutions(substitutions)).traverseOption.orBadRequest("Could not apply substitutions to extraction premises")
                    _ <- (intendedInferencePremises == inference.premises).orBadRequest("Intended premises did not match inference premises")
                  } yield (inference.copy(premises = intendedInferencePremises), Some(substitutedIntendedExtractionPremises))
                case None =>
                  Success((inference, None))
              }
              (inferenceExtractionStep, targets) <- ExtractionApplier.getInferenceExtractionStepWithPremises(inferenceExtraction, substitutions, Nil, intendedExtractionPremisesOption, intendedConclusionOption)
            } yield (inferenceExtractionStep.statement, Some(inferenceExtractionStep), targets)
          }
        }
        def fromPremise(serializedPremiseStatement: String): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.TargetStep])] = {
          getResult { getIntendedConclusion =>
            for {
              premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
              extraction <- ExtractionCalculator.getPremiseExtractions(premiseStatement).find(_.extractionDefinition.matches(stepDefinition.extractionDefinition)).orBadRequest("Could not find extraction with given inferences")
              premise <- stepWithContext.stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
              epc = ExpressionParsingContext.withDefinitions(extraction.variableDefinitions)
              substitutions <- stepDefinition.substitutions.parse(extraction.variableDefinitions)
              intendedConclusionOption <- getIntendedConclusion(epc, substitutions)
              intendedPremiseStatementsOption <- stepDefinition.parseIntendedPremiseStatements(epc)
              substitutedIntendedPremiseStatementsOption <- intendedPremiseStatementsOption.map(_.map(_.applySubstitutions(substitutions)).traverseOption.orBadRequest("Could not apply substitutions to extraction premises")).swap
              (result, step, targets)  <- ExtractionApplier.getPremiseExtractionStepWithPremises(premise, extraction, substitutions, substitutedIntendedPremiseStatementsOption, intendedConclusionOption)
            } yield (result, step, targets)
          }
        }
        stepDefinition.getFromInferenceOrPremise(fromInference, fromPremise)
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

  @GetMapping(value = Array("/chainedTargetJoiners"))
  def getChainedTargetJoiners(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    def forConnective(connective: BinaryConnective)(implicit provingContext: ProvingContext): Try[Seq[(String, String)]] = {
      Success(provingContext.transitivities.filter(_.resultJoiner == connective).map(t => (t.firstPremiseJoiner.symbol, t.secondPremiseJoiner.symbol)))
    }
    def forRelation(relation: BinaryRelation)(implicit provingContext: ProvingContext): Try[Seq[(String, String)]] = {
      Success(
        provingContext.equalityOption.filter(_.relation != relation).map { e => Seq((e.relation.symbol, relation.symbol), (relation.symbol, e.relation.symbol))}.getOrElse(Nil) ++
          provingContext.transitivities.filter(_.resultJoiner == relation).map(t => (t.firstPremiseJoiner.symbol, t.secondPremiseJoiner.symbol)))
    }

    bookService.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepPath).flatMap(implicit stepWithContext =>
      for {
        result <- withRelation(stepWithContext.step.statement, (c, _, _) => forConnective(c), (r, _, _) => forRelation(r))
      } yield result
    ).toResponseEntity
  }

  @PostMapping(value = Array("/chainedTarget"))
  def addChainedTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: ChainedTargetDefinition
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateChainingSteps {
      def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.TargetStep])] = {
        for {
          intermediateStatement <- Statement.parser.parseFromString(definition.serializedExpression, "target expression").recoverWithBadRequest
          leftJoiner <- stepWithContext.provingContext.definedBinaryConnectives.find(_.symbol == definition.leftJoiner).orBadRequest(s"Could not find connective with symbol ${definition.leftJoiner}")
          rightJoiner <- stepWithContext.provingContext.definedBinaryConnectives.find(_.symbol == definition.rightJoiner).orBadRequest(s"Could not find connective with symbol ${definition.rightJoiner}")
          firstStep = ChainingStepDefinition.forTarget(targetLhs, intermediateStatement, leftJoiner)
          secondStep = ChainingStepDefinition.forTarget(intermediateStatement, targetRhs, rightJoiner)
        } yield (firstStep, secondStep, Nil)
      }
      def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.TargetStep])] = {
        for {
          intermediateTerm <- Term.parser.parseFromString(definition.serializedExpression, "target expression").recoverWithBadRequest
          leftJoiner <- stepWithContext.provingContext.definedBinaryRelations.find(_.symbol == definition.leftJoiner).orBadRequest(s"Could not find relation with symbol ${definition.leftJoiner}")
          rightJoiner <- stepWithContext.provingContext.definedBinaryRelations.find(_.symbol == definition.rightJoiner).orBadRequest(s"Could not find relation with symbol ${definition.rightJoiner}")
          firstStep = ChainingStepDefinition.forTarget(targetLhs, intermediateTerm, leftJoiner)
          secondStep = ChainingStepDefinition.forTarget(intermediateTerm, targetRhs, rightJoiner)
        } yield (firstStep, secondStep, Nil)
      }
    })
  }
}

object StepChainingController {
  case class ChainedTargetDefinition(
    serializedExpression: String,
    leftJoiner: String,
    rightJoiner: String)
}
