package net.prover.controllers

import net.prover._
import net.prover.chaining.{ChainingMethods, ChainingStepEditing}
import net.prover.controllers.StepChainingController.ChainedTargetDefinition
import net.prover.controllers.models._
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof._
import net.prover.model.{ExpressionParsingContext, Inference, Substitutions}
import net.prover.extensions.ExpressionExtensions._
import net.prover.structure.BookService
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator
import net.prover.util.Direction
import net.prover.utilities.complexity.ComplexityCalculator
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.reflect.ClassTag
import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepChainingController @Autowired() (val bookService: BookService) extends BookModification with ChainingStepEditing with InferenceSearch {
  private def getSubstitutionsWithTermOrSubterm(source: Expression, result: Expression, baseSubstitutions: PossibleSubstitutions)(implicit substitutionContext: SubstitutionContext, stepContext: StepContext): Option[PossibleSubstitutions] = {
    PossibleSubstitutionCalculator.calculatePossibleSubstitutions(source, result, baseSubstitutions) orElse
      (result.getTerms().map(_._1).toSet diff result.asOptionalInstanceOf[Term].toSet).toSeq.mapCollect(PossibleSubstitutionCalculator.calculatePossibleSubstitutions(source, _, baseSubstitutions)).single
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

    def withJoiner[T <: Expression, TJoiner <: BinaryJoiner[T] : ClassTag](targetConnective: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[Seq[PossibleInference]] = {
      implicit val spc = stepProvingContext
      val targetSource = direction.getSource(targetLhs, targetRhs)
      def getSubstitutions(extractionResult: Statement): Option[PossibleSubstitutions] = {
        for {
          (conclusionConnective, conclusionSource) <- stepProvingContext.provingContext.definedBinaryJoiners.ofType[TJoiner].mapFind(j => j.unapply(extractionResult).map { case (l, r) => (j, direction.getSource(l, r))} )
          if stepProvingContext.provingContext.transitivities.exists(t => direction.getSource(t.firstPremiseJoiner, t.secondPremiseJoiner) == conclusionConnective && t.resultJoiner == targetConnective)
          substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(conclusionSource, targetSource)
        } yield substitutions
      }
      def getPossibleInference(inference: Inference): Option[PossibleInferenceWithTargets] = {
        val possibleConclusions = stepProvingContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
          .mapCollect(PossibleConclusionWithPremises.fromExtractionWithSubstitutions(_, getSubstitutions))
        if (possibleConclusions.nonEmpty) {
          Some(PossibleInferenceWithTargets(inference.summary, Seq(PossibleTarget(targetSource, Nil, Nil, possibleConclusions))))
        } else {
          None
        }
      }
      def getConclusionComplexity(possibleConclusion: PossibleConclusion): Int = {
        stepProvingContext.provingContext.definedBinaryRelations.ofType[TJoiner]
          .mapFind(j => j.unapply(possibleConclusion.conclusion).map { case (l, r) => ComplexityCalculator.calculateStructuralComplexity(direction.getSource(l, r)) })
          .getOrElse(0)
      }
      Success(getPossibleInferences(stepProvingContext.provingContext.entryContext.allInferences, searchText, getPossibleInference, getConclusionComplexity))
    }

    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      possibleInferences <- withRelation(step.statement, withJoiner[Statement, BinaryConnective](_, _, _, stepProvingContext), withJoiner[Term, BinaryRelation](_, _, _, stepProvingContext))(stepProvingContext)
    } yield possibleInferences).toResponseEntity
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

  def suggestChainingFromPremise(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    serializedPremiseStatement: String,
    direction: Direction
  ): ResponseEntity[_] = {
    def getPremises[T <: Expression](joiner: BinaryJoiner[T], lhs: T, rhs: T, premise: Statement, baseSubstitutions: PossibleSubstitutions)(implicit stepProvingContext: StepProvingContext): Try[Seq[PossibleConclusionWithPremises]] = {
      Success(SubstatementExtractor.getPremiseExtractions(premise)
        .flatMap(PossibleConclusionWithPremises.fromExtractionWithSubstitutions(_, conclusion => for {
          (conclusionLhs, conclusionRhs) <- joiner.unapply(conclusion)
          substitutions <- getSubstitutionsWithTermOrSubterm(direction.getSource(conclusionLhs, conclusionRhs), direction.getSource(lhs, rhs), baseSubstitutions)
        } yield substitutions)))
    }
    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
      premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
      baseSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(premise.statement, premise.statement)(stepProvingContext.stepContext).orBadRequest(s"Somehow failed to calculate base substitutions for premise '${premise.statement}'")
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
        def getResult(
          applyExtractions: (Seq[Inference.Summary], (ExpressionParsingContext, Substitutions) => Try[Option[Statement]]) => Try[(Statement, Option[Step], Seq[Step.Target])]
        ): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
          for {
            extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
            getIntendedConclusion = (expressionParsingContext: ExpressionParsingContext, substitutions: Substitutions) => definition.serializedIntendedConclusionStatement match {
              case Some(serializedIntendedConclusionStatement) =>
                for {
                  conclusionStatement <- Statement.parser(expressionParsingContext).parseFromString(serializedIntendedConclusionStatement, "intended conclusion").recoverWithBadRequest
                  substitutedConclusionStatement <- conclusionStatement.applySubstitutions(substitutions).orBadRequest("Could not apply substitutions to intended conclusion")
                } yield Some(substitutedConclusionStatement)
              case None =>
                Success(None)
            }
            (extractionResult, extractionStep, targets) <- applyExtractions(extractionInferences, getIntendedConclusion)
            (conclusionRelation, conclusionLhs, conclusionRhs) <- ChainingMethods.getJoiner[T](extractionResult).orBadRequest("Conclusion was not binary statement")
            conclusionSource = direction.getSource(conclusionLhs, conclusionRhs)
            rewriteChainingDefinition <- handle(conclusionSource, targetSource, conclusionRelation, conclusionLhs, conclusionRhs)
            intermediate = direction.getResult(rewriteChainingDefinition.lhs, rewriteChainingDefinition.rhs)
            updatedChainingDefinition = rewriteChainingDefinition.copy(step = extractionStep)
            (targetLhs, targetRhs) = direction.swapSourceAndResult(intermediate, targetResult)
            (firstDefinition, secondDefinition) = direction.swapSourceAndResult(updatedChainingDefinition, ChainingStepDefinition.forTarget(targetLhs, targetRhs, targetRelation))
          } yield (firstDefinition, secondDefinition, targets)
        }

        def fromInference(inferenceId: String): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
          getResult { (extractionInferences, getIntendedTarget) =>
            for {
              inference <- findInference(inferenceId)
              inferenceExtraction <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId.get(inferenceId).flatMap(_.find(_.extractionInferences == extractionInferences)).orBadRequest("Could not find extraction with given inferences")
              epc = ExpressionParsingContext.withDefinitions(inferenceExtraction.variableDefinitions)
              substitutions <- definition.substitutions.parse(inferenceExtraction.variableDefinitions)
              intendedConclusionOption <- getIntendedTarget(epc, substitutions)
              intendedPremiseStatementsOption <- definition.parseIntendedPremiseStatements(epc)
              (inferenceToApply, intendedExtractionPremisesOption) <- intendedPremiseStatementsOption match {
                case Some(intendedPremiseStatements) =>
                  for {
                    (intendedInferencePremises, intendedExtractionPremises) <- intendedPremiseStatements.takeAndRemainingIfValid(inference.premises.length).orBadRequest("Not enough intended premises statements provided")
                    substitutedIntendedExtractionPremises <- intendedExtractionPremises.map(_.applySubstitutions(substitutions)).traverseTry.orBadRequest("Could not apply substitutions to extraction premises")
                    _ <- (intendedInferencePremises == inference.premises).orBadRequest("Intended premises did not match inference premises")
                  } yield (inference.copy(premises = intendedInferencePremises), Some(substitutedIntendedExtractionPremises))
                case None =>
                  Success((inference, None))
              }
              (derivationStep, targets) <- ExtractionHelper.getInferenceExtractionWithPremises(inferenceToApply, extractionInferences, substitutions, intendedExtractionPremisesOption, intendedConclusionOption)
            } yield (derivationStep.statement, Some(derivationStep.step), targets)
          }
        }
        def fromPremise(serializedPremiseStatement: String): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])] = {
          getResult { (extractionInferences, getIntendedConclusion) =>
            for {
              premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
              extraction <- SubstatementExtractor.getPremiseExtractions(premiseStatement).find(_.extractionInferences == extractionInferences).orBadRequest("Could not find extraction with given inferences")
              premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
              epc = ExpressionParsingContext.withDefinitions(extraction.variableDefinitions)
              substitutions <- definition.substitutions.parse(extraction.variableDefinitions)
              intendedConclusionOption <- getIntendedConclusion(epc, substitutions)
              intendedPremiseStatementsOption <- definition.parseIntendedPremiseStatements(epc)
              substitutedIntendedPremiseStatementsOption <- intendedPremiseStatementsOption.map(_.map(_.applySubstitutions(substitutions)).traverseTry.orBadRequest("Could not apply substitutions to extraction premises")).swap
              (result, step, targets)  <- ExtractionHelper.getPremiseExtractionWithPremises(premise, extractionInferences, substitutions, substitutedIntendedPremiseStatementsOption, intendedConclusionOption)
            } yield (result, step, targets)
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

  @GetMapping(value = Array("/chainedTargetJoiners"))
  def getChainedTargetJoiners(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    def forConnective(connective: BinaryConnective)(implicit stepProvingContext: StepProvingContext): Try[Seq[(String, String)]] = {
      Success(stepProvingContext.provingContext.transitivities.filter(_.resultJoiner == connective).map(t => (t.firstPremiseJoiner.symbol, t.secondPremiseJoiner.symbol)))
    }
    def forRelation(relation: BinaryRelation)(implicit stepProvingContext: StepProvingContext): Try[Seq[(String, String)]] = {
      Success(
        stepProvingContext.provingContext.equalityOption.filter(_.relation != relation).map { e => Seq((e.relation.symbol, relation.symbol), (relation.symbol, e.relation.symbol))}.getOrElse(Nil) ++
          stepProvingContext.provingContext.transitivities.filter(_.resultJoiner == relation).map(t => (t.firstPremiseJoiner.symbol, t.secondPremiseJoiner.symbol)))
    }

    (for {
      (step, stepProvingContext) <- bookService.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath)
      result <- withRelation(step.statement, (c, _, _) => forConnective(c)(stepProvingContext), (r, _, _) => forRelation(r)(stepProvingContext))(stepProvingContext)
    } yield result).toResponseEntity
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
      def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          intermediateStatement <- Statement.parser.parseFromString(definition.serializedExpression, "target expression").recoverWithBadRequest
          leftJoiner <- spc.provingContext.definedBinaryConnectives.find(_.symbol == definition.leftJoiner).orBadRequest(s"Could not find connective with symbol ${definition.leftJoiner}")
          rightJoiner <- spc.provingContext.definedBinaryConnectives.find(_.symbol == definition.rightJoiner).orBadRequest(s"Could not find connective with symbol ${definition.rightJoiner}")
          firstStep = ChainingStepDefinition.forTarget(targetLhs, intermediateStatement, leftJoiner)
          secondStep = ChainingStepDefinition.forTarget(intermediateStatement, targetRhs, rightJoiner)
        } yield (firstStep, secondStep, Nil)
      }
      def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          intermediateTerm <- Term.parser.parseFromString(definition.serializedExpression, "target expression").recoverWithBadRequest
          leftJoiner <- spc.provingContext.definedBinaryRelations.find(_.symbol == definition.leftJoiner).orBadRequest(s"Could not find relation with symbol ${definition.leftJoiner}")
          rightJoiner <- spc.provingContext.definedBinaryRelations.find(_.symbol == definition.rightJoiner).orBadRequest(s"Could not find relation with symbol ${definition.rightJoiner}")
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
