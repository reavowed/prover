package net.prover.controllers

import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.model._
import net.prover.model.definitions.{BinaryConnective, BinaryJoiner, BinaryRelation, RelationExpansion, Wrapper}
import net.prover.model.expressions.{DefinedStatement, Expression, Statement, Term}
import net.prover.model.proof._
import net.prover.util.Swapper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification with ChainingStepEditing {

  private def insertTransitivityAssertion(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    @RequestBody definition: StepDefinition,
    swapper: Swapper
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
        val (targetSource, targetResult) = swapper.swapSourceAndResult(targetLhs, targetRhs)
        def getResult(applyExtractions: (Seq[Inference.Summary], Substitutions) => Try[(Statement, ExtractionApplication, Seq[Step.Assertion], Seq[Step], Seq[Step.Target], Seq[Step] => Step.Elided)]) = {
          for {
            extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
            substitutions <- definition.substitutions.parse()
            (conclusion, ExtractionApplication(extractionSteps, extractionPremises, extractionTargets), additionalAssertions, additionalPremises, additionalTargets, elider) <- applyExtractions(extractionInferences, substitutions)
            (conclusionRelation, conclusionLhs, conclusionRhs) <- ChainingMethods.getRelation[T](conclusion).orBadRequest("Conclusion was not binary statement")
            conclusionSource = swapper.getSource(conclusionLhs, conclusionRhs)
            rewriteChainingDefinition <- handle(conclusionSource, targetSource, conclusionRelation, conclusionLhs, conclusionRhs)
            extractionStep <- Step.Elided.ifNecessary(additionalAssertions ++ extractionSteps, elider) orBadRequest "No extraction steps"
            finalStep = Step.Elided.ifNecessary((additionalPremises ++ extractionPremises :+ extractionStep) ++ rewriteChainingDefinition.step.toSeq, elider).get
            intermediate = swapper.getResult(rewriteChainingDefinition.lhs, rewriteChainingDefinition.rhs)
            updatedChainingDefinition = rewriteChainingDefinition.copy(step = Some(finalStep))
            (targetLhs, targetRhs) = swapper.swapSourceAndResult(intermediate, targetResult)
            newTarget = targetRelation(targetLhs, targetRhs)
            newTargetStepOption = if (stepProvingContext.allPremisesSimplestFirst.exists(_.statement == newTarget)) None else Some(Step.Target(newTarget))
            (firstDefinition, secondDefinition) = swapper.swapSourceAndResult(updatedChainingDefinition, ChainingStepDefinition(targetLhs, targetRhs, targetRelation, newTargetStepOption))
          } yield (firstDefinition, secondDefinition, additionalTargets ++ extractionTargets)
        }

        def fromInference(inferenceId: String) = {
          getResult { (extractionInferences, substitutions) =>
            for {
              inference <- findInference(inferenceId)
              (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
              (conclusion, extractionApplication) <- ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions, PremiseFinder.findPremiseStepsOrTargets)
            } yield (conclusion, extractionApplication, Seq(mainAssertion), mainPremises, mainTargets, Step.Elided.forInference(inference))
          }
        }
        def fromPremise(serializedPremiseStatement: String) = {
          getResult { (extractionInferences, substitutions) =>
            for {
              premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
              premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
              (conclusion, extractionApplication) <- ExtractionHelper.applyExtractions(premise, extractionInferences, substitutions, PremiseFinder.findPremiseStepsOrTargets)
            } yield (conclusion, extractionApplication, Nil, Nil, Nil, Step.Elided.forDescription("Extracted"))
          }
        }
        definition.getFromInferenceOrPremise(fromInference, fromPremise)
      }
    })
  }

  @PostMapping(value = Array("/transitivityFromLeft"))
  def addTransitivityFromLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    insertTransitivityAssertion(bookKey, chapterKey, theoremKey, proofIndex, stepPath, definition, Swapper.DontSwap)
  }

  @PostMapping(value = Array("/transitivityFromRight"))
  def addTransitivityFromRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    insertTransitivityAssertion(bookKey, chapterKey, theoremKey, proofIndex, stepPath, definition, Swapper.Swap)
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

  @PostMapping(value = Array("/introduceNaming"))
  def introduceNaming(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: NamingDefinition
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        variableName <- Option(definition.variableName.trim).filter(_.nonEmpty).orBadRequest("Variable name must be provided")
        inference <- findInference(definition.inferenceId)
        (namingPremises, assumption) <- ProofHelper.getNamingPremisesAndAssumption(inference).orBadRequest(s"Inference ${definition.inferenceId} is not a naming inference")
        substitutions <- definition.substitutions.parse()
        _ <- inference.substituteConclusion(substitutions).filter(_ == step.statement).orBadRequest("Conclusion was incorrect")
        premiseStatements <- namingPremises.map(inference.substituteStatement(_, substitutions)).traverseOption.orBadRequest("Could not substitute premises")
        substitutedAssumption <- assumption.applySubstitutions(substitutions, 1, stepProvingContext.stepContext.externalDepth).orBadRequest("Could not substitute assumption")
      } yield {
        val premises = premiseStatements.map(stepProvingContext.createPremise)
        val targetSteps = premises.ofType[Premise.Pending].map(p => ProofHelper.findFact(p.statement).getOrElse(Step.Target(p.statement)))
        targetSteps :+ Step.Naming(
          variableName,
          substitutedAssumption,
          step.statement,
          Seq(Step.Target(step.statement.insertExternalParameters(1))),
          inference,
          premises,
          substitutions)
      }
    }.toResponseEntity
  }


  @PostMapping(value = Array("/introduceNamingFromPremise"))
  def introduceNamingFromPremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedPremise: String
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (targetStep, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremise, "premise").recoverWithBadRequest
        premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
        variableName <- premiseStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.scopedBoundVariableNames.single).orBadRequest("Premise did not have single bound variable")
        (namingInference, namingInferenceAssumption, substitutionsAfterPremise) <- ProofHelper.findNamingInferences(stepProvingContext.provingContext.entryContext).mapFind {
          case (i, Seq(singlePremise), a) =>
            singlePremise.calculateSubstitutions(premiseStatement).map { s => (i, a, s) }
          case _ =>
            None
        }.orBadRequest("Could not find naming inference matching premise")
        substitutionsAfterConclusion <- namingInference.conclusion.calculateSubstitutions(targetStep.statement, substitutionsAfterPremise).orBadRequest("Could not calculate substitutions for conclusion")
        substitutions <- substitutionsAfterConclusion.confirmTotality.orBadRequest("Substitutions for naming inference were not total")
        substitutedAssumption <- namingInferenceAssumption.applySubstitutions(substitutions, 1, stepProvingContext.stepContext.externalDepth).orBadRequest("Could not substitute assumption")
      } yield {
        Seq(Step.Naming(
          variableName,
          substitutedAssumption,
          targetStep.statement,
          Seq(Step.Target(targetStep.statement.insertExternalParameters(1))),
          namingInference.summary,
          Seq(premise),
          substitutions))
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/introduceBoundVariable"))
  def introduceBoundVariable(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        scopingDefinition <- stepProvingContext.provingContext.scopingDefinitionOption.orBadRequest("No scoping definition provided")
        (substatement, variableName) <- (step.statement match {
          case definedStatement @ DefinedStatement(Seq(substatement: Statement), `scopingDefinition`) =>
            definedStatement.scopedBoundVariableNames.single.map(substatement -> _)
          case _ =>
            None
        }).orBadRequest("Target statement is not a scoped statement")
      } yield {
        Step.ScopedVariable(
          variableName,
          Seq(Step.Target(substatement)),
          scopingDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/introduceDeduction"))
  def introduceDeduction(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        deductionDefinition <- stepProvingContext.provingContext.deductionDefinitionOption.orBadRequest("No scoping definition provided")
        (antecedent, consequent) <- (step.statement match {
          case definedStatement @ DefinedStatement(Seq(antecedent: Statement, consequent: Statement), `deductionDefinition`) =>
            Some((antecedent, consequent))
          case _ =>
            None
        }).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rearrange"), produces = Array("application/json;charset=UTF-8"))
  def rearrange(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        newStep <- TermRearranger.rearrange(step.statement)(stepProvingContext).orBadRequest(s"Could not rearrange statement ${step.statement}")
      } yield Seq(newStep)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/target"))
  def addTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedStatement: String
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        targetStatement <- Statement.parser.parseFromString(serializedStatement, "target statement").recoverWithBadRequest
        targetStep = Step.Target(targetStatement)
      } yield Seq(targetStep, step)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rewriteDefinition"))
  def rewriteDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedPremiseStatement: String
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
        premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        newStep <- DefinitionRewriter.rewriteDefinitions(premise.statement, step.statement).orBadRequest("Could not rewrite definition")
      } yield Seq(newStep)
    }.toResponseEntity
  }
}
