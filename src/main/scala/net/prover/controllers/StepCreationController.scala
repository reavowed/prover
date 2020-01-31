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
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification with TransitivityEditing {

  def rewriteFromConclusion(conclusion: Statement, rewriteInferenceId: Option[String])(implicit stepProvingContext: StepProvingContext): Try[(Option[Step.Assertion], Statement)] = {
    rewriteInferenceId.map { rewriteInferenceid =>
      for {
        (rewriteInference, rewriteInferencePremise) <- stepProvingContext.provingContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
        rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(conclusion).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
        substitutedPremise <- rewriteInferencePremise.applySubstitutions(rewriteSubstitutions).orBadRequest("Could not apply substitutions from rewrite inference")
      } yield (Some(Step.Assertion(conclusion, rewriteInference.summary, Seq(Premise.Pending(substitutedPremise)), rewriteSubstitutions)), substitutedPremise)
    }.getOrElse(Success((None, conclusion)))
  }
  def rewriteFromPremise(premise: Statement, rewriteInferenceId: Option[String])(implicit provingContext: ProvingContext, stepContext: StepContext): Try[(Option[Step.Assertion], Statement)] = {
    rewriteInferenceId.map { rewriteInferenceid =>
      for {
        (rewriteInference, rewriteInferencePremise) <- provingContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
        rewriteSubstitutions <- rewriteInferencePremise.calculateSubstitutions(premise).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
        substitutedConclusion <- rewriteInference.conclusion.applySubstitutions(rewriteSubstitutions).orBadRequest("Could not apply substitutions from rewrite inference")
      } yield (Some(Step.Assertion(substitutedConclusion, rewriteInference.summary, Seq(Premise.Pending(premise)), rewriteSubstitutions)), substitutedConclusion)
    }.getOrElse(Success((None, premise)))
  }

  def createStep(definition: StepDefinition)(implicit stepProvingContext: StepProvingContext): Try[(Statement, Step, Seq[Step.Target])] = {
    def withInference(inferenceId: String) = {
      for {
        inference <- findInference(inferenceId)
        substitutions <- definition.substitutions.parse()
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
        (extractionResult, ExtractionApplication(extractionSteps, extractionPremises, extractionTargets)) <- ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions)
        extractionStep = Step.Elided.ifNecessary(mainAssertion +: extractionSteps, inference).get
        finalStep = Step.Elided.ifNecessary(mainPremises ++ extractionPremises :+ extractionStep, inference).get
      } yield (extractionResult, finalStep, mainTargets ++ extractionTargets)
    }
    def withPremise(serializedPremiseStatement: String) = {
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
        substitutions <- definition.substitutions.parse()
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        (extractionResult, ExtractionApplication(extractionSteps, extractionPremises, extractionTargets)) <- ExtractionHelper.applyExtractions(premise, extractionInferences, substitutions)
        extractionStep = Step.Elided.ifNecessary(extractionSteps, "Extracted").get
        finalStep = Step.Elided.ifNecessary(extractionPremises :+ extractionStep, "Extracted").get
      } yield (extractionResult, finalStep, extractionTargets)
    }
    definition.getFromInferenceOrPremise(withInference, withPremise)
  }

  @PutMapping
  def createAssertion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStepAndAddBeforeTransitivity[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (targetStep, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        (result, newStep, targets) <- createStep(definition)
        _ <- (result == targetStep.statement).orBadRequest("Conclusion was incorrect")
      } yield (newStep, targets)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/assertion"))
  def addAssertion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStepAndAddBeforeTransitivity[Step](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        (_, newStep, targets) <- createStep(definition)
      } yield (step, targets :+ newStep)
    }.toResponseEntity
  }

  private def insertTransitivityAssertion(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    @RequestBody definition: StepDefinition,
    swapper: Swapper
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateStepsForTransitivity {
      override def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[Statement], Option[Step], BinaryJoiner[Statement], Option[Step], Statement, Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        createSteps(targetConnective, targetLhs, targetRhs, (_, _, relation, lhs, rhs) => Success((None, relation(lhs, rhs))))
      }
      override def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[Term], Option[Step], BinaryJoiner[Term], Option[Step], Term, Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        def getExpansion(conclusionSource: Term, targetSource: Term, conclusionRelation: BinaryJoiner[Term], conclusionLhs: Term, conclusionRhs: Term) = for {
          wrapper <- targetSource.getTerms().filter(_._1 == conclusionSource).map(_._2).map(Wrapper.fromExpression).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
          (expansionStep, expandedConclusion) <-
            if (wrapper.isIdentity)
              Success((None, conclusionRelation(conclusionLhs, conclusionRhs)))
            else
              for {
                expansionDefinition <- stepProvingContext.provingContext.expansions.ofType[RelationExpansion]
                  .find(e => e.sourceJoiner == conclusionRelation && e.resultJoiner == targetRelation)
                  .orBadRequest("Could not find expansion")
                step = expansionDefinition.assertionStep(conclusionLhs, conclusionRhs, wrapper)
              } yield (
                Some(step),
                step.statement)
        } yield (expansionStep, expandedConclusion)
        createSteps(targetRelation, targetLhs, targetRhs, getExpansion)
      }

      def createSteps[T <: Expression : TransitivityMethods](
        targetRelation: BinaryJoiner[T],
        targetLhs: T,
        targetRhs: T,
        handle: (T, T, BinaryJoiner[T], T, T) => Try[(Option[Step], Statement)])(
        implicit stepProvingContext: StepProvingContext
      ): Try[(BinaryJoiner[T], Option[Step], BinaryJoiner[T], Option[Step], T, Seq[Step.Target])] = {
        val (targetSource, targetDestination) = swapper.swap(targetLhs, targetRhs)
        def getResult(applyExtractions: (Seq[Inference.Summary], Substitutions) => Try[(Statement, ExtractionApplication, Seq[Step.Assertion], Seq[Step.Assertion], Seq[Step.Target], Seq[Step] => Step.Elided)]) = {
          for {
            extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
            substitutions <- definition.substitutions.parse()
            (conclusion, ExtractionApplication(extractionSteps, extractionPremises, extractionTargets), additionalAssertions, additionalPremises, additionalTargets, elider) <- applyExtractions(extractionInferences, substitutions)
            (conclusionRelation, conclusionLhs, conclusionRhs) <- TransitivityMethods.getRelation[T](conclusion).orBadRequest("Conclusion was not binary statement")
            conclusionSource = swapper.getOne(conclusionLhs, conclusionRhs)
            (expansionStepOption, expandedConclusion) <- handle(conclusionSource, targetSource, conclusionRelation, conclusionLhs, conclusionRhs)
            (_, intermediateTerm) <- conclusionRelation.unapply(expandedConclusion).map(swapper.swapTuple).orBadRequest("Rewritten expanded conclusion is not a transitive statement")
            extractionStep <- Step.Elided.ifNecessary(additionalAssertions ++ extractionSteps, elider) orBadRequest "No extraction steps"
            finalStep = Step.Elided.ifNecessary((additionalPremises ++ extractionPremises :+ extractionStep) ++ expansionStepOption.toSeq, elider).get
            newTarget = (targetRelation.apply _).tupled.apply(swapper.swap(intermediateTerm, targetDestination))
            newTargetStepOption = if (stepProvingContext.allPremisesSimplestFirst.exists(_.statement == newTarget)) None else Some(Step.Target(newTarget))
            ((firstRelation, firstStep), (secondRelation, secondStep)) = swapper.swap((conclusionRelation, Some(finalStep)), (targetRelation, newTargetStepOption))
          } yield (firstRelation, firstStep, secondRelation, secondStep, intermediateTerm, additionalTargets ++ extractionTargets)
        }

        def fromInference(inferenceId: String) = {
          getResult { (extractionInferences, substitutions) =>
            for {
              inference <- findInference(inferenceId)
              (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
              (conclusion, extractionApplication) <- ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions)
            } yield (conclusion, extractionApplication, Seq(mainAssertion), mainPremises, mainTargets, Step.Elided.forInference(inference))
          }
        }
        def fromPremise(serializedPremiseStatement: String) = {
          getResult { (extractionInferences, substitutions) =>
            for {
              premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
              premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
              (conclusion, extractionApplication) <- ExtractionHelper.applyExtractions(premise, extractionInferences, substitutions)
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
    @RequestBody serializedTerm: String
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath, new CreateStepsForTransitivityCommon {
      def createSteps[T <: Expression : TransitivityMethods](joiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[T], Option[Step], BinaryJoiner[T], Option[Step], T, Seq[Step.Target])] = {
        implicit val spc = stepProvingContext
        for {
          intermediateTerm <- TransitivityMethods.parser.parseFromString(serializedTerm, "target term").recoverWithBadRequest
          firstStep = Step.Target(joiner(targetLhs, intermediateTerm))
          secondStep = Step.Target(joiner(intermediateTerm, targetRhs))
        } yield (joiner, Some(firstStep), joiner, Some(secondStep), intermediateTerm, Nil)
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (targetStep, stepProvingContext) =>
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        (substatement, variableName, scopingStatementDefinition) <- stepProvingContext.provingContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
      } yield {
        Step.ScopedVariable(
          variableName,
          Seq(Step.Target(substatement)),
          scopingStatementDefinition)
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        (antecedent, consequent, deductionStatementDefinition) <- stepProvingContext.provingContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionStatementDefinition)
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
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
    replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        premiseStatement <- Statement.parser(stepProvingContext).parseFromString(serializedPremiseStatement, "premise statement").recoverWithBadRequest
        premise <- stepProvingContext.allPremisesSimplestFirst.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        newStep <- DefinitionRewriter.rewriteDefinitions(premise.statement, step.statement).orBadRequest("Could not rewrite definition")
      } yield Seq(newStep)
    }.toResponseEntity
  }
}
