package net.prover.controllers

import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.model._
import net.prover.model.definitions.Wrapper
import net.prover.model.expressions.{DefinedStatement, Statement, Term}
import net.prover.model.proof.SubstatementExtractor.VariableTracker
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
    for {
      inference <- findInference(definition.inferenceId)
      substitutions <- definition.substitutions.parse()
      extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
      (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
      (extractionResult, ExtractionApplication(extractionSteps, extractionPremises, extractionTargets)) <- ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions)
      extractionStep = Step.Elided.ifNecessary(mainAssertion +: extractionSteps, inference).get
      finalStep = Step.Elided.ifNecessary(mainPremises ++ extractionPremises :+ extractionStep, inference).get
    } yield (extractionResult, finalStep, mainTargets ++ extractionTargets)
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhs, targetRhs) =>
      val (targetSource, targetDestination) = swapper.swap(targetLhs, targetRhs)
      implicit val spc = stepProvingContext
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.substitutions.parse()
        extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
        // Our target is A = C. We're either going to prove A = B or B = C, and add the other part in as a target.
        (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
        (conclusion, ExtractionApplication(extractionSteps, extractionPremises, extractionTargets)) <- ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions)
        (conclusionLhs, conclusionRhs) <- transitivity.relation.unapply(conclusion).orBadRequest("Inference conclusion is not a transitive statement")
        conclusionSource = swapper.getOne(conclusionLhs, conclusionRhs)
        wrapper <- targetSource.getTerms().filter(_._1 == conclusionSource).map(_._2).map(Wrapper.fromExpression).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
        (expansionStep, expandedConclusion) <-
          if (wrapper.isIdentity)
            Success((None, conclusion))
          else
            for {
              expansionDefinition <- stepProvingContext.provingContext.definitionsByRelation.get(transitivity.relation).flatMap(_.expansion).orBadRequest("Could not find expansion inference")
              step = expansionDefinition.assertionStep(conclusionLhs, conclusionRhs, wrapper)
            } yield (
              Some(step),
              step.statement)
        (_, intermediateTerm) <- transitivity.relation.unapply(expandedConclusion).map(swapper.swapTuple).orBadRequest("Rewritten expanded conclusion is not a transitive statement")
        extractionStep = Step.Elided.ifNecessary(mainAssertion +: extractionSteps, inference).get
        finalStep = Step.Elided.ifNecessary((mainPremises ++ extractionPremises :+ extractionStep) ++ expansionStep.toSeq, inference).get
        newTarget = Step.Target((transitivity.relation.apply _).tupled.apply(swapper.swap(intermediateTerm, targetDestination)))
        (firstStep, secondStep) = swapper.swap(finalStep, newTarget)
      } yield (Some(firstStep), Some(secondStep), intermediateTerm, mainTargets ++ extractionTargets)
    }
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        intermediateTerm <- Term.parser.parseFromString(serializedTerm, "target term").recoverWithBadRequest
        firstStep = Step.Target(transitivity.relation(targetLhs, intermediateTerm))
        secondStep = Step.Target(transitivity.relation(intermediateTerm, targetRhs))
      } yield (Some(firstStep), Some(secondStep), intermediateTerm, Nil)
    }
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
          definition.variableName,
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
