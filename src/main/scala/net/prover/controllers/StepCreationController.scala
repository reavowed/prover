package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.model.{ExpressionParsingContext, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{EqualityRewriter, Premise, ProofHelper, Step, SubstatementExtractor, TermRearranger}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification {

  @PutMapping
  def createAssertion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepContext) =>
      for {
        inference <- findInference(definition.inferenceId)(stepContext.entryContext)
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(stepContext))
        (rewriteStep, target) <- definition.rewriteInferenceId.map { rewriteInferenceid =>
          for {
            (rewriteInference, rewriteInferencePremise) <- stepContext.entryContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
            rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(step.statement, stepContext).headOption.orBadRequest("Could not calculate substitutions for rewrite inference")
            newTarget <- rewriteInferencePremise.applySubstitutions(rewriteSubstitutions, stepContext).orBadRequest("Could not apply substitutions from rewrite inference")
          } yield (Some(Step.Assertion(step.statement, rewriteInference.summary, Seq(Premise.Pending(newTarget)), rewriteSubstitutions)), newTarget)
        }.getOrElse(Success((None, step.statement)))
        _ <- Try(inference.validateConclusion(target, substitutions, stepContext))
      } yield {
        ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, rewriteStep)
      }
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
    replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepContext) =>
      for {
        inference <- findInference(definition.inferenceId)(stepContext.entryContext)
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(stepContext))
      } yield {
        ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext) :+ step
      }
    }.toResponseEntity
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      val entryContext = stepContext.entryContext
      for {
        inference <- findInference(definition.inferenceId)(entryContext)
        (namingPremises, assumption) <- ProofHelper.getNamingPremisesAndAssumption(inference, entryContext).orBadRequest(s"Inference ${definition.inferenceId} is not a naming inference")
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(stepContext))
        _ <- inference.validateConclusion(step.statement, substitutions, stepContext).recoverWithBadRequest
        premiseStatements <- namingPremises.map(inference.substituteStatement(_, substitutions, stepContext)).recoverWithBadRequest
        substitutedAssumption <- assumption.applySubstitutions(substitutions, 1, stepContext.externalDepth).orBadRequest("Could not substitute assumption")
      } yield {
        val premises = premiseStatements.map(stepContext.createPremise)
        val targetSteps = premises.ofType[Premise.Pending].map(p => ProofHelper.findFact(p.statement, stepContext).getOrElse(Step.Target(p.statement)))
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

  @PostMapping(value = Array("/introduceBoundVariable"))
  def introduceBoundVariable(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      for {
        (substatement, variableName, scopingStatementDefinition) <- stepContext.entryContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      for {
        (antecedent, consequent, deductionStatementDefinition) <- stepContext.entryContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionStatementDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/extract"), produces = Array("application/json;charset=UTF-8"))
  def extract(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      for {
        newStep <- SubstatementExtractor.extract(step.statement, stepContext).orBadRequest(s"Could not extract statement ${step.statement}")
      } yield Seq(newStep)
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      for {
        newStep <- TermRearranger.rearrange(step.statement, stepContext).orBadRequest(s"Could not rearrange statement ${step.statement}")
      } yield Seq(newStep)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rewrite"), produces = Array("application/json;charset=UTF-8"))
  def rewrite(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      EqualityRewriter.rewrite(step.statement, stepContext)
        .orBadRequest(s"Could not simplify statement ${step.statement}")
        .map(Seq(_))
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
    replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepContext) =>
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.atStep(stepContext)
      for {
        targetStatement <- Statement.parser.parseFromString(serializedStatement, "target statement").recoverWithBadRequest
        targetStep = Step.Target(targetStatement)
      } yield Seq(targetStep, step)
    }.toResponseEntity
  }
}
