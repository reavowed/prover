package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.model.ExpressionParsingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, ProofHelper, Step}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/{stepPath}"))
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification {

  @PutMapping
  def createAssertion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (step, stepContext, premiseContext, entryContext) =>
      for {
        inference <- findInference(definition.inferenceId)(entryContext)
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(entryContext, stepContext))
        _ = inference.validateConclusion(step.statement, substitutions, stepContext)
      } yield {
        ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, premiseContext, entryContext)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/assertion"))
  def addAssertion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepReference) { (step, stepContext, premiseContext, entryContext) =>
      for {
        inference <- findInference(definition.inferenceId)(entryContext)
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(entryContext, stepContext))
      } yield {
        ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, premiseContext, entryContext) :+ step
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/introduceNaming"))
  def introduceNaming(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody definition: NamingDefinition
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, stepContext, premiseContext, entryContext) =>
      for {
        inference <- findInference(definition.inferenceId)(entryContext)
        (namingPremises, assumption) <- ProofHelper.getNamingPremisesAndAssumption(inference, entryContext).orBadRequest(s"Inference ${definition.inferenceId} is not a naming inference")
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(entryContext, stepContext))
        _ <- inference.validateConclusion(step.statement, substitutions, stepContext).recoverWithBadRequest
        premiseStatements <- namingPremises.map(inference.substituteStatement(_, substitutions, stepContext)).recoverWithBadRequest
        substitutedAssumption <- assumption.applySubstitutions(substitutions, 1, stepContext.externalDepth).orBadRequest("Could not substitute assumption")
      } yield {
        val premises = premiseStatements.map(premiseContext.createPremise)
        val targetSteps = premises.ofType[Premise.Pending].map(p => ProofHelper.findFact(p.statement, stepContext, entryContext).getOrElse(Step.Target(p.statement)))
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
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, _, entryContext) =>
      for {
        (substatement, variableName, scopingStatementDefinition) <- entryContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
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
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, _, entryContext) =>
      for {
        (antecedent, consequent, deductionStatementDefinition) <- entryContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
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
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, stepContext, premiseContext, entryContext) =>
      for {
        newStep <- ProofHelper.extract(step.statement, entryContext, stepContext, premiseContext).orBadRequest(s"Could not extract statement ${step.statement}")
      } yield Seq(newStep)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rearrange"), produces = Array("application/json;charset=UTF-8"))
  def rearrange(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, stepContext, premiseContext, entryContext) =>
      for {
        newStep <- ProofHelper.rearrange(step.statement, entryContext, premiseContext, stepContext).orBadRequest(s"Could not rearrange statement ${step.statement}")
      } yield Seq(newStep)
    }.toResponseEntity
  }

  @PostMapping(value = Array("/rewrite"), produces = Array("application/json;charset=UTF-8"))
  def rewrite(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, stepContext, premiseContext, entryContext) =>
      ProofHelper.rewrite(step.statement, entryContext, premiseContext, stepContext)
        .orBadRequest(s"Could not simplify statement ${step.statement}")
    }.toResponseEntity
  }

  @PostMapping(value = Array("/target"))
  def addTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody serializedStatement: String
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepPath) { (step, stepContext, _, entryContext) =>
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.atStep(entryContext, stepContext)
      for {
        targetStatement <- Statement.parser.parseFromString(serializedStatement, "target statement").recoverWithBadRequest
        targetStep = Step.Target(targetStatement)
      } yield Seq(targetStep, step)
    }.toResponseEntity
  }
}
