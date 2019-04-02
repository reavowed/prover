package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.model.proof.{Premise, ProofHelper, Step}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (step, stepContext, parsingContext) =>
      for {
        inference <- findInference(definition.inferenceId)(parsingContext)
        substitutions <- definition.substitutions.parse(inference)(parsingContext)
        premiseStatements <- inference.substitutePremisesAndValidateConclusion(step.statement, substitutions, stepContext.externalDepth).recoverWithBadRequest
      } yield {
        val premises = premiseStatements.map(createPremise(_, stepContext, parsingContext))
        val targetSteps = premises.ofType[Premise.Pending].map(p => ProofHelper.findFact(p.statement, stepContext, parsingContext).getOrElse(Step.Target(p.statement)))
        targetSteps :+ Step.Assertion(
          step.statement,
          inference,
          premises,
          substitutions)
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, stepContext, parsingContext) =>
      for {
        inference <- findInference(definition.inferenceId)(parsingContext)
        (namingPremises, assumption) <- ProofHelper.getNamingPremisesAndAssumption(inference, parsingContext).orBadRequest(s"Inference ${definition.inferenceId} is not a naming inference")
        substitutions <- definition.substitutions.parse(inference)(parsingContext)
        _ <- inference.validateConclusion(step.statement, substitutions, stepContext.externalDepth).recoverWithBadRequest
        premiseStatements <- namingPremises.map(inference.substituteStatement(_, substitutions, stepContext.externalDepth)).recoverWithBadRequest
        substitutedAssumption <- assumption.applySubstitutions(substitutions, 1, stepContext.externalDepth).orBadRequest("Could not substitute assumption")
      } yield {
        val premises = premiseStatements.map(createPremise(_, stepContext, parsingContext))
        val targetSteps = premises.ofType[Premise.Pending].map(p => ProofHelper.findFact(p.statement, stepContext, parsingContext).getOrElse(Step.Target(p.statement)))
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
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody variableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, _, parsingContext) =>
      for {
        (substatement, scopingStatementDefinition) <- parsingContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, _, parsingContext) =>
      for {
        (antecedent, consequent, deductionStatementDefinition) <- parsingContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionStatementDefinition)
      }
    }.toResponseEntity
  }
}
