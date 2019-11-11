package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.exceptions.{BadRequestException, NotFoundException}
import net.prover.model.{ExpressionParsingContext, Substitutions}
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.reflect.classTag
import scala.util.{Failure, Success, Try}

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
            rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(step.statement, stepContext).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
            newTarget <- rewriteInferencePremise.applySubstitutions(rewriteSubstitutions, stepContext).orBadRequest("Could not apply substitutions from rewrite inference")
          } yield (Some(Step.Assertion(step.statement, rewriteInference.summary, Seq(Premise.Pending(newTarget)), rewriteSubstitutions)), newTarget)
        }.getOrElse(Success((None, step.statement)))
        result <- ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, rewriteStep).orBadRequest("Could not apply substitutions to inference")
        _ <- inference.substituteConclusion(substitutions, stepContext).filter(_ == target).orBadRequest("Conclusion was incorrect")
      } yield result
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
        stepsForAssertion <- ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext).orBadRequest("Could not apply substitutions to inference")
      } yield {
        stepsForAssertion :+ step
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/transitivityFromLeft"))
  def addTransitivityFromLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    def splitTransitivity(template: Statement, target: Statement, stepContext: StepContext): Option[(Term, Term)] = {
      for {
        substitutions <- template.calculateSubstitutions(target, stepContext)
        Seq(lhs, rhs) <- template.requiredSubstitutions.terms.map(substitutions.terms.get).traverseOption
      } yield (lhs, rhs)
    }

    (stepReference.indexes match {
      case Nil =>
        Failure(NotFoundException(s"Step $stepReference"))
      case init :+ last =>
        modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, entryContext) =>
          theorem.modifySteps(proofIndex, init, entryContext) { (steps, outerStepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              val stepContext = outerStepContext.addSteps(before).atIndex(last)
              for {
                targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest(s"Step was not target")
                inference <- findInference(definition.inferenceId)(stepContext.entryContext)
                substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(stepContext))
                (targetLhs, targetRhs, transitivityTemplate, transitivityInference) <- entryContext.getTransitivityDefinitions.mapFind { case (_, template, transitivityInference) =>
                  for {
                    (lhs, rhs) <- splitTransitivity(template, targetStep.statement, stepContext)
                  } yield (lhs, rhs, template, transitivityInference)
                }.orBadRequest("Target step is not a transitive statement")
                (followingStep, restOfSteps) <- after.headAndTailOption.orBadRequest("No following step")
                (mainLhs, _) <- followingStep.asOptionalInstanceOf[Step.Assertion]
                  .filter(_.inference.id == transitivityInference.id)
                  .flatMap(s => splitTransitivity(transitivityTemplate, s.statement, stepContext))
                  .orBadRequest("Following step not transitivity")
                applyTemplate = (a: Term, b: Term) => transitivityTemplate.applySubstitutions(Substitutions(terms = transitivityTemplate.requiredSubstitutions.terms.zip(Seq(a, b)).toMap), 0, 0).get
                conclusion <- inference.substituteConclusion(substitutions, stepContext).orBadRequest("Could not apply substitutions to inference")
                (conclusionLhs, conclusionRhs) <- (
                  for {
                    transitivitySubstitutions <- transitivityTemplate.calculateSubstitutions(conclusion, stepContext)
                    Seq(conclusionLhs, conclusionRhs) <- transitivityTemplate.requiredSubstitutions.terms.map(transitivitySubstitutions.terms.get).traverseOption
                  } yield (conclusionLhs, conclusionRhs)
                  ).orBadRequest("Inference conclusion is not a transitive statement")
                function <- targetLhs.getTerms(stepContext).filter(_._1 == conclusionLhs).map(_._2).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
                (expansionStep, intermediateTerm) <- if (function.isInstanceOf[FunctionParameter])
                  Success((None, conclusionRhs))
                else
                  for {
                    expansionInference <- entryContext.findExpansionInference(transitivityTemplate).orBadRequest("Could not find expansion inference")
                    expandedLhs = function.specify(Seq(conclusionLhs), 0, stepContext.externalDepth)
                    expandedRhs = function.specify(Seq(conclusionRhs), 0, stepContext.externalDepth)
                  } yield (
                    Some(
                      Step.Assertion(
                        applyTemplate(expandedLhs, expandedRhs),
                        expansionInference.summary,
                        Seq(Premise.Pending(applyTemplate(conclusionLhs, conclusionRhs))),
                        Substitutions(
                          terms = expansionInference.requiredSubstitutions.terms.zip(Seq(conclusionLhs, conclusionRhs)).toMap,
                          functions = expansionInference.requiredSubstitutions.functions.zip(Seq(function)).toMap))),
                    expandedRhs)
                assertionSteps <- ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, expansionStep).orBadRequest("Could not apply substitutions to inference")
                firstTransitivityStep = Step.Assertion(
                  applyTemplate(mainLhs, intermediateTerm),
                  transitivityInference.summary,
                  Seq(Premise.Pending(applyTemplate(mainLhs, targetLhs)), Premise.Pending(applyTemplate(targetLhs, intermediateTerm))),
                  Substitutions(terms = transitivityInference.requiredSubstitutions.terms.zip(Seq(mainLhs, targetLhs, intermediateTerm)).toMap))
                newTarget = Step.Target(applyTemplate(intermediateTerm, targetRhs))
                secondTransitivityStep = Step.Assertion(
                  applyTemplate(mainLhs, targetRhs),
                  transitivityInference.summary,
                  Seq(Premise.Pending(applyTemplate(mainLhs, intermediateTerm)), Premise.Pending(applyTemplate(intermediateTerm, targetRhs))),
                  Substitutions(terms = transitivityInference.requiredSubstitutions.terms.zip(Seq(mainLhs, intermediateTerm, targetRhs)).toMap))
              } yield before ++ (assertionSteps :+ firstTransitivityStep :+ newTarget :+ secondTransitivityStep) ++ restOfSteps
            }
          }.orNotFound(s"Step $stepReference").flatten
        }
    }).toResponseEntity
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
        _ <- inference.substituteConclusion(substitutions, stepContext).filter(_ == step.statement).orBadRequest("Conclusion was incorrect")
        premiseStatements <- namingPremises.map(inference.substituteStatement(_, substitutions, stepContext)).traverseOption.orBadRequest("Could not substitute premises")
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
