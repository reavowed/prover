package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.exceptions.NotFoundException
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._
import net.prover.model.{ExpressionParsingContext, TransitivityDefinition}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification {

  def rewriteFromConclusion(conclusion: Statement, rewriteInferenceId: Option[String])(implicit stepContext: StepContext): Try[(Option[Step.Assertion], Statement)] = {
    rewriteInferenceId.map { rewriteInferenceid =>
      for {
        (rewriteInference, rewriteInferencePremise) <- stepContext.entryContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
        rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(conclusion, stepContext).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
        substitutedPremise <- rewriteInferencePremise.applySubstitutions(rewriteSubstitutions, stepContext).orBadRequest("Could not apply substitutions from rewrite inference")
      } yield (Some(Step.Assertion(conclusion, rewriteInference.summary, Seq(Premise.Pending(substitutedPremise)), rewriteSubstitutions)), substitutedPremise)
    }.getOrElse(Success((None, conclusion)))
  }
  def rewriteFromPremise(premise: Statement, rewriteInferenceId: Option[String])(implicit stepContext: StepContext): Try[(Option[Step.Assertion], Statement)] = {
    rewriteInferenceId.map { rewriteInferenceid =>
      for {
        (rewriteInference, rewriteInferencePremise) <- stepContext.entryContext.rewriteInferences.find(_._1.id == rewriteInferenceid).orBadRequest(s"Could not find rewrite inference $rewriteInferenceid")
        rewriteSubstitutions <- rewriteInferencePremise.calculateSubstitutions(premise, stepContext).flatMap(_.confirmTotality).orBadRequest("Could not calculate substitutions for rewrite inference")
        substitutedConclusion <- rewriteInference.conclusion.applySubstitutions(rewriteSubstitutions, stepContext).orBadRequest("Could not apply substitutions from rewrite inference")
      } yield (Some(Step.Assertion(substitutedConclusion, rewriteInference.summary, Seq(Premise.Pending(premise)), rewriteSubstitutions)), substitutedConclusion)
    }.getOrElse(Success((None, premise)))
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepContext) =>
      for {
        inference <- findInference(definition.inferenceId)(stepContext.entryContext)
        substitutions <- definition.substitutions.parse(inference)(ExpressionParsingContext.atStep(stepContext))
        (rewriteStep, target) <- rewriteFromConclusion(step.statement, definition.rewriteInferenceId)(stepContext)
        result <- ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, rewriteStep.toSeq).orBadRequest("Could not apply substitutions to inference")
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

  private def insertTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    f: (StepContext, TransitivityDefinition, Term, Term) => Try[(Seq[Step], Seq[Step], Term)]
  ): ResponseEntity[_] = {
    (stepPath.indexes match {
      case Nil =>
        Failure(NotFoundException(s"Step $stepPath"))
      case init :+ last =>
        modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, entryContext) =>
          theorem.modifySteps(proofIndex, init, entryContext) { (steps, outerStepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              implicit val stepContext = outerStepContext.addSteps(before).atIndex(last)
              for {
                targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest(s"Step was not target")
                (targetLhs, targetRhs, transitivityDefinition) <- entryContext.getTransitivityDefinitions.mapFind { transitivityDefinition =>
                  for {
                    (lhs, rhs) <- transitivityDefinition.splitStatement(targetStep.statement)
                  } yield (lhs, rhs, transitivityDefinition)
                }.orBadRequest("Target step is not a transitive statement")
                (followingStep, restOfSteps) <- after.headAndTailOption.orBadRequest("No following step")
                (mainLhs, _) <- followingStep.asOptionalInstanceOf[Step.Assertion]
                  .filter(_.inference.id == transitivityDefinition.inference.id)
                  .flatMap(s => transitivityDefinition.splitStatement(s.statement))
                  .orBadRequest("Following step not transitivity")
                (firstSteps, secondSteps, intermediateTerm) <- f(stepContext, transitivityDefinition, targetLhs, targetRhs)
                firstTransitivityStep = transitivityDefinition.assertionStep(mainLhs, targetLhs, intermediateTerm)
                secondTransitivityStep = transitivityDefinition.assertionStep(mainLhs, intermediateTerm, targetRhs)
              } yield before ++ (firstSteps :+ firstTransitivityStep) ++ (secondSteps :+ secondTransitivityStep) ++ restOfSteps
            }
          }.orNotFound(s"Step $stepPath").flatten
        }
    }).toResponseEntity
  }

  trait MaybeSwap {
    def f[T](tuple: (T, T)): (T, T)
    def g[T](l: T, r: T): (T, T) = f((l, r))
  }
  object MaybeSwap {
    val swap: MaybeSwap = new MaybeSwap {
      override def f[T](tuple: (T, T)): (T, T) = tuple.swap
    }
    val dontSwap: MaybeSwap = new MaybeSwap {
      override def f[T](tuple: (T, T)): (T, T) = tuple
    }
  }

  private def insertTransitivityAssertion(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    @RequestBody definition: StepDefinition,
    swapper: MaybeSwap
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepContext, transitivityDefinition, targetLhsUnswapped, targetRhsUnswapped) =>
      val (targetLhs, targetRhs) = swapper.f(targetLhsUnswapped, targetRhsUnswapped)
      implicit val implicitStepContext = stepContext
      import stepContext.entryContext
      for {
        inference <- findInference(definition.inferenceId)(stepContext.entryContext)
        substitutions <- definition.substitutions.parse(inference)

        // Our target is A = C. We're either going to prove A = B or B = C, and add the other part in as a target.
        conclusion <- inference.substituteConclusion(substitutions, stepContext).orBadRequest("Could not apply substitutions to inference")
        function <- for {
          (_, rewrittenConclusion) <- rewriteFromConclusion(conclusion, definition.rewriteInferenceId)(stepContext)
          (rewrittenConclusionLhs, _) <- transitivityDefinition.splitStatement(rewrittenConclusion).map(swapper.f).orBadRequest("Inference conclusion is not a transitive statement")
          function <- targetLhs.getTerms(stepContext).filter(_._1 == rewrittenConclusionLhs).map(_._2).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
        } yield function
        (expansionStep, expandedConclusion) <- if (function.isInstanceOf[FunctionParameter])
          Success((None, conclusion))
        else
          for {
            (conclusionLhs, conclusionRhs) <- transitivityDefinition.splitStatement(conclusion).orBadRequest("Inference conclusion is not a transitive statement")
            expansionDefinition <- entryContext.findExpansion(transitivityDefinition.template).orBadRequest("Could not find expansion inference")
            step = expansionDefinition.assertionStep(conclusionLhs, conclusionRhs, function)
          } yield (
            Some(step),
            step.statement)
        (rewriteStep, rewrittenConclusion) <- rewriteFromPremise(expandedConclusion, definition.rewriteInferenceId)(stepContext)
        (_, intermediateTerm) <- transitivityDefinition.splitStatement(rewrittenConclusion).map(swapper.f).orBadRequest("Rewritten expanded conclusion is not a transitive statement")
        assertionSteps <- ProofHelper.getAssertionWithPremises(inference, substitutions, stepContext, expansionStep.toSeq ++ rewriteStep.toSeq).orBadRequest("Could not apply substitutions to inference")
        newTarget = Step.Target((transitivityDefinition.joinTerms _).tupled.apply(swapper.f((intermediateTerm, targetRhs))))
        (firstSteps, secondSteps) = swapper.f((assertionSteps, Seq(newTarget)))
      } yield (firstSteps, secondSteps, intermediateTerm)
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
    insertTransitivityAssertion(bookKey, chapterKey, theoremKey, proofIndex, stepPath, definition, MaybeSwap.dontSwap)
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
    insertTransitivityAssertion(bookKey, chapterKey, theoremKey, proofIndex, stepPath, definition, MaybeSwap.swap)
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepContext, transitivityDefinition, targetLhs, targetRhs) =>
      implicit val implictStepContext = stepContext
      for {
        intermediateTerm <- Term.parser.parseFromString(serializedTerm, "target term").recoverWithBadRequest
        firstStep = Step.Target(transitivityDefinition.joinTerms(targetLhs, intermediateTerm))
        secondStep = Step.Target(transitivityDefinition.joinTerms(intermediateTerm, targetRhs))
      } yield (Seq(firstStep), Seq(secondStep), intermediateTerm)
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
