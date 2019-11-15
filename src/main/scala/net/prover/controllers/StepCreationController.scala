package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData, StepDefinition}
import net.prover.exceptions.NotFoundException
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof._
import net.prover.model.{ExpressionParsingContext, ProvingContext}
import net.prover.model.definitions.{Transitivity, Wrapper}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification {

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

  @PutMapping
  def createAssertion(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.substitutions.parse(inference)
        (rewriteStep, target) <- rewriteFromConclusion(step.statement, definition.rewriteInferenceId)
        result <- ProofHelper.getAssertionWithPremises(inference, substitutions, rewriteStep.toSeq).orBadRequest("Could not apply substitutions to inference")
        _ <- inference.substituteConclusion(substitutions).filter(_ == target).orBadRequest("Conclusion was incorrect")
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
    replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepReference) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.substitutions.parse(inference)
        stepsForAssertion <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
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
    f: (StepProvingContext, Transitivity, Term, Term) => Try[(Seq[Step], Seq[Step], Term)]
  ): ResponseEntity[_] = {
    (stepPath.indexes match {
      case Nil =>
        Failure(NotFoundException(s"Step $stepPath"))
      case init :+ last =>
        modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
          theorem.modifySteps(proofIndex, init) { (steps, outerStepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              implicit val stepContext = outerStepContext.addSteps(before).atIndex(last)
              implicit val stepProvingContext = StepProvingContext(stepContext, provingContext)
              for {
                targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest(s"Step was not target")
                (targetLhs, targetRhs, transitivityDefinition) <- provingContext.transitivityDefinitions.map(_._2).mapFind { transitivityDefinition =>
                  for {
                    (lhs, rhs) <- transitivityDefinition.relation.unapply(targetStep.statement)
                  } yield (lhs, rhs, transitivityDefinition)
                }.orBadRequest("Target step is not a transitive statement")
                (followingStep, restOfSteps) <- after.headAndTailOption.orBadRequest("No following step")
                (mainLhs, _) <- followingStep.asOptionalInstanceOf[Step.Assertion]
                  .filter(_.inference.id == transitivityDefinition.inference.id)
                  .flatMap(s => transitivityDefinition.relation.unapply(s.statement))
                  .orBadRequest("Following step not transitivity")
                (firstSteps, secondSteps, intermediateTerm) <- f(stepProvingContext, transitivityDefinition, targetLhs, targetRhs)
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhsUnswapped, targetRhsUnswapped) =>
      val (targetLhs, targetRhs) = swapper.f(targetLhsUnswapped, targetRhsUnswapped)
      implicit val spc = stepProvingContext
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.substitutions.parse(inference)
        // Our target is A = C. We're either going to prove A = B or B = C, and add the other part in as a target.
        conclusion <- inference.substituteConclusion(substitutions).orBadRequest("Could not apply substitutions to inference")
        wrapper <- for {
          (_, rewrittenConclusion) <- rewriteFromConclusion(conclusion, definition.rewriteInferenceId)
          (rewrittenConclusionLhs, _) <- transitivity.relation.unapply(rewrittenConclusion).map(swapper.f).orBadRequest("Inference conclusion is not a transitive statement")
          function <- targetLhs.getTerms(stepProvingContext.stepContext).filter(_._1 == rewrittenConclusionLhs).map(_._2).map(Wrapper.fromFunction).single.orBadRequest("Could not find conclusion LHS uniquely in target LHS")
        } yield function
        (expansionStep, expandedConclusion) <- if (wrapper.isIdentity)
          Success((None, conclusion))
        else
          for {
            (conclusionLhs, conclusionRhs) <- transitivity.relation.unapply(conclusion).orBadRequest("Inference conclusion is not a transitive statement")
            expansionDefinition <- stepProvingContext.provingContext.definitionsByRelation.get(transitivity.relation).flatMap(_.expansion).orBadRequest("Could not find expansion inference")
            step = expansionDefinition.assertionStep(conclusionLhs, conclusionRhs, wrapper)
          } yield (
            Some(step),
            step.statement)
        (rewriteStep, rewrittenConclusion) <- rewriteFromPremise(expandedConclusion, definition.rewriteInferenceId)
        (_, intermediateTerm) <- transitivity.relation.unapply(rewrittenConclusion).map(swapper.f).orBadRequest("Rewritten expanded conclusion is not a transitive statement")
        assertionSteps <- ProofHelper.getAssertionWithPremises(inference, substitutions, expansionStep.toSeq ++ rewriteStep.toSeq).orBadRequest("Could not apply substitutions to inference")
        newTarget = Step.Target((transitivity.relation.apply _).tupled.apply(swapper.f((intermediateTerm, targetRhs))))
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
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        intermediateTerm <- Term.parser.parseFromString(serializedTerm, "target term").recoverWithBadRequest
        firstStep = Step.Target(transitivity.relation(targetLhs, intermediateTerm))
        secondStep = Step.Target(transitivity.relation(intermediateTerm, targetRhs))
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        inference <- findInference(definition.inferenceId)
        (namingPremises, assumption) <- ProofHelper.getNamingPremisesAndAssumption(inference).orBadRequest(s"Inference ${definition.inferenceId} is not a naming inference")
        substitutions <- definition.substitutions.parse(inference)
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

  @PostMapping(value = Array("/extract"), produces = Array("application/json;charset=UTF-8"))
  def extract(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        newStep <- SubstatementExtractor.extract(step.statement)(stepProvingContext).orBadRequest(s"Could not extract statement ${step.statement}")
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      for {
        newStep <- TermRearranger.rearrange(step.statement)(stepProvingContext).orBadRequest(s"Could not rearrange statement ${step.statement}")
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
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      EqualityRewriter.rewrite(step.statement)(stepProvingContext)
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
    replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      implicit val spc = stepProvingContext
      for {
        targetStatement <- Statement.parser.parseFromString(serializedStatement, "target statement").recoverWithBadRequest
        targetStep = Step.Target(targetStatement)
      } yield Seq(targetStep, step)
    }.toResponseEntity
  }
}
