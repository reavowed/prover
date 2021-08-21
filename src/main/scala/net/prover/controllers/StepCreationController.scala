package net.prover.controllers

import net.prover._
import net.prover.chaining.ChainingStepEditing
import net.prover.controllers.models.{NamingDefinition, PathData}
import net.prover.extensions.ExpressionExtensions._
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof._
import net.prover.old.OldParameterInserter
import net.prover.structure.BookService
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepCreationController @Autowired() (val bookService: BookService) extends BookModification with ChainingStepEditing {

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
        (namingPremises, assumption, generalizationDefinition, deductionDefinition) <- ProofHelper.getNamingPremisesAndAssumption(inference).orBadRequest(s"Inference ${definition.inferenceId} is not a naming inference")
        substitutions <- definition.substitutions.parse(inference.variableDefinitions)
        _ <- inference.substituteConclusion(substitutions).filter(_ == step.statement).orBadRequest("Conclusion was incorrect")
        premiseStatements <- namingPremises.map(_.applySubstitutions(substitutions)).traverseTry.orBadRequest("Could not substitute premises")
        substitutedAssumption <- assumption.applySubstitutionsInsideStep(substitutions)(stepProvingContext.stepContext).orBadRequest("Could not substitute assumption")
      } yield {
        val premises = premiseStatements.map(stepProvingContext.createPremise)
        val targetSteps = premises.ofType[Premise.Pending].map(p => spc.provingContext.factsBySerializedStatement.get(p.statement.serialized).map(_.step).getOrElse(Step.Target(p.statement)))
        targetSteps :+ Step.Naming(
          variableName,
          substitutedAssumption,
          step.statement,
          Seq(Step.Target(OldParameterInserter.insertParameters(step.statement, 1, 0))),
          inference,
          premises,
          substitutions,
          generalizationDefinition,
          deductionDefinition)
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
        variableName <- premiseStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).orBadRequest("Premise did not have single bound variable")
        (namingInference, namingInferenceAssumption, substitutionsAfterPremise, generalizationDefinition, deductionDefinition) <- ProofHelper.findNamingInferences.mapFind {
          case (i, Seq(singlePremise), a, generalizationDefinition, deductionDefinition) =>
            PossibleSubstitutionCalculator.calculatePossibleSubstitutions(singlePremise, premiseStatement).map { s => (i, a, s, generalizationDefinition, deductionDefinition) }
          case _ =>
            None
        }.orBadRequest("Could not find naming inference matching premise")
        substitutionsAfterConclusion <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(namingInference.conclusion, targetStep.statement, substitutionsAfterPremise).orBadRequest("Could not calculate substitutions for conclusion")
        substitutions <- substitutionsAfterConclusion.confirmTotality(namingInference.variableDefinitions).orBadRequest("Substitutions for naming inference were not total")
        substitutedAssumption <- namingInferenceAssumption.applySubstitutionsInsideStep(substitutions)(stepProvingContext.stepContext).orBadRequest("Could not substitute assumption")
      } yield {
        Seq(Step.Naming(
          variableName,
          substitutedAssumption,
          targetStep.statement,
          Seq(Step.Target(OldParameterInserter.insertParameters(targetStep.statement, 1, 0))),
          namingInference.summary,
          Seq(premise),
          substitutions,
          generalizationDefinition,
          deductionDefinition))
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
        generalizationDefinition <- stepProvingContext.provingContext.generalizationDefinitionOption.orBadRequest("No generalization definition provided")
        (variableName, predicate) <- generalizationDefinition.unapply(step.statement).orBadRequest("Target statement is not a generalized statement")
      } yield {
        Step.Generalization(
          variableName,
          Seq(Step.Target(predicate)),
          generalizationDefinition)
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
        deductionDefinition <- stepProvingContext.provingContext.deductionDefinitionOption.orBadRequest("No deduction definition provided")
        (antecedent, consequent) <- deductionDefinition.unapply(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/introduceAll"))
  def introduceAll(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.modifyStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (step, stepProvingContext) =>
      def getNestedIntroductionStepForStatement(statement: Statement): Step = {
        def byGeneralization = for {
          generalizationDefinition <- stepProvingContext.provingContext.generalizationDefinitionOption
          (variableName, predicate) <- generalizationDefinition.unapply(statement)
        } yield Step.Generalization(variableName, Seq(getNestedIntroductionStepForStatement(predicate)), generalizationDefinition)
        def byDeduction = for {
          deductionDefinition <- stepProvingContext.provingContext.deductionDefinitionOption
          (antecedent, consequent) <- deductionDefinition.unapply(statement)
        } yield Step.Deduction(antecedent, Seq(getNestedIntroductionStepForStatement(consequent)), deductionDefinition)
        byGeneralization orElse byDeduction getOrElse Step.Target(statement)
      }
      Success(getNestedIntroductionStepForStatement(step.statement))
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
    addBeforeTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { stepProvingContext =>
      for {
        targetStatement <- Statement.parser(stepProvingContext).parseFromString(serializedStatement, "target statement").recoverWithBadRequest
      } yield Seq(Step.Target(targetStatement))
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
        premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        newStep <- DefinitionRewriter.rewriteDefinitions(premise.statement, step.statement).orBadRequest("Could not rewrite definition")
      } yield Seq(newStep)
    }.toResponseEntity
  }
}
