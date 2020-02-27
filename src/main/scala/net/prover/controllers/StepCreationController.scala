package net.prover.controllers

import net.prover.controllers.models.{NamingDefinition, PathData}
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

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
        premise <- stepProvingContext.allPremises.find(_.statement == premiseStatement).orBadRequest(s"Could not find premise '$premiseStatement'")
        newStep <- DefinitionRewriter.rewriteDefinitions(premise.statement, step.statement).orBadRequest("Could not rewrite definition")
      } yield Seq(newStep)
    }.toResponseEntity
  }
}
