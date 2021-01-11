package net.prover.model

import net.prover.core.expressions._
import net.prover.core.substitutions.Substitutions
import net.prover.model.proof.{Premise, Step}
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.Theorem
import net.prover.structure.model.entries.Theorem.Proof

import scala.reflect.ClassTag

case class ExpressionDefinitionReplacements(map: Map[CompoundExpressionType, CompoundExpressionType], entryContext: EntryContext) {
  def replaceDefinitions(expression: Expression): Expression = expression match {
    case statement: Statement =>
      replaceDefinitions(statement)
    case term: Term =>
      replaceDefinitions(term)
  }
  def replaceDefinitions(statement: Statement): Statement = statement match {
    case statementVariable: StatementVariable =>
      replaceVariableDefinitions(statementVariable)
    case compoundStatement: CompoundStatement =>
      replaceDefinitions(compoundStatement)
  }
  def replaceDefinitions(term: Term): Term = {
    case termVariable: TermVariable =>
      replaceVariableDefinitions(termVariable)
    case compoundStatement: CompoundStatement =>
      replaceDefinitions(compoundStatement)
    case parameter: Parameter =>
      parameter
  }

  def replaceVariableDefinitions[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
    variable: TVariable
  ): TVariable = {
    variable.withNewArguments(variable.arguments.map(replaceDefinitions))
  }
  def replaceDefinitions(compoundStatement: CompoundStatement): CompoundStatement = {
    CompoundStatement(
      map(compoundStatement.definition).asInstanceOf[CompoundStatementType],
      compoundStatement.components.map(replaceDefinitions))(
      compoundStatement.boundVariableNames)
  }
  def replaceDefinitions(compoundTerm: CompoundTerm): CompoundTerm = {
    CompoundTerm(
      map(compoundTerm.definition).asInstanceOf[CompoundTermType],
      compoundTerm.components.map(replaceDefinitions))(
      compoundTerm.boundVariableNames)
  }

  def replaceDefinitions(theorem: Theorem): Theorem = {
    Theorem(
      theorem.name,
      theorem.variableDefinitions,
      theorem.premises.map(replaceDefinitions),
      replaceDefinitions(theorem.conclusion),
      theorem.proofs.map(replaceDefinitions))
  }
  def replaceDefinitions(proof: Proof): Proof = {
    Proof(proof.steps.map(replaceDefinitions))
  }
  def replaceDefinitions(step: Step): Step = step match {
      case targetStep: Step.Target =>
        Step.Target(replaceDefinitions(targetStep.statement))
      case assertionStep: Step.Assertion =>
        Step.Assertion(
          replaceDefinitions(assertionStep.statement),
          replaceDefinitions(assertionStep.inference),
          assertionStep.premises.map(replaceDefinitions),
          replaceDefinitions(assertionStep.substitutions))
      case deductionStep: Step.Deduction =>
        Step.Deduction(
          replaceDefinitions(deductionStep.assumption),
          deductionStep.substeps.map(replaceDefinitions),
          entryContext.deductionDefinitionOption.get)
      case generalizationStep: Step.Generalization =>
        Step.Generalization(
          generalizationStep.variableName,
          generalizationStep.substeps.map(replaceDefinitions),
          entryContext.generalizationDefinitionOption.get)
      case namingStep: Step.Naming =>
        Step.Naming(
          namingStep.variableName,
          replaceDefinitions(namingStep.assumption),
          replaceDefinitions(namingStep.statement),
          namingStep.substeps.map(replaceDefinitions),
          replaceDefinitions(namingStep.inference),
          namingStep.premises.map(replaceDefinitions),
          replaceDefinitions(namingStep.substitutions),
          entryContext.generalizationDefinitionOption.get,
          entryContext.deductionDefinitionOption.get)
      case elidedStep: Step.Elided =>
        Step.Elided(
          elidedStep.substeps.map(replaceDefinitions),
          elidedStep.highlightedInference.map(replaceDefinitions),
          elidedStep.description)
      case subproofStep: Step.SubProof =>
        Step.SubProof(
          subproofStep.name,
          subproofStep.substeps.map(replaceDefinitions))
  }

  def replaceDefinitions(premise: Premise): Premise = premise match {
    case pending: Premise.Pending =>
      Premise.Pending(replaceDefinitions(pending.statement))
    case given: Premise.Given =>
      Premise.Given(replaceDefinitions(given.statement), given.referencedLine)
    case simplification: Premise.Simplification =>
      Premise.Simplification(
        replaceDefinitions(simplification.statement),
        replaceDefinitions(simplification.premise).asInstanceOf[Premise.SingleLinePremise],
        replaceDefinitions(simplification.inference),
        replaceDefinitions(simplification.substitutions),
        simplification.path)
  }

  def replaceDefinitions(inference: Inference.Summary): Inference.Summary = {
      val newPremises = inference.premises.map(replaceDefinitions)
      val newConclusion = replaceDefinitions(inference.conclusion)
      Inference.Summary(
        inference.name,
        Inference.calculateHash(newPremises, newConclusion),
        inference.variableDefinitions,
        newPremises,
        newConclusion)
  }

  def replaceDefinitions(substitutions: Substitutions): Substitutions = {
    Substitutions(
      substitutions.statements.map(replaceDefinitions),
      substitutions.terms.map(replaceDefinitions))
  }
}
