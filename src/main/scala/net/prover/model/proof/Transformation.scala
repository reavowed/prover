package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions._

case class Transformation(statementDefinition: StatementDefinition, boundVariableName: String, specificationInference: Inference) {
  def generalise(statement: Statement): Statement = {
    DefinedStatement(Seq(statement), statementDefinition)(statementDefinition.boundVariableNames)
  }
  def specify(statement: Statement): Statement = {
    statement.specify(Seq(TermVariable("_")), 0, 0)
  }

  def getSubstitutions(inference: Inference): Option[Substitutions] = {
    val requiredSubstitutions = inference.requiredSubstitutions
    if (requiredSubstitutions.terms.nonEmpty || requiredSubstitutions.predicates.nonEmpty || requiredSubstitutions.functions.nonEmpty)
      None
    else
      Some(Substitutions(
        statements = requiredSubstitutions.statements.map { name =>
            name -> PredicateApplication(name, Seq(FunctionParameter(0, 0)))
        }.toMap))
  }

  def applyFully(inference: Inference): Option[(Seq[Premise], Statement)] = {
    getSubstitutions(inference).flatMap(applyFully(inference, _))
  }

  def applyFully(inference: Inference, transformationSubstitutions: Substitutions): Option[(Seq[Premise], Statement)] = {
    for {
      conclusionStatementToProve <- inference.conclusion.applySubstitutions(transformationSubstitutions, 0, 0)
      transformedConclusion = generalise(conclusionStatementToProve)
      transformedPremises <- inference.premises.map { premise =>
        premise.statement.applySubstitutions(transformationSubstitutions, 0, 0).map { statementToProve =>
          premise.withStatement(generalise(statementToProve))
        }
      }.traverseOption
    } yield (transformedPremises, transformedConclusion)
  }

  def applyPartially(inference: Inference,  transformationSubstitutions: Substitutions): Option[(Seq[Seq[Premise]], Statement)] = {
    for {
      transformedConclusion <- inference.conclusion.applySubstitutions(transformationSubstitutions, 0, 0).map(specify)
      transformedPremises <- inference.premises.map { premise =>
        premise.statement.applySubstitutions(transformationSubstitutions, 0, 0).map { statement =>
          Seq(
            premise.withStatement(generalise(statement)),
            premise.withStatement(specify(statement)))
        }
      }.traverseOption
    } yield (transformedPremises, transformedConclusion)
  }
}

object Transformation {
  def find(scopingStatement: StatementDefinition, inferences: Seq[Inference]): Option[Transformation] = {
    for {
      boundVariableName <- scopingStatement.boundVariableNames.single
      specificationInference <- inferences.find {
        case Inference(
          _,
          Seq(Premise(DefinedStatement(Seq(PredicateApplication(name1, Seq(FunctionParameter(0, 0)))), `scopingStatement`), 0)),
          PredicateApplication(name2, Seq(TermVariable(_)))
        ) if name1 == name2 =>
          true
        case _ =>
          false
      }
    } yield Transformation(scopingStatement, boundVariableName, specificationInference)
  }
}
