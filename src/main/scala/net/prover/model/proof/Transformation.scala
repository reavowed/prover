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

  def applyFully(inference: Inference): Option[(Seq[Statement], Statement)] = {
    getSubstitutions(inference).flatMap(applyFully(inference, _))
  }

  def applyFully(inference: Inference, transformationSubstitutions: Substitutions): Option[(Seq[Statement], Statement)] = {
    for {
      conclusionStatementToProve <- inference.conclusion.applySubstitutions(transformationSubstitutions, 0, 0)
      transformedConclusion = generalise(conclusionStatementToProve)
      transformedPremises <- inference.premises.map { premise =>
        premise.applySubstitutions(transformationSubstitutions, 0, 0).map { statementToProve =>
          generalise(statementToProve)
        }
      }.traverseOption
    } yield (transformedPremises, transformedConclusion)
  }

  def applyPartially(inference: Inference,  transformationSubstitutions: Substitutions): Option[(Seq[Seq[Statement]], Statement)] = {
    for {
      transformedConclusion <- inference.conclusion.applySubstitutions(transformationSubstitutions, 0, 0).map(specify)
      transformedPremises <- inference.premises.map { premise =>
        premise.applySubstitutions(transformationSubstitutions, 0, 0).map { statement =>
          Seq(
            generalise(statement),
            specify(statement))
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
          Seq(DefinedStatement(Seq(PredicateApplication(name1, Seq(FunctionParameter(0, 0)))), `scopingStatement`)),
          PredicateApplication(name2, Seq(TermVariable(_)))
        ) if name1 == name2 =>
          true
        case _ =>
          false
      }
    } yield Transformation(scopingStatement, boundVariableName, specificationInference)
  }
}
