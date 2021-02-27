package net.prover.core.proof

import net.prover.core.RuleOfInference
import net.prover.core.expressions.Statement
import net.prover._
import net.prover.core.substitutions.SubstitutionApplier
import net.prover.core.transformers.{InsertionParameters, ParameterInserter}

import scala.util.{Failure, Success, Try}

object ProofValidator {
  case class ProofValidationContext(knownStatements: Set[Statement], rulesOfInference: Seq[RuleOfInference], externalDepth: Int) {
    def addKnownStatement(knownStatement: Statement): ProofValidationContext = {
      ProofValidationContext(knownStatements + knownStatement, rulesOfInference, externalDepth)
    }
    def addBoundVariable(): ProofValidationContext = {
      ProofValidationContext(knownStatements.map(ParameterInserter.insertParameters(_, 1, 0)), rulesOfInference, externalDepth + 1)
    }
    def validatePremise(premise: Statement): Try[Unit] = {
      knownStatements.contains(premise).orExceptionWithMessage(s"Required premise $premise not found")
    }
  }

  def validate(premises: Seq[Statement], conclusion: Statement, steps: Seq[Step], rulesOfInference: Seq[RuleOfInference]): Try[Any] = {
    for {
      _ <- validateSteps(steps)(ProofValidationContext(premises.toSet, rulesOfInference, 0))
      lastStep <- steps.lastOption match {
        case Some(s) => Success(s)
        case None => Failure(new Exception("Proof must have at least one step"))
      }
      _ <- if (lastStep.statement == conclusion) Success(()) else Failure(new Exception(s"Final proven statement ${lastStep.statement} did not match expected conclusion $conclusion"))
    } yield ()
  }

  private def validateSteps(steps: Seq[Step])(implicit context: ProofValidationContext): Try[Any] = {
    steps.foldLeft(Try(context)) { case (acc, step) =>
      for {
        context <- acc
        _ <- validateStep(step)(context)
      } yield context.addKnownStatement(step.statement)
    }
  }

  private def validateStep(step: Step)(implicit context: ProofValidationContext): Try[Any] = {
    step match {
      case RuleOfInferenceApplicationStep(statement, ruleOfInference, substitutions) =>
        for {
          substitutedRule <- SubstitutionApplier.applySubstitutions(ruleOfInference, substitutions)
          _ <- substitutedRule.premises.map(context.validatePremise).traverseTry
          _ <- (substitutedRule.conclusion == statement).orExceptionWithMessage(s"Conclusion ${substitutedRule.conclusion} did not match statement to be proved $statement")
        } yield ()
      case ImplicationIntroductionStep(antecedent, substeps, _) =>
        val innerContext = context.addKnownStatement(antecedent)
        for {
          _ <- validateSteps(substeps)(innerContext)
        } yield ()
      case UniversalQuantificationIntroductionStep(_, substeps, _) =>
        val innerContext = context.addBoundVariable()
        for {
          _ <- validateSteps(substeps)(innerContext)
        } yield ()
    }
  }

}
