package net.prover.model.unwrapping

import net.prover.controllers._
import net.prover.model.definitions.{BinaryJoiner, DeductionDefinition, GeneralizationDefinition, Wrapper}
import net.prover.model.expressions._
import net.prover.model.proof.Step.ExistingStatementExtraction
import net.prover.model.proof._
import net.prover.model.{Inference, ProvingContext, Substitutions}

import scala.util.{Success, Try}

sealed trait Unwrapper {
  def definitionSymbol: String
  def depth: Int
  def inference: Inference
  def enhanceStepContext(implicit stepContext: StepContext): StepContext
  def enhanceStepProvingContext(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.updateStepContext(enhanceStepContext(_))
  }
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement]
  def addToStatement(statement: Statement): Statement
  def extractionStep(result: Statement, depth: Int)(implicit substitutionContext: SubstitutionContext): Step.Assertion
  def rewrap(steps: Seq[Step]): Step
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Try[Seq[Step]]
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement): Option[(Term, Seq[Statement], Statement)]
}

case class GeneralizationUnwrapper(variableName: String, generalizationDefinition: GeneralizationDefinition, specificationInference: Inference) extends Unwrapper {
  val definitionSymbol: String = generalizationDefinition.statementDefinition.symbol
  val depth = 1
  def inference = specificationInference
  def enhanceStepContext(implicit stepContext: StepContext): StepContext = {
    stepContext.addBoundVariable(variableName).atIndex(0)
  }
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement] = {
    Wrapper((t, sc) => addToStatement(wrapper(t)(SubstitutionContext.withExtraParameter(sc))))
  }
  def addToStatement(statement: Statement): Statement = {
    generalizationDefinition(variableName, statement)
  }
  def extractionStep(result: Statement, depth: Int)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    val parameter = FunctionParameter(0, depth)
    val predicate = result.calculateApplicatives(Seq(TermVariable(0, Nil)), Substitutions.Possible(Map.empty, Map(0 -> parameter))).next()._1
    val substitutions = Substitutions(Seq(predicate), Seq(parameter))
    val baseAssertionStep = Step.Assertion.forInference(inference, substitutions).get
    baseAssertionStep.copy(premises = Seq(Premise.Pending(baseAssertionStep.premises.head.statement.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(variableName)))))
  }
  def rewrap(steps: Seq[Step]): Step = {
    Step.Generalization(variableName, steps, generalizationDefinition)
  }
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Try[Seq[Step]] = {
    for {
      distributionInference <- provingContext.generalizationDistributions.get(joiner).orBadRequest(s"Could not find generalization distribution inference for ${joiner.symbol}")
      distributionSubstitutions <- distributionInference.premises.head.calculateSubstitutions(addToStatement(joiner(source, result)(enhanceStepContext(implicitly)))).flatMap(_.confirmTotality(distributionInference.variableDefinitions))
        .orBadRequest("Could not calculate substitutions for generalization distribution inference")
      distributionStep <- Step.Assertion.forInference(distributionInference, distributionSubstitutions).orBadRequest("Could not apply generalization distribution inference")
      generalizationStep = rewrap(steps)
    } yield Seq(generalizationStep, distributionStep)
  }
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement): Option[(Term, Seq[Statement], Statement)] = {
    for {
      removedSource <- source.removeExternalParameters(1)
      removedPremises <- premises.map(_.removeExternalParameters(1)).traverseOption
    } yield (removedSource, removedPremises, addToStatement(wrapperStatement))
  }
}

case class DeductionUnwrapper(antecedent: Statement, deductionDefinition: DeductionDefinition, deductionEliminationInference: Inference) extends Unwrapper {
  val definitionSymbol: String = deductionDefinition.statementDefinition.symbol
  val depth = 0
  def inference = deductionEliminationInference
  def enhanceStepContext(implicit stepContext: StepContext): StepContext = {
    stepContext.addAssumption(antecedent).atIndex(0)
  }
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement] = {
    Wrapper((t, sc) => addToStatement(wrapper(t)(sc)))
  }
  def addToStatement(statement: Statement): Statement = {
    deductionDefinition(antecedent, statement)
  }
  def extractionStep(result: Statement, depth: Int)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    val insertedAntecedent = antecedent.insertExternalParameters(depth)
    val substitutions = Substitutions(Seq(insertedAntecedent, result), Nil)
    Step.Assertion.forInference(deductionEliminationInference, substitutions).get
  }
  def rewrap(steps: Seq[Step]): Step = {
    Step.Deduction(antecedent, steps, deductionDefinition)
  }
  def rewrap(steps: Seq[Step], wrapper: Wrapper[Term, Statement]): (Step, Wrapper[Term, Statement]) = {
    (rewrap(steps), enhanceWrapper(wrapper))
  }
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Try[Seq[Step]] = {
    for {
      distributionInference <- provingContext.deductionDistributions.get(joiner).orBadRequest(s"Could not find deduction distribution inference for ${joiner.symbol}")
      distributionStep <- Step.Assertion.forInference(distributionInference, Substitutions(Seq(antecedent, source, result), Nil)).orBadRequest("Could not apply deduction distribution inference")
      deductionStep = rewrap(steps)
    } yield Seq(deductionStep, distributionStep)
  }
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement): Option[(Term, Seq[Statement], Statement)] = {
    if (!premises.contains(antecedent)) {
      Some((source, premises, addToStatement(wrapperStatement)))
    } else {
      None
    }
  }
}

object Unwrapper {

  implicit class Unwrappers(unwrappers: Seq[Unwrapper]) {
    def depth: Int = unwrappers.map(_.depth).sum

    def enhanceStepContext(implicit stepContext: StepContext): StepContext = {
      unwrappers.foldLeft(stepContext) { case (context, unwrapper) => unwrapper.enhanceStepContext(context) }
    }
    def enhanceStepProvingContext(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
      unwrappers.foldLeft(stepProvingContext) { case (context, unwrapper) => unwrapper.enhanceStepProvingContext(context) }
    }

    def addToStatement(statement: Statement): Statement = {
      unwrappers.foldRight(statement) { _.addToStatement(_) }
    }

    def getTargetExtraction(target: Statement)(implicit stepContext: StepContext): (Statement, Option[Step.PremiseDerivation]) = {
      val enhancedStepContext = enhanceStepContext(stepContext)
      def helper(currentUnwrappers: Seq[Unwrapper]): (Statement, Seq[Step.Assertion], Int) = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val (innerTarget, innerSteps, innerDepth) = helper(tailUnwrappers)
            val newStep = unwrapper.extractionStep(innerTarget, innerDepth)(enhancedStepContext)
            val newTarget = newStep.premises.head.statement
            (newTarget, newStep +: innerSteps, innerDepth + unwrapper.depth)
          case Nil =>
            (target, Nil, 0)
        }
      }
      helper(unwrappers).strip3.mapRight(ExistingStatementExtraction.ifNecessary)
    }

    def rewrap(steps: Seq[Step]): Seq[Step] = {
      unwrappers.foldRight(steps) { case (unwrapper, currentSteps) =>
        Seq(unwrapper.rewrap(currentSteps))
      }
    }

    def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement] = {
      unwrappers.foldRight(wrapper) { case (unwrapper, currentWrapper) =>
        unwrapper.enhanceWrapper(currentWrapper)
      }
    }

    def rewrapWithDistribution(joiner: BinaryJoiner[Statement], source: Term, result: Term, steps: Seq[Step], wrapper: Wrapper[Term, Statement], inference: Option[Inference])(implicit stepContext: StepContext, provingContext: ProvingContext): Try[(Seq[Step], Wrapper[Term, Statement])] = {
      val (sourceStatement, resultStatement) = {
        val enhancedStepContext = enhanceStepContext(implicitly)
        (wrapper(source)(enhancedStepContext), wrapper(result)(enhancedStepContext))
      }

      def helper(currentUnwrappers: Seq[Unwrapper], currentStepContext: StepContext): Try[(Seq[Step], Wrapper[Term, Statement], Statement, Statement)] = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val innerStepContext = unwrapper.enhanceStepContext(currentStepContext)
            for {
              (innerSteps, innerWrapper, innerSource, innerResult) <- helper(tailUnwrappers, innerStepContext)
              rewrappedSteps <- unwrapper.rewrapWithDistribution(innerSteps, joiner, innerSource, innerResult)(currentStepContext, provingContext)
            } yield (rewrappedSteps, unwrapper.enhanceWrapper(innerWrapper), unwrapper.addToStatement(innerSource), unwrapper.addToStatement(innerResult))
          case Nil =>
            Success((Seq(EqualityRewriter.rewriteElider(inference)(steps).get), wrapper, sourceStatement, resultStatement))
        }
      }

      helper(unwrappers, stepContext).map { case (steps, wrapper, _, _) => (steps, wrapper) }
    }

    def removeUnneeded(source: Term, premises: Seq[Statement], wrapper: Statement)(implicit stepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], Statement) = {
      def helper(currentUnwrappers: Seq[Unwrapper], currentStepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], Statement) = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val innerStepContext = unwrapper.enhanceStepContext(currentStepContext)
            val (innerUnwrappers, innerTerm, innerPremises, innerWrapper) = helper(tailUnwrappers, innerStepContext)
            if (innerUnwrappers.isEmpty) {
              unwrapper.remove(innerTerm, innerPremises, innerWrapper) match {
                case Some((removedTerm, removedPremises, removedWrapper)) =>
                  (Nil, removedTerm, removedPremises, removedWrapper)
                case None =>
                  (Seq(unwrapper), innerTerm, innerPremises, innerWrapper)
              }
            } else {
              (unwrapper +: innerUnwrappers, innerTerm, innerPremises, innerWrapper)
            }
          case Nil =>
            (Nil, source, premises, wrapper)
        }
      }

      helper(unwrappers, stepContext)
    }

    def addNecessaryExtractions(step: Step.AssertionOrExtraction, targets: Seq[Statement])(implicit stepContext: StepContext): (Step.InferenceApplicationWithoutPremises, Seq[Statement]) = {
      val newTargetsAndExtractions = targets.map(getTargetExtraction)
      val newTargets = newTargetsAndExtractions.map(_._1)
      val targetExtractionSteps = newTargetsAndExtractions.flatMap(_._2)
      if (unwrappers.nonEmpty || targetExtractionSteps.nonEmpty) {
        (Step.WrappedInferenceApplication(unwrappers, targetExtractionSteps, step), newTargets)
      } else {
        (step, newTargets)
      }
    }
  }
}
