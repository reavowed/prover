package net.prover.model.unwrapping

import net.prover.controllers._
import net.prover.model.definitions.Wrapper
import net.prover.model.expressions._
import net.prover.model.proof.Step.ExistingStatementExtractionStep
import net.prover.model.proof._
import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.proving.structure.inferences.{DeductionEliminationInference, SpecificationInference}
import net.prover.proving.structure.statements.BinaryJoiner

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
  def extractionStep(result: Statement, innerUnwrappers: Seq[Unwrapper])(implicit substitutionContext: SubstitutionContext): Step.AssertionStep
  def rewrap(steps: Seq[Step]): Step
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Try[Seq[Step]]
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement): Option[(Term, Seq[Statement], Statement)]
}

case class GeneralizationUnwrapper(variableName: String, generalizationDefinition: GeneralizationDefinition, specificationInference: SpecificationInference) extends Unwrapper {
  val definitionSymbol: String = generalizationDefinition.statementDefinition.symbol
  val depth = 1
  def inference = specificationInference.inference
  def enhanceStepContext(implicit stepContext: StepContext): StepContext = {
    stepContext.addBoundVariable(variableName).forChild()
  }
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement] = {
    Wrapper((t, sc) => addToStatement(wrapper(t)(SubstitutionContext.withExtraParameter(sc))))
  }
  def addToStatement(statement: Statement): Statement = {
    generalizationDefinition(variableName, statement)
  }
  def extractionStep(result: Statement, innerUnwrappers: Seq[Unwrapper])(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    val parameter = FunctionParameter(0, innerUnwrappers.depth)
    val predicate = result.calculateApplicatives(Seq(TermVariable(0, Nil)), Substitutions.Possible(Map.empty, Map(0 -> parameter))).next()._1
    val substitutions = Substitutions(Seq(predicate), Seq(parameter))
    val baseAssertionStep = Step.AssertionStep.forInference(inference, substitutions).get
    baseAssertionStep.copy(premises = Seq(Premise.Pending(baseAssertionStep.premises.head.statement.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(variableName)))))
  }
  def rewrap(steps: Seq[Step]): Step = {
    Step.GeneralizationStep(variableName, steps, generalizationDefinition)
  }
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Try[Seq[Step]] = {
    for {
      distributionInference <- provingContext.generalizationDistributions.get(joiner).orBadRequest(s"Could not find generalization distribution inference for ${joiner.symbol}")
      distributionSubstitutions <- distributionInference.premises.head.calculateSubstitutions(addToStatement(joiner(source, result)(enhanceStepContext(implicitly)))).flatMap(_.confirmTotality(distributionInference.variableDefinitions))
        .orBadRequest("Could not calculate substitutions for generalization distribution inference")
      distributionStep <- Step.AssertionStep.forInference(distributionInference, distributionSubstitutions).orBadRequest("Could not apply generalization distribution inference")
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

case class DeductionUnwrapper(antecedent: Statement, deductionDefinition: DeductionDefinition, deductionEliminationInference: DeductionEliminationInference) extends Unwrapper {
  val definitionSymbol: String = deductionDefinition.statementDefinition.symbol
  val depth = 0
  def inference = deductionEliminationInference.inference
  def enhanceStepContext(implicit stepContext: StepContext): StepContext = {
    stepContext.addAssumption(antecedent).forChild()
  }
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement] = {
    Wrapper((t, sc) => addToStatement(wrapper(t)(sc)))
  }
  def addToStatement(statement: Statement): Statement = {
    deductionDefinition(antecedent, statement)
  }
  def extractionStep(result: Statement, innerUnwrappers: Seq[Unwrapper])(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    val substitutions = Substitutions(Seq(antecedent.insertExternalParameters(innerUnwrappers.depth), result), Nil)
    Step.AssertionStep.forInference(inference, substitutions).get
  }
  def rewrap(steps: Seq[Step]): Step = {
    Step.DeductionStep(antecedent, steps, deductionDefinition)
  }
  def rewrap(steps: Seq[Step], wrapper: Wrapper[Term, Statement]): (Step, Wrapper[Term, Statement]) = {
    (rewrap(steps), enhanceWrapper(wrapper))
  }
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Try[Seq[Step]] = {
    for {
      distributionInference <- provingContext.deductionDistributions.get(joiner).orBadRequest(s"Could not find deduction distribution inference for ${joiner.symbol}")
      distributionStep <- Step.AssertionStep.forInference(distributionInference, Substitutions(Seq(antecedent, source, result), Nil)).orBadRequest("Could not apply deduction distribution inference")
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

    def getTargetExtraction(target: Statement)(implicit stepProvingContext: StepProvingContext): (Option[Statement], Option[Step.PremiseDerivation]) = {
      val enhancedStepProvingContext = enhanceStepProvingContext
      def helper(currentUnwrappers: Seq[Unwrapper]): (Statement, Seq[Step.AssertionStep]) = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val (innerTarget, innerSteps) = helper(tailUnwrappers)
            val newStep = unwrapper.extractionStep(innerTarget, tailUnwrappers)(enhancedStepProvingContext)
            val newTarget = newStep.premises.head.statement
            (newTarget, newStep +: innerSteps)
          case Nil =>
            (target, Nil)
        }
      }
      if (enhancedStepProvingContext.allPremises.exists(_.statement == target)) {
        (None, None)
      } else {
        helper(unwrappers).mapLeft(s => Some(s.removeExternalParameters(unwrappers.depth).get)).mapRight(ExistingStatementExtractionStep.ifNecessary)
      }
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

    def addNecessaryExtractions(step: Step.AssertionOrExtraction, targets: Seq[Statement])(implicit stepProvingContext: StepProvingContext): (Step.InferenceApplicationWithoutPremises, Seq[Statement]) = {
      val newTargetsAndExtractions = targets.map(getTargetExtraction)
      val newTargets = newTargetsAndExtractions.flatMap(_._1)
      val targetExtractionSteps = newTargetsAndExtractions.flatMap(_._2)
      if (unwrappers.nonEmpty || targetExtractionSteps.nonEmpty) {
        (Step.WrappedInferenceApplicationStep(unwrappers, targetExtractionSteps, step), newTargets)
      } else {
        (step, newTargets)
      }
    }
  }
}
