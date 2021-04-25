package net.prover.model.unwrapping

import net.prover.controllers._
import net.prover.model.definitions.{BinaryJoiner, DeductionDefinition, GeneralizationDefinition, Wrapper}
import net.prover.model.expressions._
import net.prover.model.proof._
import net.prover.model.{Inference, Substitutions}
import net.prover.old.OldParameterInserter
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.{ApplicativeCalculator, ParameterRemover, PossibleSubstitutionCalculator}

import scala.util.{Success, Try}

sealed trait Unwrapper {
  def definitionSymbol: String
  def depth: Int
  def inference: Inference
  def enhanceContext(stepContext: StepContext): StepContext
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement]
  def addToStatement(statement: Statement): Statement
  def extractionStep(result: Statement, depth: Int)(implicit substitutionContext: SubstitutionContext): Step.Assertion
  def rewrap(steps: Seq[Step]): Step
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepProvingContext: StepProvingContext): Try[Seq[Step]]
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement)(implicit stepContext: StepContext): Option[(Term, Seq[Statement], Statement)]
}

case class GeneralizationUnwrapper(variableName: String, generalizationDefinition: GeneralizationDefinition, specificationInference: Inference) extends Unwrapper {
  val definitionSymbol: String = generalizationDefinition.statementDefinition.symbol
  val depth = 1
  def inference = specificationInference
  def enhanceContext(stepContext: StepContext): StepContext = {
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
    val predicate = ApplicativeCalculator.calculateApplicatives(result, Seq(TermVariable(0, Nil)), PossibleSubstitutions(Map.empty, Map(0 -> parameter))).next()._1
    val substitutions = Substitutions(Seq(predicate), Seq(parameter))
    val baseAssertionStep = Step.Assertion.forInference(inference, substitutions).get
    baseAssertionStep.copy(premises = Seq(Premise.Pending(baseAssertionStep.premises.head.statement.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(variableName)))))
  }
  def rewrap(steps: Seq[Step]): Step = {
    Step.Generalization(variableName, steps, generalizationDefinition)
  }
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepProvingContext: StepProvingContext): Try[Seq[Step]] = {
    for {
      distributionInference <- stepProvingContext.provingContext.generalizationDistributions.get(joiner).orBadRequest(s"Could not find generalization distribution inference for ${joiner.symbol}")
      distributionSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(distributionInference.premises.head, addToStatement(joiner(source, result)(enhanceContext(implicitly))))
        .flatMap(_.confirmTotality(distributionInference.variableDefinitions))
        .orBadRequest("Could not calculate substitutions for generalization distribution inference")
      distributionStep <- Step.Assertion.forInference(distributionInference, distributionSubstitutions).orBadRequest("Could not apply generalization distribution inference")
      generalizationStep = rewrap(steps)
    } yield Seq(generalizationStep, distributionStep)
  }
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement)(implicit stepContext: StepContext): Option[(Term, Seq[Statement], Statement)] = {
    for {
      removedSource <- ParameterRemover.removeParameters(source, 1, 0)
      removedPremises <- premises.map(ParameterRemover.removeParameters(_, 1, 0)).traverseOption
    } yield (removedSource, removedPremises, addToStatement(wrapperStatement))
  }
}

case class DeductionUnwrapper(antecedent: Statement, deductionDefinition: DeductionDefinition, deductionEliminationInference: Inference) extends Unwrapper {
  val definitionSymbol: String = deductionDefinition.statementDefinition.symbol
  val depth = 0
  def inference = deductionEliminationInference
  def enhanceContext(stepContext: StepContext): StepContext = {
    stepContext.addStatement(antecedent, "a").atIndex(0)
  }
  def enhanceWrapper(wrapper: Wrapper[Term, Statement]): Wrapper[Term, Statement] = {
    Wrapper((t, sc) => addToStatement(wrapper(t)(sc)))
  }
  def addToStatement(statement: Statement): Statement = {
    deductionDefinition(antecedent, statement)
  }
  def extractionStep(result: Statement, depth: Int)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    val insertedAntecedent = OldParameterInserter.insertParameters(antecedent, depth, 0)
    val substitutions = Substitutions(Seq(insertedAntecedent, result), Nil)
    Step.Assertion.forInference(deductionEliminationInference, substitutions).get
  }
  def rewrap(steps: Seq[Step]): Step = {
    Step.Deduction(antecedent, steps, deductionDefinition)
  }
  def rewrap(steps: Seq[Step], wrapper: Wrapper[Term, Statement]): (Step, Wrapper[Term, Statement]) = {
    (rewrap(steps), enhanceWrapper(wrapper))
  }
  def rewrapWithDistribution(steps: Seq[Step], joiner: BinaryJoiner[Statement], source: Statement, result: Statement)(implicit stepProvingContext: StepProvingContext): Try[Seq[Step]] = {
    for {
      distributionInference <- stepProvingContext.provingContext.deductionDistributions.get(joiner).orBadRequest(s"Could not find deduction distribution inference for ${joiner.symbol}")
      distributionStep <- Step.Assertion.forInference(distributionInference, Substitutions(Seq(antecedent, source, result), Nil)).orBadRequest("Could not apply deduction distribution inference")
      deductionStep = rewrap(steps)
    } yield Seq(deductionStep, distributionStep)
  }
  def remove(source: Term, premises: Seq[Statement], wrapperStatement: Statement)(implicit stepContext: StepContext): Option[(Term, Seq[Statement], Statement)] = {
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

    def enhanceStepContext(stepContext: StepContext): StepContext = {
      unwrappers.foldLeft(stepContext) { case (context, unwrapper) => unwrapper.enhanceContext(context) }
    }
    def enhanceStepProvingContext(stepProvingContext: StepProvingContext): StepProvingContext = {
      stepProvingContext.updateStepContext(enhanceStepContext)
    }

    def getExtractionSteps(premise: Statement, steps: Seq[Step], inference: Inference)(implicit stepContext: StepContext): Seq[Step] = {
      val enhancedContext = enhanceStepContext(stepContext)

      def helper(currentUnwrappers: Seq[Unwrapper]): (Seq[Step], Statement, Int) = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val (innerSteps, innerStatement, innerDepth) = helper(tailUnwrappers)
            val newStep = unwrapper.extractionStep(innerStatement, innerDepth)(enhancedContext)
            (newStep +: innerSteps, newStep.premises(0).statement, innerDepth + unwrapper.depth)
          case Nil =>
            (Nil, premise, 0)
        }
      }

      val (extractionSteps, _, _) = helper(unwrappers)
      Step.Elided.ifNecessary(extractionSteps, "Extracted") match {
        case Some(extractionStep) =>
          Seq(extractionStep, EqualityRewriter.rewriteElider(Some(inference))(steps).get)
        case None =>
          steps
      }
    }

    def rewrap(steps: Seq[Step])(implicit stepContext: StepContext): Seq[Step] = {
      unwrappers.foldRight(steps) { case (unwrapper, currentSteps) =>
        Seq(unwrapper.rewrap(currentSteps))
      }
    }

    def enhanceWrapper(wrapper: Wrapper[Term, Statement])(implicit stepContext: StepContext): Wrapper[Term, Statement] = {
      unwrappers.foldRight(wrapper) { case (unwrapper, currentWrapper) =>
        unwrapper.enhanceWrapper(currentWrapper)
      }
    }

    def rewrapWithDistribution(joiner: BinaryJoiner[Statement], source: Term, result: Term, steps: Seq[Step], wrapper: Wrapper[Term, Statement], inference: Option[Inference])(implicit stepProvingContext: StepProvingContext): Try[(Seq[Step], Wrapper[Term, Statement])] = {
      val (sourceStatement, resultStatement) = {
        implicit val enhancedStepContext = enhanceStepContext(implicitly)
        (wrapper(source), wrapper(result))
      }

      def helper(currentUnwrappers: Seq[Unwrapper], currentStepContext: StepContext): Try[(Seq[Step], Wrapper[Term, Statement], Statement, Statement)] = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val innerStepContext = unwrapper.enhanceContext(currentStepContext)
            for {
              (innerSteps, innerWrapper, innerSource, innerResult) <- helper(tailUnwrappers, innerStepContext)
              rewrappedSteps <- unwrapper.rewrapWithDistribution(innerSteps, joiner, innerSource, innerResult)(StepProvingContext.withStepContext(currentStepContext))
            } yield (rewrappedSteps, unwrapper.enhanceWrapper(innerWrapper), unwrapper.addToStatement(innerSource), unwrapper.addToStatement(innerResult))
          case Nil =>
            Success((Seq(EqualityRewriter.rewriteElider(inference)(steps).get), wrapper, sourceStatement, resultStatement))
        }
      }

      helper(unwrappers, stepProvingContext.stepContext).map { case (steps, wrapper, _, _) => (steps, wrapper) }
    }

    def removeUnneeded(source: Term, premises: Seq[Statement], wrapper: Statement)(implicit stepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], Statement) = {
      def helper(currentUnwrappers: Seq[Unwrapper], currentStepContext: StepContext): (Seq[Unwrapper], Term, Seq[Statement], Statement) = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val innerStepContext = unwrapper.enhanceContext(currentStepContext)
            val (innerUnwrappers, innerTerm, innerPremises, innerWrapper) = helper(tailUnwrappers, innerStepContext)
            if (innerUnwrappers.isEmpty) {
              unwrapper.remove(innerTerm, innerPremises, innerWrapper)(currentStepContext) match {
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

    def addNecessaryExtractions(statement: Statement, step: Step, targets: Seq[Step.Target])(implicit stepContext: StepContext): (Statement, Seq[Step], Seq[Step.Target]) = {
      val innermostContext = enhanceStepContext(stepContext)
      def getTargetExtraction(target: Statement): (Statement, Option[Step]) = {
        def helper(currentUnwrappers: Seq[Unwrapper]): (Statement, Seq[Step], Int) = {
          currentUnwrappers match {
            case unwrapper +: tailUnwrappers =>
              val (innerTarget, innerSteps, innerDepth) = helper(tailUnwrappers)
              val newStep = unwrapper.extractionStep(innerTarget, innerDepth)(innermostContext)
              val newTarget = newStep.premises.head.statement
              (newTarget, newStep +: innerSteps, innerDepth + unwrapper.depth)
            case Nil =>
              (target, Nil, 0)
          }
        }
        val (newTarget, extractionSteps, _) = helper(unwrappers)
        val stepOption = Step.Elided.ifNecessary(extractionSteps, "Extracted")
        (newTarget, stepOption)
      }

      val newTargetsAndExtractions = targets.map(t => getTargetExtraction(t.statement))
      val newTargets = newTargetsAndExtractions.map(_._1).map(Step.Target(_))
      val targetExtractionSteps = newTargetsAndExtractions.flatMap(_._2)

      def helper(currentUnwrappers: Seq[Unwrapper]): (Statement, Seq[Step]) = {
        currentUnwrappers match {
          case unwrapper +: tailUnwrappers =>
            val (innerStatement, innerSteps) = helper(tailUnwrappers)
            (unwrapper.addToStatement(innerStatement), Seq(unwrapper.rewrap(innerSteps)))
          case Nil =>
            (statement, targetExtractionSteps :+ step)
        }
      }
      val (wrappedStatement, wrappedSteps) = helper(unwrappers)
      (wrappedStatement, wrappedSteps, newTargets)
    }
  }
}
