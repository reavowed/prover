package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions._

object ProofHelper {
  def findFact(target: Statement, stepContext: StepContext): Option[Step.Assertion] = {
    stepContext.entryContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target, stepContext).flatMap(_.confirmTotality)
          .map { substitutions =>
            Step.Assertion(target, inference.summary, Nil, substitutions)
          }
      }
  }

  def findNamingInferences(entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement)] = {
    entryContext.inferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i, entryContext).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference, entryContext: EntryContext): Option[(Seq[Statement], Statement)] = {
    (entryContext.scopingDefinitionOption, entryContext.deductionDefinitionOption) match {
      case (Some(scopingDefinition), Some(deductionDefinition)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+
              DefinedStatement(
              Seq(DefinedStatement(
                Seq(assumption: Statement, StatementVariable(deductionConclusionVariableName)),
                `deductionDefinition`
                )),
              `scopingDefinition`),
            StatementVariable(conclusionVariableName)
          ) if deductionConclusionVariableName == conclusionVariableName =>
            Some((initialPremises, assumption))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  def getAssertionWithPremises(
    inference: Inference.Summary,
    substitutions: Substitutions,
    stepContext: StepContext,
    followUpSteps: Seq[Step] = Nil
  ): Option[Seq[Step]] = {
    def elide(steps: Seq[Step]): Option[Step] = WrapElided.wrapAsElidedIfNecessary(steps, inference)
    for {
      premiseStatements <- inference.substitutePremises(substitutions, stepContext)
      conclusion <- inference.substituteConclusion(substitutions, stepContext)
      (targetSteps, premiseSteps) = premiseStatements.foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((targetStepsSoFar, premiseStepsSoFar), premiseStatement) =>
        PremiseFinder.findPremiseSteps(premiseStatement, stepContext) match {
          case Some(newPremiseSteps) =>
            (targetStepsSoFar, premiseStepsSoFar ++ newPremiseSteps)
          case None =>
            val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement, stepContext)
            val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
              PremiseFinder.findPremiseSteps(deconstructedStatement, stepContext) match {
                case Some(newPremiseSteps) =>
                  (otherTargetStepsSoFar, otherPremiseStepsSoFar ++ newPremiseSteps)
                case None =>
                  (otherTargetStepsSoFar :+ Step.Target(deconstructedStatement), otherPremiseStepsSoFar)
              }
            }
            (targetStepsSoFar ++ deconstructionTargetSteps, premiseStepsSoFar ++ deconstructionPremiseSteps ++ deconstructionSteps)
        }
      }
      assertionStep = Step.Assertion(
        conclusion,
        inference,
        premiseStatements.map(Premise.Pending),
        substitutions)
      result <- if (InferenceTypes.isTransitivity(inference)) {
        elide(assertionStep +: followUpSteps).map((targetSteps ++ premiseSteps) :+ _)
      } else {
        elide((premiseSteps :+ assertionStep) ++ followUpSteps).map(targetSteps :+ _)
      }
    } yield result
  }

  object WrapElided {
    def wrapAsElidedIfNecessary(steps: Seq[Step], description: String): Option[Step] = {
      wrapAsElidedIfNecessary(steps, Step.Elided(_, None, Some(description)))
    }
    def wrapAsElidedIfNecessary(steps: Seq[Step], inference: Inference.Summary): Option[Step] = {
      wrapAsElidedIfNecessary(steps, Step.Elided(_, Some(inference), None))
    }
    def wrapAsElidedIfNecessary(steps: Seq[Step], inference: Inference): Option[Step] = {
      wrapAsElidedIfNecessary(steps, inference.summary)
    }
    private def wrapAsElidedIfNecessary(steps: Seq[Step], f: Seq[Step] => Step.Elided): Option[Step] = {
      steps match {
        case Nil =>
          None
        case Seq(singleStep) =>
          Some(singleStep)
        case _ =>
          Some(f(steps))
      }
    }
  }

  trait TransitiveEquality {
    def equalityDefinition: StatementDefinition
    def equalityTransitivityInference: Inference

    def equalityTransitivityStep(a: Term, b: Term, c: Term): Step.Assertion = {
      Step.Assertion(
        equalityDefinition(a, c),
        equalityTransitivityInference.summary,
        Seq(Premise.Pending(equalityDefinition(a, b)), Premise.Pending(equalityDefinition(b, c))),
        Substitutions(terms = equalityTransitivityInference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap))
    }
  }
  object TransitiveEquality {
    def apply(definition: StatementDefinition, transitivityInference: Inference): TransitiveEquality = new TransitiveEquality {
      override def equalityDefinition: StatementDefinition = definition
      override def equalityTransitivityInference: Inference = transitivityInference
    }
  }

  trait ReverseEquality {
    def equalityDefinition: StatementDefinition
    def equalityReversalInference: Inference

    def equalityReversalStep(a: Term, b: Term): Step.Assertion = {
      Step.Assertion(
        equalityDefinition(a, b),
        equalityReversalInference.summary,
        Seq(Premise.Pending(equalityDefinition(b, a))),
        Substitutions(terms = equalityReversalInference.requiredSubstitutions.terms.zip(Seq(b, a)).toMap))
    }
  }

  trait ExpandEquality {
    def equalityDefinition: StatementDefinition
    def equalityExpansionInference: Inference

    def equalityExpansionStep(left: Term, right: Term, function: Term)(implicit stepContext: StepContext): Step.Assertion = {
      Step.Assertion(
        equalityDefinition(function.specify(Seq(left), 0, stepContext.externalDepth), function.specify(Seq(right), 0, stepContext.externalDepth)),
        equalityExpansionInference.summary,
        Seq(Premise.Pending(equalityDefinition(left, right))),
        Substitutions(
          terms = equalityExpansionInference.requiredSubstitutions.terms.zip(Seq(left, right)).toMap,
          functions = equalityExpansionInference.requiredSubstitutions.functions.zip(Seq(function)).toMap))
    }
  }
  object ExpandEquality {
    def apply(definition: StatementDefinition, expansionInference: Inference): ExpandEquality = new ExpandEquality {
      override def equalityDefinition: StatementDefinition = definition
      override def equalityExpansionInference: Inference = expansionInference
    }
  }

}
