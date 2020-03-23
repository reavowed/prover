package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model._
import net.prover.model.definitions.{Equality, RearrangementStep, TermRewriteInference, Wrapper}
import net.prover.model.expressions._
import net.prover.util.{Direction, PossibleSingleMatch}

import scala.Ordering.Implicits._

case class EqualityRewriter(equality: Equality)(implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext.provingContext._

  case class SimplificationStepWithInference(result: Term, rearrangementStep: RearrangementStep[Term], inference: Inference.Summary)
  case class RearrangementStepWithInference(rearrangementStep: RearrangementStep[Term], inference: Option[Inference.Summary])
  case class StepWithInference(step: Step, inference: Option[Inference.Summary])

  def rewrite(targetStatement: Statement): Option[Step] = {
    def findSimplificationsFromInferences(premiseTerm: Term, inferences: Seq[TermRewriteInference], direction: Direction, wrapper: Wrapper[Term, Term]): Seq[SimplificationStepWithInference] = {
      for {
        TermRewriteInference(inference, extractionOption, left, right) <- inferences
        (inferenceSource, inferenceResult) = direction.swapSourceAndResult(left, right)
        conclusionSubstitutions <- inferenceSource.calculateSubstitutions(premiseTerm).flatMap(_.confirmTotality)
        simplifiedTerm <- inferenceResult.applySubstitutions(conclusionSubstitutions).flatMap(_.asOptionalInstanceOf[Term])
        (premiseSteps, _, possibleFinalSubstitutions) <- PremiseFinder.findPremiseStepsForStatementsBySubstituting(extractionOption.premises, conclusionSubstitutions)
        finalSubstitutions <- possibleFinalSubstitutions.confirmTotality
        (source, result) = direction.swapSourceAndResult(premiseTerm, simplifiedTerm)
        assertionStep <- Step.Assertion.forInference(inference, finalSubstitutions)
        ExtractionApplication(_, _, extractionSteps, _, _) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionOption.extractionInferences, inference, finalSubstitutions, None, None, _ => (Nil, Nil)).toOption
        elidedExtractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
        expansionStep = equality.expansion.assertionStepIfNecessary(source, result, wrapper)
      } yield SimplificationStepWithInference(wrapper(source), RearrangementStep(wrapper(result), (premiseSteps :+ elidedExtractionStep) ++ expansionStep.toSeq, inference.summary), inference.summary)
    }

    def findSimplifications(premiseTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Seq[SimplificationStepWithInference] = {
      def findSimplificationsDirectly = {
        val inferences = direction.getSource(termSimplificationInferences, termDesimplificationInferences)
        findSimplificationsFromInferences(premiseTerm, inferences, direction, wrapper)
      }
      def findSimplificationsWithinExpansion: Seq[SimplificationStepWithInference] = {
        premiseTerm match {
          case DefinedTerm(components, termDefinition) =>
            @scala.annotation.tailrec
            def helper(
              previousComponents: Seq[Expression],
              nextComponents: Seq[Expression],
              simplificationsSoFar: Seq[SimplificationStepWithInference]
            ): Seq[SimplificationStepWithInference] = {
              nextComponents match {
                case (innerTerm: Term) +: moar =>
                  val innerWrapper = wrapper.insert[Term]((t, _) => termDefinition((previousComponents :+ t) ++ moar: _*))
                  val newSimplifications = findSimplifications(innerTerm, direction, innerWrapper)
                  helper(previousComponents :+ innerTerm, moar, simplificationsSoFar ++ newSimplifications)
                case nonTerm +: moar =>
                  helper(previousComponents :+ nonTerm, moar, simplificationsSoFar)
                case Nil =>
                  simplificationsSoFar
              }
            }
            helper(Nil, components, Nil)
          case _ =>
            Nil
        }
      }
      findSimplificationsDirectly ++ findSimplificationsWithinExpansion
    }

    def findKnownEquality(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStepWithInference]] = {
      def findExactly = {
        if (premiseTerm == targetTerm)
          Some(Nil)
        else
          None
      }
      def findDirectly = {
        for {
          (steps, inferences) <- PremiseFinder.findPremiseStepsWithInferencesForStatement(equality(premiseTerm, targetTerm)).map(_.split)
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
          inference = inferences.singleMatch match {
            case PossibleSingleMatch.NoMatches =>
              Some(equality.expansion.inference).filter(_ => !wrapper.isIdentity)
            case PossibleSingleMatch.SingleMatch(inference) =>
              Some(inference.summary)
            case PossibleSingleMatch.MultipleMatches =>
              None
          }
        } yield Seq(RearrangementStepWithInference(RearrangementStep(wrapper(targetTerm), steps ++ wrappingStepOption.toSeq, EqualityRewriter.rewriteElider(inference)), inference))
      }
      findExactly orElse findDirectly
    }

    def findSimplificationEquality(premiseTerm: Term, targetTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStepWithInference]] = {
      val (source, result) = direction.swapSourceAndResult(premiseTerm, targetTerm)
      if (source.complexity > result.complexity) {
        findSimplifications(source, direction, wrapper).mapCollect { case SimplificationStepWithInference(baseTerm, rearrangementStep, inference) =>
          val newSource = direction.swapSourceAndResult(baseTerm, rearrangementStep.result)._2
          val (precedingSteps, followingSteps) = direction.swapSourceAndResult(Seq(RearrangementStepWithInference(rearrangementStep, Some(inference))), Nil)
          val (newPremise, newTarget) = direction.swapSourceAndResult(newSource, result)
          findEqualitySteps(newPremise, newTarget, wrapper).map { remainingSteps =>
            precedingSteps ++ remainingSteps ++ followingSteps
          }
        }.headOption
      }
      else
        None
    }

    def findComponentEquality(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStepWithInference]] = {
      def helper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        rewritesSoFar: Seq[RearrangementStepWithInference],
        innerWrapper: Wrapper[Seq[Term], Term]
      ): Option[Seq[RearrangementStepWithInference]] = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            for {
              nextSteps <- findEqualitySteps(premiseComponent, targetComponent, innerWrapper.insert((t, _) => (previousComponents.map(_._2) :+ t) ++ moreComponents.map(_._1)))
              result <- helper(
                previousComponents :+ (premiseComponent, targetComponent),
                moreComponents,
                rewritesSoFar ++ nextSteps,
                innerWrapper)
            } yield result
          case Nil =>
            Some(rewritesSoFar)
        }
      }

      (premiseTerm, targetTerm) match {
        case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition =>
          for {
            premiseTerms <- premiseComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            targetTerms <- targetComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            componentTerms <- premiseTerms.zipStrict(targetTerms)
            result <- helper(Nil, componentTerms, Nil, wrapper.insert((components, _) => premiseDefinition(components:_*)))
          } yield result
        case (TermVariable(f, premiseComponents), TermVariable(g, targetComponents)) if f == g =>
          for {
            componentTerms <- premiseComponents.zipStrict(targetComponents)
            result <- helper(Nil, componentTerms, Nil, wrapper.insert((arguments, _) => TermVariable(f, arguments)))
          } yield result
        case _ =>
          None
      }
    }

    def findEqualitySteps(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStepWithInference]] = {
      def direction = if (premiseTerm.complexity > targetTerm.complexity) Direction.Forward else Direction.Reverse
      findKnownEquality(premiseTerm, targetTerm, termWrapper) orElse
        findComponentEquality(premiseTerm, targetTerm, termWrapper) orElse
        findSimplificationEquality(premiseTerm, targetTerm, direction, termWrapper)
    }

    def findEquality(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term], statementWrapperOption: Option[Wrapper[Term, Statement]]): Option[StepWithInference] = {
      for {
        rearrangementStepsWithInferences <- findEqualitySteps(premiseTerm, targetTerm, termWrapper)
        rearrangementSteps = rearrangementStepsWithInferences.map(_.rearrangementStep)
        rearrangementInferences = rearrangementStepsWithInferences.map(_.inference)
        transitivitySteps = rearrangementSteps.single match {
          case Some(singleRearrangementStep) =>
            singleRearrangementStep.substeps
          case _ =>
            equality.transitivity.addToRearrangement(premiseTerm, rearrangementSteps)
        }
        substitutionStepOption = statementWrapperOption.map(wrapper => equality.substitution.assertionStep(premiseTerm, targetTerm, wrapper))
        inference = if (rearrangementInferences.nonEmpty) {
          val singleInference = rearrangementInferences.distinct.single.flatten
          singleInference match {
            case Some(equality.reversal.inference) =>
              Some(equality.substitution.inference)
            case _ =>
              singleInference
          }
        } else Some(equality.substitution.inference)
        result <- EqualityRewriter.rewriteElider(inference)(transitivitySteps ++ substitutionStepOption.toSeq)
      } yield StepWithInference(result, inference)
    }

    def rewritePremise(premise: Premise): Option[Step] = {
      def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrapper: Wrapper[Seq[Expression], Statement]): Option[Seq[StepWithInference]] = {
        def helper(
          previousComponents: Seq[(Expression, Expression)],
          nextComponents: Seq[(Expression, Expression)],
          currentSteps: Seq[StepWithInference]
        ): Option[Seq[StepWithInference]] = {
          nextComponents match {
            case Nil =>
              Some(currentSteps)
            case (premise: Statement, target: Statement) +: moar =>
              rewriteStatement(premise, target, wrapper.insert((s, _) => (previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
                .flatMap { newSteps =>
                  helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps)
                }
            case (premise: Term, target: Term) +: moar =>
              rewriteTerm(premise, target, wrapper.insert((t, _) => (previousComponents.map(_._2) :+ t) ++ moar.map(_._1)))
                .flatMap { newSteps =>
                  helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps)
                }
            case _ =>
              None
          }
        }
        premiseComponents.zipStrict(targetComponents).flatMap(helper(Nil, _, Nil))
      }

      def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrapper: Wrapper[Statement, Statement]): Option[Seq[StepWithInference]] = {
        if (premiseStatement == currentTarget)
          Some(Nil)
        else (premiseStatement, currentTarget) match {
          case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rewriteComponents(premiseComponents, targetComponents, wrapper.insert((components, _) => premiseDefinition(components:_*)))
          case (StatementVariable(premiseName, premiseArguments), StatementVariable(targetName, targetArguments)) if premiseName == targetName =>
            rewriteComponents(premiseArguments, targetArguments, wrapper.insert((arguments, _) => StatementVariable(premiseName, arguments.toType[Term].get)))
          case _ =>
            None
        }
      }

      def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement]): Option[Seq[StepWithInference]] = {
        if (premiseTerm == targetTerm)
          Some(Nil)
        else
          (premiseTerm, targetTerm) match {
            case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
              rewriteComponents(premiseComponents, targetComponents, wrapper.insert((components, _) => premiseDefinition(components:_*)))
            case (TermVariable(premiseName, premiseArguments), TermVariable(targetName, targetArguments)) if premiseName == targetName =>
              rewriteComponents(premiseArguments, targetArguments, wrapper.insert((arguments, _) => TermVariable(premiseName, arguments.toType[Term].get)))
            case _ =>
              findEquality(premiseTerm, targetTerm, Wrapper.identity, Some(wrapper)).map(Seq(_))
          }
      }

      rewriteStatement(premise.statement, targetStatement, Wrapper.identity)
        .flatMap { stepsAndInferences => EqualityRewriter.optionalRewriteElider(stepsAndInferences.map(_.inference))(stepsAndInferences.map(_.step)) }
    }

    (targetStatement match {
      case equality(premiseTerm, targetTerm) =>
        findEquality(premiseTerm, targetTerm, Wrapper.identity, None).map(_.step)
      case _ =>
        None
    }) orElse stepProvingContext.allPremises.mapFind(rewritePremise)
  }
}

object EqualityRewriter {
  def rewriteElider(inferences: Seq[Inference]): Seq[Step] => Option[Step] = rewriteElider(inferences.distinct.single)
  def optionalRewriteElider(inferences: Seq[Option[Inference]]): Seq[Step] => Option[Step] = rewriteElider(inferences.distinct.single.flatten)
  def rewriteElider(inference: Option[Inference]): Seq[Step] => Option[Step] = { steps =>
    Step.Elided.ifNecessary(steps, inference, "Rewritten")
  }

  def rewrite(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Step] = {
    for {
      equality <- stepProvingContext.provingContext.equalityOption
      rewriter = EqualityRewriter(equality)
      result <- rewriter.rewrite(targetStatement)
    } yield result
  }
}
