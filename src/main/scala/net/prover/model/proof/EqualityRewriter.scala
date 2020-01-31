package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{Equality, RearrangementStep, Wrapper}
import net.prover.model.expressions._

import scala.Ordering.Implicits._

case class EqualityRewriter(equality: Equality)(implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext.provingContext._
  def rewrite(targetStatement: Statement): Option[Step] = {
    sealed trait Direction {
      def getSourceAndResult[T](left: T, right: T): (T, T)
      def reverse: Direction
    }
    object Direction {
      val SourceToTarget: Direction = new Direction {
        def getSourceAndResult[T](left: T, right: T): (T, T) = (left, right)
        def reverse: Direction = TargetToSource
      }
      val TargetToSource: Direction = new Direction {
        def getSourceAndResult[T](left: T, right: T): (T, T) = (right, left)
        def reverse: Direction = SourceToTarget
      }
    }

    def findSimplificationsFromInferences(premiseTerm: Term, inferences: Seq[(Inference, Term, Term)], direction: Direction, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep, Seq[Inference.Summary])] = {
      for {
        (inference, left, right) <- inferences
        (inferenceSource, inferenceResult) = direction.getSourceAndResult(left, right)
        conclusionSubstitutions <- inferenceSource.calculateSubstitutions(premiseTerm).flatMap(_.confirmTotality)
        simplifiedTerm <- inferenceResult.applySubstitutions(conclusionSubstitutions).flatMap(_.asOptionalInstanceOf[Term])
        (premiseSteps, substitutedPremises, possibleFinalSubstitutions) <- PremiseFinder.findPremiseSteps(inference.premises, conclusionSubstitutions)
        finalSubstitutions <- possibleFinalSubstitutions.confirmTotality
        (source, result) = direction.getSourceAndResult(premiseTerm, simplifiedTerm)
        assertionStep = Step.Assertion(
          equality(source, result),
          inference.summary,
          substitutedPremises.map(Premise.Pending),
          finalSubstitutions)
        expansionStep = equality.expansion.assertionStepIfNecessary(source, result, wrapper)
      } yield (wrapper(source), RearrangementStep(wrapper(result), (premiseSteps :+ assertionStep) ++ expansionStep.toSeq, inference.summary), Seq(inference.summary))
    }

    def findSimplifications(premiseTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep, Seq[Inference.Summary])] = {
      def findSimplificationsDirectly = {
        val (forwardInferences, reverseInferences) = direction.getSourceAndResult(termSimplificationInferences, termDesimplificationInferences)
        findSimplificationsFromInferences(premiseTerm, forwardInferences, direction, wrapper) ++ reverse(findSimplificationsFromInferences(premiseTerm, reverseInferences, direction.reverse, wrapper))
      }
      def findSimplificationsWithinExpansion: Seq[(Term, RearrangementStep, Seq[Inference.Summary])] = {
        premiseTerm match {
          case DefinedTerm(components, termDefinition) =>
            @scala.annotation.tailrec
            def helper(
              previousComponents: Seq[Expression],
              nextComponents: Seq[Expression],
              simplificationsSoFar: Seq[(Term, RearrangementStep, Seq[Inference.Summary])]
            ): Seq[(Term, RearrangementStep, Seq[Inference.Summary])] = {
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

    def reverse(simplifications: Seq[(Term, RearrangementStep, Seq[Inference.Summary])]): Seq[(Term, RearrangementStep, Seq[Inference.Summary])] = {
      simplifications.map { case (source, RearrangementStep(result, steps, elider), inferenceForElision) =>
        val reversalStep = equality.reversal.assertionStep(result, source)
        (result, RearrangementStep(source, steps :+ reversalStep, elider), inferenceForElision)
      }
    }

    def findKnownEqualityRearrangement(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], Seq[Inference.Summary])] = {
      def findExactly = {
        if (premiseTerm == targetTerm)
          Some((Nil, Nil))
        else
          None
      }
      def findDirectly = {
        for {
          steps <- PremiseFinder.findPremiseSteps(equality(premiseTerm, targetTerm))
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
          assertionInferences = steps.map(_.inference)
          inferences = if (assertionInferences.nonEmpty) assertionInferences else if (!wrapper.isIdentity) Seq(equality.expansion.inference) else Nil
        } yield (Seq(RearrangementStep(wrapper(targetTerm), steps ++ wrappingStepOption.toSeq, inferences.single, "Rewritten")), assertionInferences)
      }
      def findReverse = {
        for {
          steps <- PremiseFinder.findPremiseSteps(equality(targetTerm, premiseTerm))
          reversalStep = equality.reversal.assertionStep(premiseTerm, targetTerm)
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
          assertionInferences = steps.map(_.inference)
          inferences = if (assertionInferences.nonEmpty) assertionInferences else if (!wrapper.isIdentity) Seq(equality.expansion.inference) else Seq(equality.reversal.inference)
        } yield (Seq(RearrangementStep(wrapper(targetTerm), (steps :+ reversalStep) ++ wrappingStepOption.toSeq, inferences.single, "Rewritten")), assertionInferences)
      }
      findExactly orElse findDirectly orElse findReverse
    }

    def findSimplificationRearrangement(premiseTerm: Term, targetTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], Seq[Inference.Summary])] = {
      val (source, result) = direction.getSourceAndResult(premiseTerm, targetTerm)
      if (source.complexity > result.complexity) {
        findSimplifications(source, direction, wrapper).mapCollect { case (baseTerm, rearrangementStep, inferences) =>
          val newSource = direction.getSourceAndResult(baseTerm, rearrangementStep.resultingTerm)._2
          val (precedingSteps, followingSteps) = direction.getSourceAndResult(Seq(rearrangementStep), Nil)
          val (newPremise, newTarget) = direction.getSourceAndResult(newSource, result)
          findRearrangementSteps(newPremise, newTarget, wrapper).map { case (remainingSteps, nextInferences) =>
            (precedingSteps ++ remainingSteps ++ followingSteps, inferences ++ nextInferences)
          }
        }.headOption
      }
      else
        None
    }

    def findComponentRearrangement(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], Seq[Inference.Summary])] = {
      def helper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        stepsSoFar: Seq[RearrangementStep],
        currentInferences: Seq[Inference.Summary],
        innerWrapper: Wrapper[Seq[Term], Term]
      ): Option[(Seq[RearrangementStep], Seq[Inference.Summary])] = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            for {
              (nextSteps, nextInferences) <- findRearrangementSteps(premiseComponent, targetComponent, innerWrapper.insert((t, _) => (previousComponents.map(_._2) :+ t) ++ moreComponents.map(_._1)))
              result <- helper(
                previousComponents :+ (premiseComponent, targetComponent),
                moreComponents,
                stepsSoFar ++ nextSteps,
                currentInferences ++ nextInferences,
                innerWrapper)
            } yield result
          case Nil =>
            Some((stepsSoFar, currentInferences))
        }
      }

      (premiseTerm, targetTerm) match {
        case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition =>
          for {
            premiseTerms <- premiseComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            targetTerms <- targetComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            componentTerms <- premiseTerms.zipStrict(targetTerms)
            result <- helper(Nil, componentTerms, Nil, Nil, wrapper.insert((components, _) => premiseDefinition(components:_*)))
          } yield result
        case (TermVariable(f, premiseComponents), TermVariable(g, targetComponents)) if f == g =>
          for {
            componentTerms <- premiseComponents.zipStrict(targetComponents)
            result <- helper(Nil, componentTerms, Nil, Nil, wrapper.insert((arguments, _) => TermVariable(f, arguments)))
          } yield result
        case _ =>
          None
      }
    }

    def findRearrangementSteps(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], Seq[Inference.Summary])] = {
      def direction = if (premiseTerm.complexity > targetTerm.complexity) Direction.SourceToTarget else Direction.TargetToSource
      findKnownEqualityRearrangement(premiseTerm, targetTerm, termWrapper) orElse
        findComponentRearrangement(premiseTerm, targetTerm, termWrapper) orElse
        findSimplificationRearrangement(premiseTerm, targetTerm, direction, termWrapper)
    }

    def findRearrangement(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term], statementWrapperOption: Option[Wrapper[Term, Statement]]): Option[(Step, Seq[Inference.Summary])] = {
      for {
        (rearrangementSteps, rearrangementInferences) <- findRearrangementSteps(premiseTerm, targetTerm, termWrapper)
        transitivitySteps = equality.addTransitivityToRearrangement(premiseTerm, rearrangementSteps)
        substitutionStepOption = statementWrapperOption.map(wrapper => equality.substitution.assertionStep(premiseTerm, targetTerm, wrapper))
        inferences = if (rearrangementInferences.nonEmpty) rearrangementInferences else Seq(equality.substitution.inference)
        result <- Step.Elided.ifNecessary(transitivitySteps ++ substitutionStepOption.toSeq, inferences.single, "Rewritten")
      } yield (result, rearrangementInferences)
    }

    def rewritePremise(premise: Premise): Option[Step] = {
      def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrapper: Wrapper[Seq[Expression], Statement]): Option[(Seq[Step], Seq[Inference.Summary])] = {
        def helper(
          previousComponents: Seq[(Expression, Expression)],
          nextComponents: Seq[(Expression, Expression)],
          currentSteps: Seq[Step],
          currentInferences: Seq[Inference.Summary]
        ): Option[(Seq[Step], Seq[Inference.Summary])] = {
          nextComponents match {
            case Nil =>
              Some((currentSteps, currentInferences))
            case (premise: Statement, target: Statement) +: moar =>
              rewriteStatement(premise, target, wrapper.insert((s, _) => (previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
                .flatMap { case (newSteps, newInferences) =>
                  helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps, currentInferences ++ newInferences)
                }
            case (premise: Term, target: Term) +: moar =>
              rewriteTerm(premise, target, wrapper.insert((t, _) => (previousComponents.map(_._2) :+ t) ++ moar.map(_._1)))
                .flatMap { case (newSteps, newInferenceForElision) =>
                  helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps, currentInferences ++ newInferenceForElision)
                }
            case _ =>
              None
          }
        }
        premiseComponents.zipStrict(targetComponents).flatMap(helper(Nil, _, Nil, Nil))
      }

      def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrapper: Wrapper[Statement, Statement]): Option[(Seq[Step], Seq[Inference.Summary])] = {
        if (premiseStatement == currentTarget)
          Some((Nil, Nil))
        else (premiseStatement, currentTarget) match {
          case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rewriteComponents(premiseComponents, targetComponents, wrapper.insert((components, _) => premiseDefinition(components:_*)))
          case (StatementVariable(premiseName, premiseArguments), StatementVariable(targetName, targetArguments)) if premiseName == targetName =>
            rewriteComponents(premiseArguments, targetArguments, wrapper.insert((arguments, _) => StatementVariable(premiseName, arguments.toType[Term].get)))
          case _ =>
            None
        }
      }

      def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement]): Option[(Seq[Step], Seq[Inference.Summary])] = {
        if (premiseTerm == targetTerm)
          Some((Nil, Nil))
        else
          (premiseTerm, targetTerm) match {
            case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
              rewriteComponents(premiseComponents, targetComponents, wrapper.insert((components, _) => premiseDefinition(components:_*)))
            case (TermVariable(premiseName, premiseArguments), TermVariable(targetName, targetArguments)) if premiseName == targetName =>
              rewriteComponents(premiseArguments, targetArguments, wrapper.insert((arguments, _) => TermVariable(premiseName, arguments.toType[Term].get)))
            case _ =>
              findRearrangement(premiseTerm, targetTerm, Wrapper.identity, Some(wrapper)).map(_.mapLeft(Seq(_)))
          }
      }

      rewriteStatement(premise.statement, targetStatement, Wrapper.identity)
        .flatMap { case (steps, inferences) => Step.Elided.ifNecessary(steps, inferences.single, "Rewritten") }
    }

    (targetStatement match {
      case equality(premiseTerm, targetTerm) =>
        findRearrangement(premiseTerm, targetTerm, Wrapper.identity, None).map(_._1)
      case _ =>
        None
    }) orElse stepProvingContext.allPremisesSimplestFirst.mapFind(rewritePremise)
  }
}

object EqualityRewriter {
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
