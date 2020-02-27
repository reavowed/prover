package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{Equality, RearrangementStep, Wrapper}
import net.prover.model.expressions._
import net.prover.util.{Direction, PossibleSingleMatch}

import scala.Ordering.Implicits._

case class EqualityRewriter(equality: Equality)(implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext.provingContext._
  def rewrite(targetStatement: Statement): Option[Step] = {
    def findSimplificationsFromInferences(premiseTerm: Term, inferences: Seq[(Inference, Term, Term)], direction: Direction, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])] = {
      for {
        (inference, left, right) <- inferences
        (inferenceSource, inferenceResult) = direction.swapSourceAndResult(left, right)
        conclusionSubstitutions <- inferenceSource.calculateSubstitutions(premiseTerm).flatMap(_.confirmTotality)
        simplifiedTerm <- inferenceResult.applySubstitutions(conclusionSubstitutions).flatMap(_.asOptionalInstanceOf[Term])
        (premiseSteps, substitutedPremises, possibleFinalSubstitutions) <- PremiseFinder.findPremiseStepsForStatementsBySubstituting(inference.premises, conclusionSubstitutions)
        finalSubstitutions <- possibleFinalSubstitutions.confirmTotality
        (source, result) = direction.swapSourceAndResult(premiseTerm, simplifiedTerm)
        assertionStep = Step.Assertion(
          equality(source, result),
          inference.summary,
          substitutedPremises.map(Premise.Pending),
          finalSubstitutions)
        expansionStep = equality.expansion.assertionStepIfNecessary(source, result, wrapper)
      } yield (wrapper(source), RearrangementStep(wrapper(result), (premiseSteps :+ assertionStep) ++ expansionStep.toSeq, inference.summary), Some(inference.summary))
    }

    def reverseSimplifications(simplifications: Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])]): Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])] = {
      simplifications.map { case (source, RearrangementStep(result, steps, elider), inferenceForElision) =>
        val reversalStep = equality.reversal.assertionStep(result, source)
        (result, RearrangementStep(source, steps :+ reversalStep, elider), inferenceForElision)
      }
    }

    def findSimplifications(premiseTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])] = {
      def findSimplificationsDirectly = {
        val (forwardInferences, reverseInferences) = direction.swapSourceAndResult(termSimplificationInferences, termDesimplificationInferences)
        findSimplificationsFromInferences(premiseTerm, forwardInferences, direction, wrapper) ++ reverseSimplifications(findSimplificationsFromInferences(premiseTerm, reverseInferences, direction.reverse, wrapper))
      }
      def findSimplificationsWithinExpansion: Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])] = {
        premiseTerm match {
          case DefinedTerm(components, termDefinition) =>
            @scala.annotation.tailrec
            def helper(
              previousComponents: Seq[Expression],
              nextComponents: Seq[Expression],
              simplificationsSoFar: Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])]
            ): Seq[(Term, RearrangementStep[Term], Option[Inference.Summary])] = {
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

    def findKnownEquality(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[Seq[(RearrangementStep[Term], Option[Inference.Summary])]] = {
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
        } yield Seq((RearrangementStep(wrapper(targetTerm), steps ++ wrappingStepOption.toSeq, EqualityRewriter.rewriteElider(inference)), inference))
      }
      def findReverse = {
        for {
          steps <- PremiseFinder.findPremiseStepsForStatement(equality(targetTerm, premiseTerm))
          reversalStep = equality.reversal.assertionStep(premiseTerm, targetTerm)
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
          inference = Some(equality.expansion.inference).filter(_ => !wrapper.isIdentity)
        } yield Seq((RearrangementStep(wrapper(targetTerm), (steps :+ reversalStep) ++ wrappingStepOption.toSeq, EqualityRewriter.rewriteElider(inference)), inference))
      }
      findExactly orElse findDirectly orElse findReverse
    }

    def findSimplificationEquality(premiseTerm: Term, targetTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Option[Seq[(RearrangementStep[Term], Option[Inference.Summary])]] = {
      val (source, result) = direction.swapSourceAndResult(premiseTerm, targetTerm)
      if (source.complexity > result.complexity) {
        findSimplifications(source, direction, wrapper).mapCollect { case (baseTerm, rearrangementStep, inference) =>
          val newSource = direction.swapSourceAndResult(baseTerm, rearrangementStep.result)._2
          val (precedingSteps, followingSteps) = direction.swapSourceAndResult(Seq((rearrangementStep, inference)), Nil)
          val (newPremise, newTarget) = direction.swapSourceAndResult(newSource, result)
          findEqualitySteps(newPremise, newTarget, wrapper).map { remainingSteps =>
            precedingSteps ++ remainingSteps ++ followingSteps
          }
        }.headOption
      }
      else
        None
    }

    def findComponentEquality(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[Seq[(RearrangementStep[Term], Option[Inference.Summary])]] = {
      def helper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        stepsSoFar: Seq[(RearrangementStep[Term], Option[Inference.Summary])],
        innerWrapper: Wrapper[Seq[Term], Term]
      ): Option[Seq[(RearrangementStep[Term], Option[Inference.Summary])]] = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            for {
              nextSteps <- findEqualitySteps(premiseComponent, targetComponent, innerWrapper.insert((t, _) => (previousComponents.map(_._2) :+ t) ++ moreComponents.map(_._1)))
              result <- helper(
                previousComponents :+ (premiseComponent, targetComponent),
                moreComponents,
                stepsSoFar ++ nextSteps,
                innerWrapper)
            } yield result
          case Nil =>
            Some(stepsSoFar)
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

    def findEqualitySteps(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term]): Option[Seq[(RearrangementStep[Term], Option[Inference.Summary])]] = {
      def direction = if (premiseTerm.complexity > targetTerm.complexity) Direction.Forward else Direction.Reverse
      findKnownEquality(premiseTerm, targetTerm, termWrapper) orElse
        findComponentEquality(premiseTerm, targetTerm, termWrapper) orElse
        findSimplificationEquality(premiseTerm, targetTerm, direction, termWrapper)
    }

    def findEquality(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term], statementWrapperOption: Option[Wrapper[Term, Statement]]): Option[(Step, Option[Inference.Summary])] = {
      for {
        rearrangementStepsAndInferences <- findEqualitySteps(premiseTerm, targetTerm, termWrapper)
        rearrangementSteps = rearrangementStepsAndInferences.map(_._1)
        rearrangementInferences = rearrangementStepsAndInferences.map(_._2)
        transitivitySteps = equality.transitivity.addToRearrangement(premiseTerm, rearrangementSteps)
        substitutionStepOption = statementWrapperOption.map(wrapper => equality.substitution.assertionStep(premiseTerm, targetTerm, wrapper))
        inference = if (rearrangementInferences.nonEmpty) rearrangementInferences.distinct.single.flatten else Some(equality.substitution.inference)
        result <- EqualityRewriter.rewriteElider(inference)(transitivitySteps ++ substitutionStepOption.toSeq)
      } yield (result, inference)
    }

    def rewritePremise(premise: Premise): Option[Step] = {
      def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrapper: Wrapper[Seq[Expression], Statement]): Option[Seq[(Step, Option[Inference.Summary])]] = {
        def helper(
          previousComponents: Seq[(Expression, Expression)],
          nextComponents: Seq[(Expression, Expression)],
          currentSteps: Seq[(Step, Option[Inference.Summary])]
        ): Option[Seq[(Step, Option[Inference.Summary])]] = {
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

      def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrapper: Wrapper[Statement, Statement]): Option[Seq[(Step, Option[Inference.Summary])]] = {
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

      def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement]): Option[Seq[(Step, Option[Inference.Summary])]] = {
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
        .flatMap { stepsAndInferences => EqualityRewriter.optionalRewriteElider(stepsAndInferences.map(_._2))(stepsAndInferences.map(_._1)) }
    }

    (targetStatement match {
      case equality(premiseTerm, targetTerm) =>
        findEquality(premiseTerm, targetTerm, Wrapper.identity, None).map(_._1)
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
