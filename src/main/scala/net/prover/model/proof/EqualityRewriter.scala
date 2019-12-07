package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{Equality, RearrangementStep, Wrapper}
import net.prover.model.expressions._

case class EqualityRewriter(equality: Equality)(implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext.provingContext._
  def rewrite(targetStatement: Statement): Option[Step] = {
    sealed trait InferenceForElision {
      def combine(other: InferenceForElision): InferenceForElision
      def elider(fallbackDescription: String): Seq[Step] => Option[Step]
      def orElse(inference: Inference): InferenceForElision
    }
    object InferenceForElision {
      object NotDetermined extends InferenceForElision {
        override def combine(other: InferenceForElision): InferenceForElision = other
        override def elider(fallbackDescription: String): Seq[Step] => Option[Step] = Step.Elided.ifNecessary(_, fallbackDescription)
        override def orElse(inference: Inference): InferenceForElision = SingleInference(inference.summary)
      }
      object MultipleInferences extends InferenceForElision {
        override def combine(other: InferenceForElision): InferenceForElision = this
        override def elider(fallbackDescription: String): Seq[Step] => Option[Step] = Step.Elided.ifNecessary(_, fallbackDescription)
        override def orElse(inference: Inference): InferenceForElision = this
      }
      case class SingleInference(inference: Inference.Summary) extends InferenceForElision {
        override def combine(other: InferenceForElision): InferenceForElision = other match {
          case NotDetermined =>
            this
          case _ =>
            MultipleInferences
        }
        override def elider(fallbackDescription: String): Seq[Step] => Option[Step] = Step.Elided.ifNecessary(_, inference)
        override def orElse(inference: Inference): InferenceForElision = this
      }
      def get(steps: Seq[Step.Assertion]): InferenceForElision = {
        steps.map(_.inference).distinct match {
          case Nil => InferenceForElision.NotDetermined
          case Seq(inference) => InferenceForElision.SingleInference(inference)
          case _ => InferenceForElision.MultipleInferences
        }
      }
    }

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

    def findSimplificationsFromInferences(premiseTerm: Term, inferences: Seq[(Inference, Term, Term)], direction: Direction, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep, InferenceForElision)] = {
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
      } yield (wrapper(source), RearrangementStep(wrapper(result), (premiseSteps :+ assertionStep) ++ expansionStep.toSeq, inference.summary), InferenceForElision.SingleInference(inference.summary))
    }

    def findSimplifications(premiseTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep, InferenceForElision)] = {
      def findSimplificationsDirectly = {
        val (forwardInferences, reverseInferences) = direction.getSourceAndResult(termSimplificationInferences, termDesimplificationInferences)
        findSimplificationsFromInferences(premiseTerm, forwardInferences, direction, wrapper) ++ reverse(findSimplificationsFromInferences(premiseTerm, reverseInferences, direction.reverse, wrapper))
      }
      def findSimplificationsWithinExpansion: Seq[(Term, RearrangementStep, InferenceForElision)] = {
        premiseTerm match {
          case DefinedTerm(components, termDefinition) =>
            @scala.annotation.tailrec
            def helper(
              previousComponents: Seq[Expression],
              nextComponents: Seq[Expression],
              simplificationsSoFar: Seq[(Term, RearrangementStep, InferenceForElision)]
            ): Seq[(Term, RearrangementStep, InferenceForElision)] = {
              nextComponents match {
                case (innerTerm: Term) +: moar =>
                  val innerWrapper = wrapper.insert((t: Term) => termDefinition((previousComponents :+ t) ++ moar: _*))
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

    def reverse(simplifications: Seq[(Term, RearrangementStep, InferenceForElision)]): Seq[(Term, RearrangementStep, InferenceForElision)] = {
      simplifications.map { case (source, RearrangementStep(result, steps, elider), inferenceForElision) =>
        val reversalStep = equality.reversal.assertionStep(result, source)
        (result, RearrangementStep(source, steps :+ reversalStep, elider), inferenceForElision)
      }
    }

    def findKnownEqualityRearrangement(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], InferenceForElision)] = {
      def findExactly = {
        if (premiseTerm == targetTerm)
          Some((Nil, InferenceForElision.NotDetermined))
        else
          None
      }
      def findDirectly = {
        for {
          steps <- PremiseFinder.findPremiseSteps(equality(premiseTerm, targetTerm))
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
          inferenceForElision = InferenceForElision.get(steps).orElse(equality.expansion.inference)
        } yield (Seq(RearrangementStep(wrapper(targetTerm), steps ++ wrappingStepOption.toSeq, inferenceForElision.elider("Rewritten"))), inferenceForElision)
      }
      def findReverse = {
        for {
          steps <- PremiseFinder.findPremiseSteps(equality(targetTerm, premiseTerm))
          reversalStep = equality.reversal.assertionStep(premiseTerm, targetTerm)
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
          inferenceForElision = InferenceForElision.get(steps).orElse(equality.reversal.inference)
        } yield (Seq(RearrangementStep(wrapper(targetTerm), (steps :+ reversalStep) ++ wrappingStepOption.toSeq, inferenceForElision.elider("Rewritten"))), inferenceForElision)
      }
      findExactly orElse findDirectly orElse findReverse
    }

    def findSimplificationRearrangement(premiseTerm: Term, targetTerm: Term, direction: Direction, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], InferenceForElision)] = {
      val (source, result) = direction.getSourceAndResult(premiseTerm, targetTerm)
      if (source.complexity > result.complexity) {
        findSimplifications(source, direction, wrapper).mapCollect { case (baseTerm, rearrangementStep, inferenceForElision) =>
          val newSource = direction.getSourceAndResult(baseTerm, rearrangementStep.resultingTerm)._2
          val (precedingSteps, followingSteps) = direction.getSourceAndResult(Seq(rearrangementStep), Nil)
          val (newPremise, newTarget) = direction.getSourceAndResult(newSource, result)
          findRearrangementSteps(newPremise, newTarget, wrapper).map { case (remainingSteps, nextInferenceForElision) =>
            (precedingSteps ++ remainingSteps ++ followingSteps, inferenceForElision.combine(nextInferenceForElision))
          }
        }.headOption
      }
      else
        None
    }

    def findComponentRearrangement(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], InferenceForElision)] = {
      def helper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        stepsSoFar: Seq[RearrangementStep],
        currentInferenceForElision: InferenceForElision,
        innerWrapper: Wrapper[Seq[Term], Term]
      ): Option[(Seq[RearrangementStep], InferenceForElision)] = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            for {
              (nextSteps, nextInferenceForElision) <- findRearrangementSteps(premiseComponent, targetComponent, innerWrapper.insert(t => (previousComponents.map(_._2) :+ t) ++ moreComponents.map(_._1)))
              result <- helper(
                previousComponents :+ (premiseComponent, targetComponent),
                moreComponents,
                stepsSoFar ++ nextSteps,
                currentInferenceForElision.combine(nextInferenceForElision),
                innerWrapper)
            } yield result
          case Nil =>
            Some((stepsSoFar, currentInferenceForElision))
        }
      }

      (premiseTerm, targetTerm) match {
        case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition =>
          for {
            premiseTerms <- premiseComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            targetTerms <- targetComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            componentTerms <- premiseTerms.zipStrict(targetTerms)
            result <- helper(Nil, componentTerms, Nil, InferenceForElision.NotDetermined, wrapper.insert(components => premiseDefinition(components:_*)))
          } yield result
        case (FunctionApplication(f, premiseComponents), FunctionApplication(g, targetComponents)) if f == g =>
          for {
            componentTerms <- premiseComponents.zipStrict(targetComponents)
            result <- helper(Nil, componentTerms, Nil, InferenceForElision.NotDetermined, wrapper.insert(arguments => FunctionApplication(f, arguments)))
          } yield result
        case _ =>
          None
      }
    }

    def findRearrangementSteps(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], InferenceForElision)] = {
      def direction = if (premiseTerm.complexity > targetTerm.complexity) Direction.SourceToTarget else Direction.TargetToSource
      findKnownEqualityRearrangement(premiseTerm, targetTerm, termWrapper) orElse
        findComponentRearrangement(premiseTerm, targetTerm, termWrapper) orElse
        findSimplificationRearrangement(premiseTerm, targetTerm, direction, termWrapper)
    }

    def findRearrangement(premiseTerm: Term, targetTerm: Term, termWrapper: Wrapper[Term, Term], statementWrapperOption: Option[Wrapper[Term, Statement]]): Option[(Step, InferenceForElision)] = {
      for {
        (rearrangementSteps, inferenceForElision) <- findRearrangementSteps(premiseTerm, targetTerm, termWrapper)
        transitivitySteps = equality.addTransitivityToRearrangement(premiseTerm, rearrangementSteps)
        substitutionStepOption = statementWrapperOption.map(wrapper => equality.substitution.assertionStep(premiseTerm, targetTerm, wrapper))
        result <- inferenceForElision.elider("Rewritten")(transitivitySteps ++ substitutionStepOption.toSeq)
      } yield (result, inferenceForElision)
    }

    def rewritePremise(premise: Premise): Option[Step] = {
      def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrapper: Wrapper[Seq[Expression], Statement]): Option[(Seq[Step], InferenceForElision)] = {
        def helper(
          previousComponents: Seq[(Expression, Expression)],
          nextComponents: Seq[(Expression, Expression)],
          currentSteps: Seq[Step],
          currentInferenceForElision: InferenceForElision
        ): Option[(Seq[Step], InferenceForElision)] = {
          nextComponents match {
            case Nil =>
              Some((currentSteps, currentInferenceForElision))
            case (premise: Statement, target: Statement) +: moar =>
              rewriteStatement(premise, target, wrapper.insert(s => (previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
                .flatMap { case (newSteps, newInferenceForElision) =>
                  helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps, currentInferenceForElision.combine(newInferenceForElision))
                }
            case (premise: Term, target: Term) +: moar =>
              rewriteTerm(premise, target, wrapper.insert(t => (previousComponents.map(_._2) :+ t) ++ moar.map(_._1)))
                .flatMap { case (newSteps, newInferenceForElision) =>
                  helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps, currentInferenceForElision.combine(newInferenceForElision))
                }
            case _ =>
              None
          }
        }
        premiseComponents.zipStrict(targetComponents).flatMap(helper(Nil, _, Nil, InferenceForElision.NotDetermined))
      }

      def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrapper: Wrapper[Statement, Statement]): Option[(Seq[Step], InferenceForElision)] = {
        if (premiseStatement == currentTarget)
          Some((Nil, InferenceForElision.NotDetermined))
        else (premiseStatement, currentTarget) match {
          case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rewriteComponents(premiseComponents, targetComponents, wrapper.insert(components => premiseDefinition(components:_*)))
          case (PredicateApplication(premiseName, premiseArguments), PredicateApplication(targetName, targetArguments)) if premiseName == targetName =>
            rewriteComponents(premiseArguments, targetArguments, wrapper.insert(arguments => PredicateApplication(premiseName, arguments.toType[Term].get)))
          case _ =>
            None
        }
      }

      def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement]): Option[(Seq[Step], InferenceForElision)] = {
        if (premiseTerm == targetTerm)
          Some((Nil, InferenceForElision.NotDetermined))
        else
          (premiseTerm, targetTerm) match {
            case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
              rewriteComponents(premiseComponents, targetComponents, wrapper.insert(components => premiseDefinition(components:_*)))
            case (FunctionApplication(premiseName, premiseArguments), FunctionApplication(targetName, targetArguments)) if premiseName == targetName =>
              rewriteComponents(premiseArguments, targetArguments, wrapper.insert(arguments => FunctionApplication(premiseName, arguments.toType[Term].get)))
            case _ =>
              findRearrangement(premiseTerm, targetTerm, Wrapper.identity, Some(wrapper)).map(_.mapLeft(Seq(_)))
          }
      }

      rewriteStatement(premise.statement, targetStatement, Wrapper.identity)
        .flatMap { case (steps, inferenceForElision) => inferenceForElision.elider("Rewritten")(steps) }
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
