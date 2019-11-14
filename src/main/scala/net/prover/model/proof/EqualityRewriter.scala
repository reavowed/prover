package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.definitions.{Equality, RearrangementStep, Wrapper}
import net.prover.model.expressions._

case class EqualityRewriter(equality: Equality)
{
  def rewrite(targetStatement: Statement)(implicit stepContext: StepContext): Option[Step] = {
    val entryContext = stepContext.entryContext
    val termSimplificationInferences = entryContext.availableEntries.ofType[Inference]
      .collect {
        case inference @ Inference(
        _,
        _,
        equality(left: Term, right: Term))
          if left.complexity > right.complexity && left.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions) =>
          (inference, left, right)
      }
    val termDesimplificationInferences = entryContext.availableEntries.ofType[Inference]
      .collect {
        case inference @ Inference(
        _,
        _,
        equality(left: Term, right: Term))
          if left.complexity < right.complexity && right.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions) =>
          (inference, left, right)
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

    def findSimplifications(premiseTerm: Term, inferences: Seq[(Inference, Term, Term)], direction: Direction): Seq[(Term, RearrangementStep)] = {
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
      } yield (source, RearrangementStep(result, premiseSteps :+ assertionStep, inference.summary))
    }

    def findRecursiveSimplifications(premiseTerm: Term, findSimplificationsDirectly: Term => Seq[(Term, RearrangementStep)]): Seq[(Term, RearrangementStep)] = {
      def findSimplificationsWithinExpansion(premiseTerm: Term, wrapper: Wrapper[Term, Term]): Seq[(Term, RearrangementStep)] = {
        premiseTerm match {
          case DefinedTerm(components, termDefinition) =>
            @scala.annotation.tailrec
            def helper(previousComponents: Seq[Expression], nextComponents: Seq[Expression], resultsSoFar: Seq[(Term, RearrangementStep)]): Seq[(Term, RearrangementStep)] = {
              nextComponents match {
                case (innerTerm: Term) +: moar =>
                  val newWrapper = wrapper.insert((t: Term) => termDefinition((previousComponents :+ t) ++ moar: _*))
                  val newResults = findRecursiveSimplifications(innerTerm, findSimplificationsDirectly) map { case (innerSource, innerRearrangementStep) =>
                    val expansionStep = equality.expansion.assertionStep(innerSource, innerRearrangementStep.resultingTerm, newWrapper)
                    (newWrapper(innerSource), RearrangementStep(newWrapper(innerRearrangementStep.resultingTerm), innerRearrangementStep.substeps :+ expansionStep, innerRearrangementStep.elider))
                  }
                  helper(previousComponents :+ innerTerm, moar, resultsSoFar ++ newResults)
                case nonTerm +: moar =>
                  helper(previousComponents :+ nonTerm, moar, resultsSoFar)
                case Nil =>
                  resultsSoFar
              }
            }
            helper(Nil, components, Nil)
          case _ =>
            Nil
        }
      }
      findSimplificationsDirectly(premiseTerm) ++ findSimplificationsWithinExpansion(premiseTerm, Wrapper.identity)
    }

    def reverse(simplifications: Seq[(Term, RearrangementStep)]): Seq[(Term, RearrangementStep)] = {
      simplifications.map { case (source, RearrangementStep(result, steps, elider)) =>
        val reversalStep = equality.reversal.assertionStep(result, source)
        (result, RearrangementStep(source, steps :+ reversalStep, elider))
      }
    }

    def findKnownEqualityRearrangement(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStep]] = {

      def findExactly: Option[Seq[RearrangementStep]] = {
        if (premiseTerm == targetTerm)
          Some(Nil)
        else
          None
      }

      def findDirectly: Option[Seq[RearrangementStep]] = {
        for {
          steps <- PremiseFinder.findPremiseSteps(equality(premiseTerm, targetTerm))
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
        } yield Seq(RearrangementStep(wrapper(targetTerm), steps ++ wrappingStepOption.toSeq, "Rewritten"))
      }

      def findReverse: Option[Seq[RearrangementStep]] = {
        for {
          steps <- PremiseFinder.findPremiseSteps(equality(targetTerm, premiseTerm))
          reversalStep = equality.reversal.assertionStep(premiseTerm, targetTerm)
          wrappingStepOption = equality.expansion.assertionStepIfNecessary(premiseTerm, targetTerm, wrapper)
        } yield Seq(RearrangementStep(wrapper(targetTerm), (steps :+ reversalStep) ++ wrappingStepOption.toSeq, "Rewritten"))
      }

      def findByComponentsHelper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        stepsSoFar: Seq[RearrangementStep],
        innerWrapper: Wrapper[Seq[Term], Term]
      ): Option[Seq[RearrangementStep]]  = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            for {
              theseSteps <- findKnownEqualityRearrangement(premiseComponent, targetComponent, innerWrapper.insert(t => (previousComponents.map(_._2) :+ t) ++ moreComponents.map(_._1)))
              result <- findByComponentsHelper(previousComponents :+ (premiseComponent, targetComponent), moreComponents, stepsSoFar ++ theseSteps, innerWrapper)
            } yield result
          case Nil =>
            Some(stepsSoFar)
        }
      }

      def findByComponents: Option[Seq[RearrangementStep]] = (premiseTerm, targetTerm) match {
        case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition =>
          for {
            premiseTerms <- premiseComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            targetTerms <- targetComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            componentTerms <- premiseTerms.zipStrict(targetTerms)
            result <- findByComponentsHelper(Nil, componentTerms, Nil, wrapper.insert(components => premiseDefinition(components:_*)))
          } yield result
        case (FunctionApplication(f, premiseComponents), FunctionApplication(g, targetComponents)) if f == g =>
          for {
            componentTerms <- premiseComponents.zipStrict(targetComponents)
            result <- findByComponentsHelper(Nil, componentTerms, Nil, wrapper.insert(arguments => FunctionApplication(f, arguments)))
          } yield result
        case _ =>
          None
      }

      findExactly orElse findDirectly orElse findReverse orElse findByComponents
    }

    def findSimplificationRearrangement(premiseTerm: Term, targetTerm: Term, direction: Direction): Option[Seq[RearrangementStep]] = {
      def findDirectly: Option[Seq[RearrangementStep]] = if (premiseTerm == targetTerm) Some(Nil) else None
      def findBySimplifying: Option[Seq[RearrangementStep]] = {
        val (source, result) = direction.getSourceAndResult(premiseTerm, targetTerm)
        val (forwardInferences, reverseInferences) = direction.getSourceAndResult(termSimplificationInferences, termDesimplificationInferences)
        if (source.complexity > result.complexity) {
          val x = findRecursiveSimplifications(source, t => findSimplifications(t, forwardInferences, direction) ++ reverse(findSimplifications(t, reverseInferences, direction.reverse))).mapCollect { case (baseTerm, rearrangementStep) =>
            val newSource = direction.getSourceAndResult(baseTerm, rearrangementStep.resultingTerm)._2
            val (precedingSteps, followingSteps) = direction.getSourceAndResult(Seq(rearrangementStep), Nil)
            val (newPremise, newTarget) = direction.getSourceAndResult(newSource, result)
            findSimplificationRearrangement(newPremise, newTarget, direction).map { remainingSteps =>
              precedingSteps ++ remainingSteps ++ followingSteps
            }
          }.headOption
          x
        }
        else
          None
      }
      findDirectly orElse findBySimplifying
    }

    def findRearrangement(premiseTerm: Term, targetTerm: Term, wrapperOption: Option[Wrapper[Term, Statement]]): Option[Step] = {
      val direction = if (premiseTerm.complexity > targetTerm.complexity) Direction.SourceToTarget else Direction.TargetToSource
      for {
        rearrangementSteps <- findSimplificationRearrangement(premiseTerm, targetTerm, direction) orElse
          findKnownEqualityRearrangement(premiseTerm, targetTerm, Wrapper.identity)
        transitivitySteps = equality.addTransitivityToRearrangement(premiseTerm, rearrangementSteps)
        substitutionStepOption = wrapperOption.map(wrapper => equality.substitution.assertionStep(premiseTerm, targetTerm, wrapper))
        result <- Step.Elided.ifNecessary(transitivitySteps ++ substitutionStepOption.toSeq, "Rewritten")
      } yield result
    }

    def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrapper: Wrapper[Seq[Expression], Statement]): Option[Seq[Step]] = {
      def helper(previousComponents: Seq[(Expression, Expression)], nextComponents: Seq[(Expression, Expression)], currentSteps: Seq[Step]): Option[Seq[Step]] = {
        nextComponents match {
          case Nil =>
            Some(currentSteps)
          case (premise: Statement, target: Statement) +: moar =>
            rewriteStatement(premise, target, wrapper.insert(s => (previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case (premise: Term, target: Term) +: moar =>
            rewriteTerm(premise, target, wrapper.insert(t => (previousComponents.map(_._2) :+ t) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case _ =>
            None
        }
      }
      premiseComponents.zipStrict(targetComponents).flatMap(helper(Nil, _, Nil))
    }

    def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrapper: Wrapper[Statement, Statement]): Option[Seq[Step]] = {
      if (premiseStatement == currentTarget)
        Some(Nil)
      else (premiseStatement, currentTarget) match {
        case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
          rewriteComponents(premiseComponents, targetComponents, wrapper.insert(components => premiseDefinition(components:_*)))
        case (PredicateApplication(premiseName, premiseArguments), PredicateApplication(targetName, targetArguments)) if premiseName == targetName =>
          rewriteComponents(premiseArguments, targetArguments, wrapper.insert(arguments => PredicateApplication(premiseName, arguments.toType[Term].get)))
        case _ =>
          None
      }
    }

    def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement]): Option[Seq[Step]] = {
      if (premiseTerm == targetTerm)
        Some(Nil)
      else
        (premiseTerm, targetTerm) match {
          case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rewriteComponents(premiseComponents, targetComponents, wrapper.insert(components => premiseDefinition(components:_*)))
          case (FunctionApplication(premiseName, premiseArguments), FunctionApplication(targetName, targetArguments)) if premiseName == targetName =>
            rewriteComponents(premiseArguments, targetArguments, wrapper.insert(arguments => FunctionApplication(premiseName, arguments.toType[Term].get)))
          case _ =>
            findRearrangement(premiseTerm, targetTerm, Some(wrapper)).map(Seq(_))
        }
    }

    def rewritePremise(premise: Premise): Option[Step] = {
      rewriteStatement(premise.statement, targetStatement, Wrapper.identity)
        .flatMap(Step.Elided.ifNecessary(_, "Rewritten"))
    }

    (targetStatement match {
      case equality(premiseTerm, targetTerm) =>
        findRearrangement(premiseTerm, targetTerm, None)
      case _ =>
        None
    }) orElse stepContext.allPremisesSimplestFirst.mapFind(rewritePremise)
  }
}

object EqualityRewriter {
  def rewrite(
    targetStatement: Statement,
    stepContext: StepContext
  ): Option[Step] = {
    val entryContext = stepContext.entryContext
    for {
      equality <- entryContext.equalityOption
      rewriter = EqualityRewriter(equality)
      result <- rewriter.rewrite(targetStatement)(stepContext)
    } yield result
  }
}
