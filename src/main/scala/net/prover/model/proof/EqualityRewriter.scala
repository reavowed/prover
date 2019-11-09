package net.prover.model.proof

import net.prover.model.{Inference, Substitutions}
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, DefinedTerm, Expression, FunctionApplication, FunctionParameter, PredicateApplication, Statement, Term}

case class EqualityRewriter(
    equalityDefinition: StatementDefinition,
    equalitySubstitutionInference: Inference,
    equalityExpansionInference: Inference,
    equalityTransitivityInference: Inference,
    equalityReversalInference: Inference)
  extends ProofHelper.TransitiveEquality with ProofHelper.ReverseEquality with ProofHelper.ExpandEquality
{
  import ProofHelper.WrapElided._

  def rewrite(targetStatement: Statement)(implicit stepContext: StepContext): Option[Step] = {
    val entryContext = stepContext.entryContext
    val termSimplificationInferences = entryContext.availableEntries.ofType[Inference]
      .collect {
        case inference @ Inference(
        _,
        _,
        equalityDefinition(left: Term, right: Term))
          if left.complexity > right.complexity && left.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions) =>
          (inference, left, right)
      }
    val termDesimplificationInferences = entryContext.availableEntries.ofType[Inference]
      .collect {
        case inference @ Inference(
        _,
        _,
        equalityDefinition(left: Term, right: Term))
          if left.complexity < right.complexity && right.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions) =>
          (inference, left, right)
      }

    def findSimplificationsDirectly(premiseTerm: Term, reverse: Boolean): Seq[(Term, Step, Inference)] = {
      (for {
        (inference, left, right) <- termSimplificationInferences
        conclusionSubstitutions <- left.calculateSubstitutions(premiseTerm, stepContext)
        simplifiedTerm <- right.applySubstitutions(conclusionSubstitutions, stepContext).flatMap(_.asOptionalInstanceOf[Term])
        (premiseSteps, substitutedPremises, finalSubstitutions) <- PremiseFinder.findPremiseSteps(inference.premises, conclusionSubstitutions, stepContext)
        assertionStep = Step.Assertion(
          equalityDefinition(premiseTerm, simplifiedTerm),
          inference.summary,
          substitutedPremises.map(Premise.Pending),
          finalSubstitutions)
        steps =
        if (reverse)
          premiseSteps ++ Seq(assertionStep, equalityReversalStep(simplifiedTerm, premiseTerm))
        else
          premiseSteps :+ assertionStep
        step <- wrapAsElidedIfNecessary(steps, inference)
      } yield (simplifiedTerm, step, inference)) ++
        (for {
          (inference, left, right) <- termDesimplificationInferences
          conclusionSubstitutions <- right.calculateSubstitutions(premiseTerm, stepContext)
          simplifiedTerm <- left.applySubstitutions(conclusionSubstitutions, stepContext).flatMap(_.asOptionalInstanceOf[Term])
          (premiseSteps, substitutedPremises, finalSubstitutions) <- PremiseFinder.findPremiseSteps(inference.premises, conclusionSubstitutions, stepContext)
          assertionStep = Step.Assertion(
            equalityDefinition(simplifiedTerm, premiseTerm),
            inference.summary,
            substitutedPremises.map(Premise.Pending),
            finalSubstitutions)
          steps =
          if (!reverse)
            premiseSteps ++ Seq(assertionStep, equalityReversalStep(premiseTerm, simplifiedTerm))
          else
            premiseSteps :+ assertionStep
          step <- wrapAsElidedIfNecessary(steps, inference)
        } yield (simplifiedTerm, step, inference))
    }

    def findSimplificationsWithinExpansion(premiseTerm: Term, wrappingFunction: Term, reverse: Boolean): Seq[(Term, Step, Inference)] = {
      premiseTerm match {
        case DefinedTerm(components, termDefinition) =>
          @scala.annotation.tailrec
          def helper(previousComponents: Seq[Expression], nextComponents: Seq[Expression], resultsSoFar: Seq[(Term, Step, Inference)]): Seq[(Term, Step, Inference)]  = {
            nextComponents match {
              case (innerTerm: Term) +: moar =>
                val newWrapper = wrappingFunction.specify(
                  Seq(termDefinition((previousComponents :+ FunctionParameter(0, stepContext.externalDepth)) ++ moar: _*)),
                  0,
                  stepContext.externalDepth)
                val newResults = findSimplifications(innerTerm, reverse) map { case (simplifiedInnerTerm, simplificationStep, simplificationInference) =>
                  val simplifiedTerm = newWrapper.specify(Seq(simplifiedInnerTerm), 0, stepContext.externalDepth)
                  val expansionStep = if (reverse) equalityExpansionStep(simplifiedInnerTerm, innerTerm, newWrapper) else equalityExpansionStep(innerTerm, simplifiedInnerTerm, newWrapper)
                  val elidedStep = Step.Elided(Seq(simplificationStep, expansionStep), Some(simplificationInference.summary), None)
                  (simplifiedTerm, elidedStep, simplificationInference)
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

    def findSimplifications(premiseTerm: Term, reverse: Boolean): Seq[(Term, Step, Inference)] = {
      findSimplificationsDirectly(premiseTerm, reverse) ++
        findSimplificationsWithinExpansion(premiseTerm, FunctionParameter(0, stepContext.externalDepth), reverse)
    }

    def findKnownEqualityPath(premiseTerm: Term, targetTerm: Term, wrappingFunction: Term): Option[Seq[(Term, Option[Step])]] = {
      def getWrappingStep(l: Term, r: Term): Seq[Step] = {
        if (wrappingFunction.isInstanceOf[FunctionParameter])
          Nil
        else
          Seq(equalityExpansionStep(l, r, wrappingFunction))
      }
      def findExactly = if (premiseTerm == targetTerm) Some(Nil) else None
      def findDirectly = PremiseFinder.findPremiseSteps(
        equalityDefinition(premiseTerm, targetTerm),
        stepContext
      ).map(steps => Seq((
        wrappingFunction.specify(Seq(targetTerm), 0, stepContext.externalDepth),
        wrapAsElidedIfNecessary(steps ++ getWrappingStep(premiseTerm, targetTerm), "Rewritten"))))
      def findReverse = PremiseFinder.findPremiseSteps(
        equalityDefinition(targetTerm, premiseTerm),
        stepContext
      ).map(steps => Seq((
        wrappingFunction.specify(Seq(targetTerm), 0, stepContext.externalDepth),
        wrapAsElidedIfNecessary((steps :+ equalityReversalStep(premiseTerm, targetTerm)) ++ getWrappingStep(premiseTerm, targetTerm), "Rewritten"))))

      def findByComponentsHelper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        stepsSoFar: Seq[(Term, Option[Step])],
        getWrapperForComponents: Seq[Term] => Term
      ): Option[Seq[(Term, Option[Step])]]  = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            def newWrapper = getWrapperForComponents((previousComponents.map(_._2) :+ FunctionParameter(0, stepContext.externalDepth)) ++ moreComponents.map(_._1))
            for {
              theseSteps <- findKnownEqualityPath(premiseComponent, targetComponent, newWrapper)
              result <- findByComponentsHelper(previousComponents :+ (premiseComponent, targetComponent), moreComponents, stepsSoFar ++ theseSteps, getWrapperForComponents)
            } yield result
          case Nil =>
            Some(stepsSoFar)
        }
      }
      def findByComponents = (premiseTerm, targetTerm) match {
        case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition =>
          for {
            premiseTerms <- premiseComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            targetTerms <- targetComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            componentTerms <- premiseTerms.zipStrict(targetTerms)
            result <- findByComponentsHelper(Nil, componentTerms, Nil, components => wrappingFunction.specify(Seq(premiseDefinition(components:_*)), 0, stepContext.externalDepth))
          } yield result
        case (FunctionApplication(f, premiseComponents), FunctionApplication(g, targetComponents)) if f == g =>
          for {
            componentTerms <- premiseComponents.zipStrict(targetComponents)
            result <- findByComponentsHelper(Nil, componentTerms, Nil, arguments => wrappingFunction.specify(Seq(FunctionApplication(f, arguments)), 0, stepContext.externalDepth))
          } yield result
        case _ =>
          None
      }

      findExactly orElse findDirectly orElse findReverse orElse findByComponents
    }
    def findKnownEqualitySteps(premiseTerm: Term, targetTerm: Term, wrappingFunction: Term): Option[Seq[Step]] = {
      for {
        simplificationPath <- findKnownEqualityPath(premiseTerm, targetTerm, wrappingFunction)
        transitivitySteps <- buildTransitivity(premiseTerm, simplificationPath)
      } yield transitivitySteps
    }
    def findByKnownEquality(premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement): Option[Step] = {
      for {
        simplificationSteps <- findKnownEqualitySteps(premiseTerm, targetTerm, FunctionParameter(0, stepContext.externalDepth))
        result <- wrap(simplificationSteps, premiseTerm, targetTerm, wrappingPredicate, "Rewritten")
      } yield result
    }

    def findSimplificationPath(premiseTerm: Term, targetTerm: Term, reverse: Boolean): Option[Seq[(Term, Option[Step])]] = {
      def findDirectly = if (premiseTerm == targetTerm) Some(Nil) else None
      def findBySimplifying = {
        if (!reverse && premiseTerm.complexity > targetTerm.complexity)
          (for {
            (simplifiedPremise, simplificationStep, _) <- findSimplifications(premiseTerm, reverse = false)
            remainingSteps <- findSimplificationPath(simplifiedPremise, targetTerm, reverse = false)
          } yield (simplifiedPremise, Some(simplificationStep)) +: remainingSteps).headOption
        else if (reverse && premiseTerm.complexity < targetTerm.complexity)
          (for {
            (simplifiedTarget, simplificationStep, _) <- findSimplifications(targetTerm, reverse = true)
            remainingSteps <- findSimplificationPath(premiseTerm, simplifiedTarget, reverse = true)
          } yield remainingSteps :+ (targetTerm, Some(simplificationStep))).headOption
        else
          None
      }
      findDirectly orElse findBySimplifying
    }

    def buildTransitivity(left: Term, path: Seq[(Term, Option[Step])]): Option[Seq[Step]] = {
      for {
        (firstRight, firstStep) <- path.headOption
        remainingPath = path.tail
      } yield remainingPath.foldLeft((firstRight, firstStep.toSeq)) { case ((previousRight, stepsSoFar), (currentRight, currentStep)) =>
        (currentRight, stepsSoFar ++ currentStep.toSeq :+ equalityTransitivityStep(left, previousRight, currentRight))
      }._2
    }

    def findSimplificationSteps(
      premiseTerm: Term,
      targetTerm: Term
    ): Option[Seq[Step]] = {
      for {
        simplificationPath <- findSimplificationPath(premiseTerm, targetTerm, premiseTerm.complexity < targetTerm.complexity)
        transitivitySteps <- buildTransitivity(premiseTerm, simplificationPath)
      } yield transitivitySteps
    }

    def wrap(steps: Seq[Step], premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement, description: String): Option[Step] = {
      val wrappingStep = Step.Assertion(
        wrappingPredicate.specify(Seq(targetTerm), 0, stepContext.externalDepth),
        equalitySubstitutionInference.summary,
        Seq(
          Premise.Pending(equalityDefinition(premiseTerm, targetTerm)),
          Premise.Pending(wrappingPredicate.specify(Seq(premiseTerm), 0, stepContext.externalDepth))),
        Substitutions(
          terms = equalitySubstitutionInference.requiredSubstitutions.terms.zip(Seq(premiseTerm, targetTerm)).toMap,
          predicates = equalitySubstitutionInference.requiredSubstitutions.predicates.zip(Seq(wrappingPredicate)).toMap))
      wrapAsElidedIfNecessary(steps :+ wrappingStep, description)
    }

    def findBySimplifying(premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement): Option[Step] = {
      for {
        simplificationSteps <- findSimplificationSteps(premiseTerm, targetTerm)
        result <- wrap(simplificationSteps, premiseTerm, targetTerm, wrappingPredicate, if (premiseTerm.complexity > targetTerm.complexity) "Simplified" else "Expanded")
      } yield result
    }

    def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrappingFunction: Seq[Expression] => Statement): Option[Seq[Step]] = {
      def helper(previousComponents: Seq[(Expression, Expression)], nextComponents: Seq[(Expression, Expression)], currentSteps: Seq[Step]): Option[Seq[Step]] = {
        nextComponents match {
          case Nil =>
            Some(currentSteps)
          case (premise: Statement, target: Statement) +: moar =>
            rewriteStatement(premise, target, s => wrappingFunction((previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case (premise: Term, target: Term) +: moar =>
            rewriteTerm(premise, target, wrappingFunction((previousComponents.map(_._2) :+ FunctionParameter(0, stepContext.externalDepth)) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case _ =>
            None
        }
      }
      premiseComponents.zipStrict(targetComponents).flatMap(helper(Nil, _, Nil))
    }

    def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrappingFunction: Statement => Statement): Option[Seq[Step]] = {
      if (premiseStatement == currentTarget)
        Some(Nil)
      else (premiseStatement, currentTarget) match {
        case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
          rewriteComponents(premiseComponents, targetComponents, components => wrappingFunction(premiseDefinition(components:_*)))
        case (PredicateApplication(premiseName, premiseArguments), PredicateApplication(targetName, targetArguments)) if premiseName == targetName =>
          rewriteComponents(premiseArguments, targetArguments, arguments => wrappingFunction(PredicateApplication(premiseName, arguments.toType[Term].get)))
        case _ =>
          None
      }
    }

    def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement): Option[Seq[Step]] = {
      if (premiseTerm == targetTerm)
        Some(Nil)
      else
        (premiseTerm, targetTerm) match {
          case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rewriteComponents(premiseComponents, targetComponents, components => wrappingPredicate.specify(Seq(premiseDefinition(components:_*)), 0, stepContext.externalDepth))
          case (FunctionApplication(premiseName, premiseArguments), FunctionApplication(targetName, targetArguments)) if premiseName == targetName =>
            rewriteComponents(premiseArguments, targetArguments, arguments => wrappingPredicate.specify(Seq(FunctionApplication(premiseName, arguments.toType[Term].get)), 0, stepContext.externalDepth))
          case _ =>
            (findBySimplifying(premiseTerm, targetTerm, wrappingPredicate) orElse findByKnownEquality(premiseTerm, targetTerm, wrappingPredicate))
              .map(Seq(_))
        }
    }

    def rewritePremise(premise: Premise): Option[Seq[Step]] = {
      rewriteStatement(premise.statement, targetStatement, identity)
    }

    val resultStepsOption = (targetStatement match {
      case equalityDefinition(premiseTerm: Term, targetTerm: Term) =>
        findSimplificationSteps(premiseTerm, targetTerm) orElse findKnownEqualitySteps(premiseTerm, targetTerm, FunctionParameter(0, stepContext.externalDepth))
      case _ =>
        None
    }) orElse stepContext.allPremisesSimplestFirst.mapFind(rewritePremise)

    resultStepsOption.flatMap(wrapAsElidedIfNecessary(_, "Rewritten"))
  }
}

object EqualityRewriter {
  def rewrite(
    targetStatement: Statement,
    stepContext: StepContext
  ): Option[Step] = {
    val entryContext = stepContext.entryContext
    for {
      equalityDefinition <- entryContext.equalityDefinitionOption
      equalitySubstitutionInference <- entryContext.findSubstitutionInference(equalityDefinition)
      equalityExpansionInference <- entryContext.findExpansionInference(equalityDefinition)
      equalityTransitivityInference <- entryContext.findTransitivityInference(equalityDefinition)
      equalityReversalInference <- entryContext.findReversalInference(equalityDefinition)
      rewriter = EqualityRewriter(equalityDefinition, equalitySubstitutionInference, equalityExpansionInference, equalityTransitivityInference, equalityReversalInference)
      result <- rewriter.rewrite(targetStatement)(stepContext)
    } yield result
  }
}
