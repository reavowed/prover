package net.prover.model.proof

import net.prover.model.{Inference, Substitutions}
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, DefinedTerm, Expression, FunctionApplication, FunctionParameter, PredicateApplication, Statement, Term}
import net.prover.model.proof.ProofHelper.{ExpandEquality, TransitiveEquality}
import net.prover.model.proof.ProofHelper.WrapElided.wrapAsElidedIfNecessary

case class TermRearranger(
    operatorDefinition: Term,
    associativityInference: Inference,
    commutativityInference: Inference,
    equalityDefinition: StatementDefinition,
    equalityReversalInference: Inference,
    equalityTransitivityInference: Inference,
    equalityExpansionInference: Inference)(
    implicit stepContext: StepContext)
  extends ProofHelper.ReverseEquality with ProofHelper.TransitiveEquality with ProofHelper.ExpandEquality
{
  private sealed trait OperatorTree {
    def baseTerm: Term
    def allLeaves: Seq[Term]
    def isRearranged(other: OperatorTree): Boolean = other.allLeaves.toSet == allLeaves.toSet
    def contains(other: OperatorTree): Boolean = other.allLeaves.toSet.subsetOf(allLeaves.toSet)
  }
  private case class Leaf(baseTerm: Term) extends OperatorTree {
    override def allLeaves: Seq[Term] = Seq(baseTerm)
  }
  private case class Operator(l: OperatorTree, r: OperatorTree, baseTerm: Term) extends OperatorTree {
    override def allLeaves: Seq[Term] = l.allLeaves ++ r.allLeaves
  }

  private def operator(a: Term, b: Term): Term = {
    operatorDefinition.specify(Seq(a, b), 0, stepContext.externalDepth)
  }
  private def normalisedTriple(a: Term, b: Term, c: Term): Term = {
    operator(a, operator(b, c))
  }
  private def reversedTriple(a: Term, b: Term, c: Term): Term = {
    operator(operator(a, b), c)
  }
  private def wrap(wrappingFunction: Term, t: Term): Term = {
    wrappingFunction.specify(Seq(t), 0, stepContext.externalDepth)
  }

  def rearrange(lhsTerm: Term, rhsTerm: Term): Option[Seq[Step]] = {
    val baseLhs = disassemble(lhsTerm)
    val baseRhs = disassemble(rhsTerm)
    def normalisedAssociativity(a: Term, b: Term, c: Term, wrappingFunction: Term): Seq[Step] = {
      val normalised = normalisedTriple(a, b, c)
      val reversed = reversedTriple(a, b, c)
      val assertionSteps = ProofHelper.getAssertionWithPremises(
        associativityInference.summary,
        Substitutions(terms = associativityInference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap),
        stepContext)
      val steps = if (wrappingFunction.isInstanceOf[FunctionParameter]) {
        assertionSteps
      } else {
        Seq(Step.Elided(
          assertionSteps :+ equalityExpansionStep(normalised, reversed, wrappingFunction),
          Some(associativityInference.summary),
          None))
      }
      steps :+
        equalityTransitivityStep(
          baseLhs.baseTerm,
          wrap(wrappingFunction, normalised),
          wrap(wrappingFunction, reversed))
    }
    def reverseAssociativity(a: Term, b: Term, c: Term, wrappingFunction: Term): Seq[Step] = {
      val normalised = normalisedTriple(a, b, c)
      val reversed = reversedTriple(a, b, c)
      val associativityAndReversalSteps =
        ProofHelper.getAssertionWithPremises(
          associativityInference.summary,
          Substitutions(terms = associativityInference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap),
          stepContext) :+
          equalityReversalStep(reversed, normalised)
      val stepsToElide = if (wrappingFunction.isInstanceOf[FunctionParameter]) {
        associativityAndReversalSteps
      } else {
        associativityAndReversalSteps :+ equalityExpansionStep(reversed, normalised, wrappingFunction)
      }
      Seq(
        Step.Elided(
          stepsToElide,
          Some(associativityInference.summary),
          None),
        equalityTransitivityStep(
          baseLhs.baseTerm,
          wrap(wrappingFunction, reversed),
          wrap(wrappingFunction, normalised)))
    }
    def commutativity(a: Term, b: Term, wrappingFunction: Term): Seq[Step] = {
      val forward = operator(a, b)
      val reverse = operator(b, a)
      val assertionSteps = ProofHelper.getAssertionWithPremises(
        commutativityInference.summary,
        Substitutions(terms = commutativityInference.requiredSubstitutions.terms.zip(Seq(a, b)).toMap),
        stepContext)
      val steps = if (wrappingFunction.isInstanceOf[FunctionParameter]) {
        assertionSteps
      } else {
        Seq(
          Step.Elided(
            assertionSteps :+ equalityExpansionStep(forward, reverse, wrappingFunction),
            Some(commutativityInference.summary),
            None))
      }
      steps :+ equalityTransitivityStep(
        baseLhs.baseTerm,
        wrap(wrappingFunction, forward),
        wrap(wrappingFunction, reverse))
    }
    def addRight(wrappingFunction: Term, rhs: OperatorTree): Term = {
      wrap(wrappingFunction, operator(FunctionParameter(0, stepContext.externalDepth), rhs.baseTerm))
    }

    def pullLeft(tree: OperatorTree, targetLeft: OperatorTree, wrappingFunction: Term): Option[(Seq[Step], OperatorTree)] = {
      tree match {
        case Operator(`targetLeft`, r, _) =>
          Some((Nil, r))
        case Operator(l, r, _) if l.isRearranged(targetLeft) =>
          for {
            leftSteps <- matchTreesRaw(l, targetLeft, addRight(wrappingFunction, r))
          } yield (leftSteps, r)
        case Operator(l, r, _) if l.contains(targetLeft) =>
          for {
            (steps, remainingRight) <- pullLeft(
              l,
              targetLeft,
              wrappingFunction.specify(
                Seq(operator(FunctionParameter(0, stepContext.externalDepth), r.baseTerm)),
                0,
                stepContext.externalDepth))
            associativitySteps = reverseAssociativity(targetLeft.baseTerm, remainingRight.baseTerm, r.baseTerm, wrappingFunction)
          } yield (steps ++ associativitySteps, Operator(remainingRight, r, operator(remainingRight.baseTerm, r.baseTerm)))
        case Operator(l, r, _) if r.contains(targetLeft) =>
          for {
            (steps, remainingRight) <- pullLeft(Operator(r, l, operator(r.baseTerm, l.baseTerm)), targetLeft, wrappingFunction)
            commutativitySteps = commutativity(l.baseTerm, r.baseTerm, wrappingFunction)
          } yield (commutativitySteps ++ steps, remainingRight)
        case _ =>
          targetLeft match {
            case Operator(targetLeftLeft, targetLeftRight, _) =>
              for {
                (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(tree, targetLeftLeft, wrappingFunction)
                (stepsForLeftRight, treeWithoutLeft) <- pullLeft(
                  treeWithoutLeftLeft,
                  targetLeftRight,
                  wrap(wrappingFunction, operator(targetLeftLeft.baseTerm, FunctionParameter(0, stepContext.externalDepth))))
                associativitySteps = normalisedAssociativity(targetLeftLeft.baseTerm, targetLeftRight.baseTerm, treeWithoutLeft.baseTerm, wrappingFunction)
              } yield (stepsForLeftLeft ++ stepsForLeftRight ++ associativitySteps, treeWithoutLeft)
            case _ =>
              None
          }
      }
    }
    def matchTreesRaw(lhs: OperatorTree, rhs: OperatorTree, wrappingFunction: Term): Option[Seq[Step]] = {
      rhs match {
        case Operator(rhsLeft, rhsRight, _) =>
          for {
            (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, wrappingFunction)
            stepsToMatchRight <- matchTreesRaw(
              lhsRight,
              rhsRight,
              wrappingFunction.specify(
                Seq(operator(rhsLeft.baseTerm, FunctionParameter(0, stepContext.externalDepth))),
                0,
                stepContext.externalDepth))
          } yield stepsToPullLeft ++ stepsToMatchRight
        case Leaf(t) if lhs.baseTerm == t =>
          Some(Nil)
        case _ =>
          None
      }
    }
    def matchTrees(lhs: OperatorTree, rhs: OperatorTree): Option[Seq[Step]] = {
      matchTreesRaw(lhs, rhs, FunctionParameter(0, stepContext.externalDepth)).map(steps => steps.take(1) ++ steps.drop(2))
    }
    def rearrangeDirectly: Option[Seq[Step]] = matchTrees(baseLhs, baseRhs)

    def rearrangeUsingPremise(premiseLhs: OperatorTree, premiseRhs: OperatorTree): Option[Seq[Step]] = {
      (for {
        lhsMatch <- matchTrees(baseLhs, premiseLhs)
        joiner = if (lhsMatch.nonEmpty) Seq(equalityTransitivityStep(baseLhs.baseTerm, premiseLhs.baseTerm, premiseRhs.baseTerm)) else Nil
        rhsMatch <- if (lhsMatch.nonEmpty) matchTreesRaw(premiseRhs, baseRhs, FunctionParameter(0, stepContext.externalDepth)) else matchTrees(premiseRhs, baseRhs)
      } yield lhsMatch ++ joiner ++ rhsMatch) orElse
        (for {
          firstMatch <- matchTrees(baseLhs, premiseRhs)
          joiner = Seq(
            equalityReversalStep(premiseRhs.baseTerm, premiseLhs.baseTerm),
            equalityTransitivityStep(baseLhs.baseTerm, premiseRhs.baseTerm, premiseLhs.baseTerm))
          secondMatch <- matchTreesRaw(premiseLhs, baseRhs, FunctionParameter(0, stepContext.externalDepth))
        } yield firstMatch ++ joiner ++ secondMatch)
    }

    def rearrangeUsingPremises: Option[Seq[Step]] = (for {
      premise <- stepContext.allPremisesSimplestFirst
      (premiseLhsTerm, premiseRhsTerm) <- (premise.statement match {
        case equalityDefinition(l: Term, r: Term) => Some((l, r))
        case _ => None
      }).toSeq
      premiseLhs = disassemble(premiseLhsTerm)
      premiseRhs = disassemble(premiseRhsTerm)
      result <- rearrangeUsingPremise(premiseLhs, premiseRhs)
    } yield result).headOption

    rearrangeDirectly orElse rearrangeUsingPremises
  }

  private def disassemble(term: Term): OperatorTree = {
    operatorDefinition.calculateArguments(term, Map.empty, 0, stepContext.externalDepth).flatMap { map =>
      for {
        a <- map.get(0)
        b <- map.get(1)
      } yield Operator(disassemble(a), disassemble(b), term)
    }.getOrElse(Leaf(term))
  }
}

object TermRearranger {
  private def rearrangeDirectly(
    lhs: Term,
    rhs: Term,
    equalityDefinition: StatementDefinition,
    equalityReversalInference: Inference,
    equalityTransitivityInference: Inference,
    equalityExpansionInference: Inference,
    wrappingFunction: Term,
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    import stepContext.entryContext
    for {
      (operator, commutativityInference, associativityInference) <- entryContext.findRearrangableFunctions(equalityDefinition)
        .map { case (f, c, a) => (f.insertExternalParameters(stepContext.externalDepth), c, a)}
        .find { case (f, _, _) =>
          f.calculateArguments(lhs, Map.empty, 0, stepContext.externalDepth).nonEmpty
        }
      rearranger = TermRearranger(operator, associativityInference, commutativityInference, equalityDefinition, equalityReversalInference, equalityTransitivityInference, equalityExpansionInference)(stepContext)
      result <- rearranger.rearrange(lhs, rhs)
    } yield result
  }
  private def rearrange(
    baseLhs: Term,
    baseRhs: Term,
    equalityDefinition: StatementDefinition,
    equalityReversalInference: Inference,
    equalityTransitivityInference: Inference,
    equalityExpansionInference: Inference,
    stepContext: StepContext
  ): Option[Step] = {
    val transitiveEquality = TransitiveEquality(equalityDefinition, equalityTransitivityInference)
    val expandEquality = ExpandEquality(equalityDefinition, equalityExpansionInference)
    def rearrangeTerm(lhs: Term, rhs: Term, wrappingFunction: Term): Option[Seq[Step]] = {
      def wrapSteps(steps: Seq[Step]): Option[Seq[Step]] = {
        for {
          rearrangementStep <- wrapAsElidedIfNecessary(steps, "Rearranged")
          wrappedStep <- if (wrappingFunction.isInstanceOf[FunctionParameter])
              Some(rearrangementStep)
            else
              wrapAsElidedIfNecessary(Seq(rearrangementStep, expandEquality.equalityExpansionStep(lhs, rhs, wrappingFunction)(stepContext)), equalityExpansionInference)
          transitivityStep = transitiveEquality.equalityTransitivityStep(baseLhs, wrappingFunction.specify(Seq(lhs), 0, stepContext.externalDepth), wrappingFunction.specify(Seq(rhs), 0, stepContext.externalDepth))
        } yield Seq(wrappedStep, transitivityStep)
      }

      if (lhs == rhs)
        Some(Nil)
      else
        rearrangeDirectly(lhs, rhs, equalityDefinition, equalityReversalInference, equalityTransitivityInference, equalityExpansionInference, wrappingFunction, stepContext).flatMap(wrapSteps) orElse
          ((lhs, rhs) match {
            case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
              rearrangeComponents(premiseComponents, targetComponents, components => wrappingFunction.specify(Seq(premiseDefinition(components:_*)), 0, stepContext.externalDepth))
            case (FunctionApplication(premiseName, premiseArguments), FunctionApplication(targetName, targetArguments)) if premiseName == targetName =>
              rearrangeComponents(premiseArguments, targetArguments, arguments => wrappingFunction.specify(Seq(FunctionApplication(premiseName, arguments.toType[Term].get)), 0, stepContext.externalDepth))
            case _ =>
              None
          })
    }
    def rearrangeStatement(lhsStatement: Statement, rhsStatement: Statement, wrappingFunction: Statement => Term): Option[Seq[Step]] = {
      if (lhsStatement == rhsStatement)
        Some(Nil)
      else (lhsStatement, rhsStatement) match {
        case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
          rearrangeComponents(premiseComponents, targetComponents, components => wrappingFunction(premiseDefinition(components:_*)))
        case (PredicateApplication(premiseName, premiseArguments), PredicateApplication(targetName, targetArguments)) if premiseName == targetName =>
          rearrangeComponents(premiseArguments, targetArguments, arguments => wrappingFunction(PredicateApplication(premiseName, arguments.toType[Term].get)))
        case _ =>
          None
      }
    }
    def rearrangeComponents(lhsComponents: Seq[Expression], rhsComponents: Seq[Expression], wrappingFunction: Seq[Expression] => Term): Option[Seq[Step]] = {
      def helper(previousComponents: Seq[(Expression, Expression)], nextComponents: Seq[(Expression, Expression)], currentSteps: Seq[Step]): Option[Seq[Step]] = {
        nextComponents match {
          case Nil =>
            Some(currentSteps)
          case (premise: Statement, target: Statement) +: moar =>
            rearrangeStatement(premise, target, s => wrappingFunction((previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case (premise: Term, target: Term) +: moar =>
            rearrangeTerm(premise, target, wrappingFunction((previousComponents.map(_._2) :+ FunctionParameter(0, stepContext.externalDepth)) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case _ =>
            None
        }
      }
      lhsComponents.zipStrict(rhsComponents).flatMap(helper(Nil, _, Nil))
    }

    for {
      rawSteps <- rearrangeTerm(baseLhs, baseRhs, FunctionParameter(0, stepContext.externalDepth))
      steps = rawSteps.take(1) ++ rawSteps.drop(2)
      result <- wrapAsElidedIfNecessary(steps, "Rearranged")
    } yield (result)
  }

  def rearrange(targetStatement: Statement, stepContext: StepContext): Option[Step] = {
    import stepContext.entryContext
    for {
      (lhs, rhs, equalityDefinition) <- entryContext.matchEqualityStatement(targetStatement)
      equalityReversalInference <- entryContext.findReversalInference(equalityDefinition)
      equalityTransitivityInference <- entryContext.findTransitivityInference(equalityDefinition)
      equalityExpansionInference <- entryContext.findExpansionInference(equalityDefinition)
      result <- rearrange(lhs, rhs, equalityDefinition, equalityReversalInference, equalityTransitivityInference, equalityExpansionInference, stepContext)
    } yield result
  }
}
