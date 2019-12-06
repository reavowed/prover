package net.prover.model.proof

import net.prover.model.definitions._
import net.prover.model.expressions._

case class TermRearranger(
    operator: BinaryOperator,
    commutativity: Commutativity,
    associativity: Associativity,
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext._

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

  private def disassemble(term: Term): OperatorTree = {
    (for {
      (a, b) <- operator.unapply(term)
    } yield Operator(disassemble(a), disassemble(b), term))
      .getOrElse(Leaf(term))
  }

  private def addRight(wrapper: Wrapper[Term, Term], rhs: OperatorTree): Wrapper[Term, Term] = {
    wrapper.insert(operator(_, rhs.baseTerm))
  }
  private def addLeft(wrapper: Wrapper[Term, Term], lhs: OperatorTree): Wrapper[Term, Term] = {
    wrapper.insert(operator(lhs.baseTerm, _))
  }

  private def pullLeft(tree: OperatorTree, targetLeft: OperatorTree, wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], OperatorTree)] = {
    tree match {
      case Operator(`targetLeft`, r, _) =>
        Some((Nil, r))
      case Operator(l, r, _) if l.isRearranged(targetLeft) =>
        for {
          leftSteps <- matchTrees(l, targetLeft, addRight(wrapper, r))
        } yield (leftSteps, r)
      case Operator(l, r, _) if l.contains(targetLeft) =>
        for {
          (steps, remainingRight) <- pullLeft(l, targetLeft, addRight(wrapper, r))
          associativityStep <- associativity.reverseRearrangementStep(targetLeft.baseTerm, remainingRight.baseTerm, r.baseTerm, wrapper)
        } yield (steps :+ associativityStep, Operator(remainingRight, r, operator(remainingRight.baseTerm, r.baseTerm)))
      case Operator(l, r, _) if r.contains(targetLeft) =>
        for {
          (steps, remainingRight) <- pullLeft(Operator(r, l, operator(r.baseTerm, l.baseTerm)), targetLeft, wrapper)
          commutativityStep <- commutativity.rearrangementStep(l.baseTerm, r.baseTerm, wrapper)
        } yield (commutativityStep +: steps, remainingRight)
      case _ =>
        targetLeft match {
          case Operator(targetLeftLeft, targetLeftRight, _) =>
            for {
              (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(tree, targetLeftLeft, wrapper)
              (stepsForLeftRight, treeWithoutLeft) <- pullLeft(
                treeWithoutLeftLeft,
                targetLeftRight,
                addLeft(wrapper, targetLeftLeft))
              associativityStep <- associativity.forwardRearrangementStep(targetLeftLeft.baseTerm, targetLeftRight.baseTerm, treeWithoutLeft.baseTerm, wrapper)
            } yield (stepsForLeftLeft ++ stepsForLeftRight :+ associativityStep, treeWithoutLeft)
          case _ =>
            None
        }
    }
  }

  private def matchTrees(lhs: OperatorTree, rhs: OperatorTree, wrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStep]] = {
    rhs match {
      case Operator(rhsLeft, rhsRight, _) =>
        for {
          (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, wrapper)
          stepsToMatchRight <- matchTrees(
            lhsRight,
            rhsRight,
            addLeft(wrapper, rhsLeft))
        } yield stepsToPullLeft ++ stepsToMatchRight
      case Leaf(t) if lhs.baseTerm == t =>
        Some(Nil)
      case _ =>
        None
    }
  }

  private def rearrangeLeaves(baseTree: OperatorTree, availableLeaves: Seq[Term], wrapper: Wrapper[Term, Term]): Option[(Seq[RearrangementStep], OperatorTree, Seq[Term])] = {
    baseTree match {
      case Leaf(t) =>
        if (availableLeaves.contains(t)) {
          Some((Nil, Leaf(t), availableLeaves.removeSingleValue(t)))
        } else {
          availableLeaves.mapFind { t2 =>
            TermRearranger.rearrangeTerm(t, t2, wrapper, equality).map((_, Leaf(t2), availableLeaves.removeSingleValue(t2)))
          }
        }
      case Operator(l, r, _) =>
        for {
          (leftSteps, leftTree, leavesAfterLeft) <- rearrangeLeaves(l, availableLeaves, wrapper.insert(t => operator(t, r.baseTerm)))
          (rightSteps, rightTree, leavesAfterRight) <- rearrangeLeaves(r, leavesAfterLeft, wrapper.insert(t => operator(leftTree.baseTerm, t)))
        } yield (leftSteps ++ rightSteps, Operator(leftTree, rightTree, operator(leftTree.baseTerm, rightTree.baseTerm)), leavesAfterRight)
    }
  }

  private def rearrangeTrees(baseLhs: OperatorTree, baseRhs: OperatorTree, wrapper: Wrapper[Term, Term]) = {
    for {
      (innerRearrangementSteps, rearrangedLeft, remainingTerms) <- rearrangeLeaves(baseLhs, baseRhs.allLeaves, wrapper)
      if remainingTerms.isEmpty
      mainRearrangementSteps <- matchTrees(rearrangedLeft, baseRhs, wrapper)
    } yield innerRearrangementSteps ++ mainRearrangementSteps
  }

  def rearrange(lhsTerm: Term, rhsTerm: Term, wrapper: Wrapper[Term, Term]): Option[Seq[RearrangementStep]] = {
    val baseLhs = disassemble(lhsTerm)
    val baseRhs = disassemble(rhsTerm)

    def rearrangeDirectly: Option[Seq[RearrangementStep]] = rearrangeTrees(baseLhs, baseRhs, wrapper)

    def rearrangeUsingPremise(premiseLhs: OperatorTree, premiseRhs: OperatorTree): Option[Seq[RearrangementStep]] = {
      (for {
        lhsMatch <- rearrangeTrees(baseLhs, premiseLhs, wrapper)
        rhsMatch <- rearrangeTrees(premiseRhs, baseRhs, wrapper)
        joiner = RearrangementStep(premiseRhs.baseTerm, Nil, _ => None)
      } yield (lhsMatch :+ joiner) ++ rhsMatch) orElse
        (for {
          firstMatch <- rearrangeTrees(baseLhs, premiseRhs, wrapper)
          secondMatch <- rearrangeTrees(premiseLhs, baseRhs, wrapper)
          joiner = equality.reversalRearrangementStep(premiseRhs.baseTerm, premiseLhs.baseTerm, wrapper)
        } yield (firstMatch :+ joiner) ++ secondMatch)
    }

    def rearrangeUsingPremises: Option[Seq[RearrangementStep]] = (for {
      premise <- allPremisesSimplestFirst
      (premiseLhsTerm, premiseRhsTerm) <- equality.unapply(premise.statement).toSeq
      premiseLhs = disassemble(premiseLhsTerm)
      premiseRhs = disassemble(premiseRhsTerm)
      result <- rearrangeUsingPremise(premiseLhs, premiseRhs)
    } yield result).headOption

    rearrangeDirectly orElse rearrangeUsingPremises
  }
}

object TermRearranger {
  private def rearrangeDirectly(
    lhs: Term,
    rhs: Term,
    equality: Equality,
    wrapper: Wrapper[Term, Term])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep]] = {
    for {
      (operator, commutativity, associativity) <- stepProvingContext.provingContext.rearrangeableFunctions
        .find { case (operator, _, _) => operator.unapply(lhs).nonEmpty}
      rearranger = TermRearranger(operator, commutativity, associativity, equality)
      result <- rearranger.rearrange(lhs, rhs, wrapper)
    } yield result
  }
  def rearrangeTerm(
    lhs: Term,
    rhs: Term,
    wrapper: Wrapper[Term, Term],
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep]] = {
    if (lhs == rhs)
      Some(Nil)
    else
      rearrangeDirectly(lhs, rhs, equality, wrapper) orElse
        ((lhs, rhs) match {
          case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rearrangeComponents(premiseComponents, targetComponents, wrapper.insert(components => premiseDefinition(components:_*)), equality)
          case (FunctionApplication(premiseName, premiseArguments), FunctionApplication(targetName, targetArguments)) if premiseName == targetName =>
            rearrangeComponents(premiseArguments, targetArguments, wrapper.insert(arguments => FunctionApplication(premiseName, arguments.toType[Term].get)), equality)
          case _ =>
            None
        })
  }
  def rearrangeStatement(
    lhsStatement: Statement,
    rhsStatement: Statement,
    wrapper: Wrapper[Statement, Term],
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep]] = {
    if (lhsStatement == rhsStatement)
      Some(Nil)
    else (lhsStatement, rhsStatement) match {
      case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
        rearrangeComponents(premiseComponents, targetComponents, wrapper.insert(components => premiseDefinition(components:_*)), equality)
      case (PredicateApplication(premiseName, premiseArguments), PredicateApplication(targetName, targetArguments)) if premiseName == targetName =>
        rearrangeComponents(premiseArguments, targetArguments, wrapper.insert(arguments => PredicateApplication(premiseName, arguments.toType[Term].get)), equality)
      case _ =>
        None
    }
  }
  def rearrangeComponents(
    lhsComponents: Seq[Expression],
    rhsComponents: Seq[Expression],
    wrapper: Wrapper[Seq[Expression], Term],
    equality: Equality)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep]] = {
    def helper(previousComponents: Seq[(Expression, Expression)], nextComponents: Seq[(Expression, Expression)], currentSteps: Seq[RearrangementStep]): Option[Seq[RearrangementStep]] = {
      nextComponents match {
        case Nil =>
          Some(currentSteps)
        case (premise: Statement, target: Statement) +: moar =>
          rearrangeStatement(premise, target, wrapper.insert(s => (previousComponents.map(_._2) :+ s) ++ moar.map(_._1)), equality)
            .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
        case (premise: Term, target: Term) +: moar =>
          rearrangeTerm(premise, target, wrapper.insert(t => (previousComponents.map(_._2) :+ t) ++ moar.map(_._1)), equality)
            .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
        case _ =>
          None
      }
    }
    lhsComponents.zipStrict(rhsComponents).flatMap(helper(Nil, _, Nil))
  }

  def rearrange(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[Step] = {
    for {
      equality <- stepProvingContext.provingContext.equalityOption
      (lhs, rhs) <- equality.unapply(targetStatement)
      rearrangementSteps <- rearrangeTerm(lhs, rhs, Wrapper.identity, equality)
      steps = equality.addTransitivityToRearrangement(lhs, rearrangementSteps)
      result <- Step.Elided.ifNecessary(steps, "Rearranged")
    } yield result
  }
}
