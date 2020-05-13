package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{RearrangeableOperator, _}
import net.prover.model.expressions._
import scalaz.Functor

case class TermRearranger[T <: Expression](
    equality: Equality,
    expansion: Expansion[T],
    reversal: Reversal[T])(
    implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext._

  private sealed trait OperatorTree {
    def baseTerm: Term
    def leavesWithCurrentOperator: Seq[Term]
    def leavesWithOperator(operator: RearrangeableOperator): Seq[Term]
    def canRearrangeTo(other: OperatorTree): Boolean = other.leavesWithCurrentOperator.toSet == leavesWithCurrentOperator.toSet
    def contains(other: OperatorTree): Boolean = other.leavesWithCurrentOperator.toSet.subsetOf(leavesWithCurrentOperator.toSet)
    override def toString: String = baseTerm.toString
  }
  private case class Leaf(baseTerm: Term) extends OperatorTree {
    override def leavesWithCurrentOperator: Seq[Term] = Seq(baseTerm)
    override def leavesWithOperator(operator: RearrangeableOperator): Seq[Term] = Seq(baseTerm)
  }
  private case class OperatorNode(operator: RearrangeableOperator, left: OperatorTree, right: OperatorTree)(val baseTerm: Term) extends OperatorTree {
    override def leavesWithCurrentOperator: Seq[Term] = left.leavesWithOperator(operator) ++ right.leavesWithOperator(operator)
    override def leavesWithOperator(targetOperator: RearrangeableOperator): Seq[Term] = if (operator == targetOperator) leavesWithCurrentOperator else Seq(baseTerm)
  }
  private implicit class RearrangeableOperatorOps(operator: RearrangeableOperator) {
    def apply(left: OperatorTree, right: OperatorTree): OperatorNode = OperatorNode(operator, left, right)(operator(left.baseTerm, right.baseTerm))
  }

  private def disassemble(term: Term): OperatorTree = {
    provingContext.rearrangeableOperators.mapFind { operator =>
      for {
        (left, right) <- operator.unapply(term)
      } yield OperatorNode(operator, disassemble(left), disassemble(right))(term)
    }.getOrElse(Leaf(term))
  }

  private def addRight(wrapper: Wrapper[Term, T], operator: RearrangeableOperator, rhs: OperatorTree): Wrapper[Term, T] = {
    wrapper.insert(operator(_, rhs.baseTerm)(_))
  }
  private def addLeft(wrapper: Wrapper[Term, T], operator: RearrangeableOperator, lhs: OperatorTree): Wrapper[Term, T] = {
    wrapper.insert(operator(lhs.baseTerm, _)(_))
  }

  private def pullLeft(baseTree: OperatorTree, targetLeft: OperatorTree, wrapper: Wrapper[Term, T], operator: RearrangeableOperator): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    baseTree match {
      case OperatorNode(`operator`, currentLeft, currentRight) =>
        def matchingLeft = for {
          steps <- matchTrees(currentLeft, targetLeft, addRight(wrapper, operator, currentRight))
        } yield (steps, currentRight)

        def matchingRight = for {
          steps <- matchTrees(currentRight, targetLeft, addRight(wrapper, operator, currentLeft))
          commutativityStep <- operator.commutativity.rearrangementStep(currentLeft.baseTerm, currentRight.baseTerm, wrapper, expansion)
        } yield (commutativityStep +: steps, currentLeft)

        def insideLeft = for {
          (steps, remainingRight) <- pullLeft(currentLeft, targetLeft, addRight(wrapper, operator, currentRight), operator)
          associativityStep <- operator.associativity.reversedRearrangementStep(targetLeft.baseTerm, remainingRight.baseTerm, currentRight.baseTerm, wrapper, expansion, reversal)
        } yield (steps :+ associativityStep, operator(remainingRight, currentRight))

        def insideRight = for {
          (steps, remainingRight) <- pullLeft(currentRight, targetLeft, addRight(wrapper, operator, currentLeft), operator)
          commutativityStep <- operator.commutativity.rearrangementStep(currentLeft.baseTerm, currentRight.baseTerm, wrapper, expansion)
          associativityStep <- operator.associativity.reversedRearrangementStep(targetLeft.baseTerm, remainingRight.baseTerm, currentLeft.baseTerm, wrapper, expansion, reversal)
        } yield ((commutativityStep +: steps) :+ associativityStep, operator(remainingRight, currentLeft))

        def byRecursingInTarget = for {
          OperatorNode(`operator`, targetLeftLeft, targetLeftRight) <- targetLeft.asOptionalInstanceOf[OperatorNode]
          (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(baseTree, targetLeftLeft, wrapper, operator)
          (stepsForLeftRight, treeWithoutLeft) <- pullLeft(
            treeWithoutLeftLeft,
            targetLeftRight,
            addLeft(wrapper, operator, targetLeftLeft),
            operator)
          associativityStep <- operator.associativity.rearrangementStep(targetLeftLeft.baseTerm, targetLeftRight.baseTerm, treeWithoutLeft.baseTerm, wrapper, expansion)
        } yield (stepsForLeftLeft ++ stepsForLeftRight :+ associativityStep, treeWithoutLeft)

        def byLeftDistributingTarget = for {
          OperatorNode(otherOperator, targetLeftLeft, OperatorNode(`operator`, innerLeft, innerRight)) <- targetLeft.asOptionalInstanceOf[OperatorNode]
          leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == otherOperator && d.distributee == operator)
          (innerSteps, innerTree) <- pullLeft(baseTree, operator(otherOperator(targetLeftLeft, innerLeft), otherOperator(targetLeftLeft, innerRight)), wrapper, operator)
          distributivityStep <- leftDistributivity.reversedRearrangementStep(targetLeftLeft.baseTerm, innerLeft.baseTerm, innerRight.baseTerm, addRight(wrapper, operator, innerTree), expansion, reversal)
        } yield (innerSteps :+ distributivityStep, innerTree)

        def byRightDistributingTarget = for {
          OperatorNode(otherOperator, OperatorNode(`operator`, innerLeft, innerRight), targetLeftRight) <- targetLeft.asOptionalInstanceOf[OperatorNode]
          rightDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == otherOperator && d.distributee == operator)
          (innerSteps, innerTree) <- pullLeft(baseTree, operator(otherOperator(innerLeft, targetLeftRight), otherOperator(innerRight, targetLeftRight)), wrapper, operator)
          distributivityStep <- rightDistributivity.reversedRearrangementStep(innerLeft.baseTerm, innerRight.baseTerm, targetLeftRight.baseTerm, addRight(wrapper, operator, innerTree), expansion, reversal)
        } yield (innerSteps :+ distributivityStep, innerTree)

        matchingLeft orElse matchingRight orElse insideLeft orElse insideRight orElse byRecursingInTarget orElse byLeftDistributingTarget orElse byRightDistributingTarget
      case OperatorNode(otherOperator, currentLeft, OperatorNode(`operator`, innerLeft, innerRight)) =>
        for {
          leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == otherOperator && d.distributee == operator)
          (innerSteps, innerTree) <- pullLeft(operator(otherOperator(currentLeft, innerLeft), otherOperator(currentLeft, innerRight)), targetLeft, wrapper, operator)
          distributivityStep <- leftDistributivity.rearrangementStep(currentLeft.baseTerm, innerLeft.baseTerm, innerRight.baseTerm, wrapper, expansion)
        } yield (distributivityStep +: innerSteps, innerTree)
      case OperatorNode(otherOperator, OperatorNode(`operator`, innerLeft, innerRight), currentRight) =>
        for {
          rightDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == otherOperator && d.distributee == operator)
          (innerSteps, innerTree) <- pullLeft(operator(otherOperator(innerLeft, currentRight), otherOperator(innerRight, currentRight)), targetLeft, wrapper, operator)
          distributivityStep <- rightDistributivity.rearrangementStep(innerLeft.baseTerm, innerRight.baseTerm, currentRight.baseTerm, wrapper, expansion)
        } yield (distributivityStep +: innerSteps, innerTree)
      case _ =>
        None
    }
  }

  private def matchTrees(lhs: OperatorTree, rhs: OperatorTree, wrapper: Wrapper[Term, T]): Option[Seq[RearrangementStep[T]]] = {
    if (lhs.baseTerm == rhs.baseTerm)
      Some(Nil)
    else {
      def directly = for {
        OperatorNode(lhsOperator, _, _) <- lhs.asOptionalInstanceOf[OperatorNode]
        OperatorNode(rhsOperator, rhsLeft, rhsRight) <- rhs.asOptionalInstanceOf[OperatorNode]
        if lhsOperator == rhsOperator
        (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, wrapper, lhsOperator)
        stepsToMatchRight <- matchTrees(
          lhsRight,
          rhsRight,
          addLeft(wrapper, lhsOperator, rhsLeft))
      } yield stepsToPullLeft ++ stepsToMatchRight
      def byLeftDistributingLhs = for {
        OperatorNode(lhsOuterOperator, lhsLeft, OperatorNode(lhsInnerOperator, lhsRightLeft, lhsRightRight)) <- lhs.asOptionalInstanceOf[OperatorNode]
        OperatorNode(rhsOperator, _, _) <- rhs.asOptionalInstanceOf[OperatorNode]
        if lhsInnerOperator == rhsOperator
        leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == lhsOuterOperator && d.distributee == lhsInnerOperator)
        innerSteps <- matchTrees(lhsInnerOperator(lhsOuterOperator(lhsLeft, lhsRightLeft), lhsOuterOperator(lhsLeft, lhsRightRight)), rhs, wrapper)
        distributivityStep <- leftDistributivity.rearrangementStep(lhsLeft.baseTerm, lhsRightLeft.baseTerm, lhsRightRight.baseTerm, wrapper, expansion)
      } yield distributivityStep +: innerSteps
      def byRightDistributingLhs = for {
        OperatorNode(lhsOuterOperator, OperatorNode(lhsInnerOperator, lhsLeftLeft, lhsLeftRight), lhsRight) <- lhs.asOptionalInstanceOf[OperatorNode]
        OperatorNode(rhsOperator, _, _) <- rhs.asOptionalInstanceOf[OperatorNode]
        if lhsInnerOperator == rhsOperator
        rightDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == lhsOuterOperator && d.distributee == lhsInnerOperator)
        innerSteps <- matchTrees(lhsInnerOperator(lhsOuterOperator(lhsLeftLeft, lhsRight), lhsOuterOperator(lhsLeftRight, lhsRight)), rhs, wrapper)
        distributivityStep <- rightDistributivity.rearrangementStep(lhsLeftLeft.baseTerm, lhsLeftRight.baseTerm, lhsRight.baseTerm, wrapper, expansion)
      } yield distributivityStep +: innerSteps
      def byLeftDistributingRhs = for {
        OperatorNode(lhsOperator, _, _) <- lhs.asOptionalInstanceOf[OperatorNode]
        OperatorNode(rhsOuterOperator, rhsLeft, OperatorNode(rhsInnerOperator, rhsRightLeft, rhsRightRight)) <- rhs.asOptionalInstanceOf[OperatorNode]
        if lhsOperator == rhsInnerOperator
        leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == rhsOuterOperator && d.distributee == rhsInnerOperator)
        innerSteps <- matchTrees(lhs, rhsInnerOperator(rhsOuterOperator(rhsLeft, rhsRightLeft), rhsOuterOperator(rhsLeft, rhsRightRight)), wrapper)
        distributivityStep <- leftDistributivity.reversedRearrangementStep(rhsLeft.baseTerm, rhsRightLeft.baseTerm, rhsRightRight.baseTerm, wrapper, expansion, reversal)
      } yield innerSteps :+ distributivityStep
      def byRightDistributingRhs = for {
        OperatorNode(lhsOperator, _, _) <- lhs.asOptionalInstanceOf[OperatorNode]
        OperatorNode(rhsOuterOperator, OperatorNode(rhsInnerOperator, rhsLeftLeft, rhsLeftRight), rhsRight) <- rhs.asOptionalInstanceOf[OperatorNode]
        if lhsOperator == rhsInnerOperator
        rightDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == rhsOuterOperator && d.distributee == rhsInnerOperator)
        innerSteps <- matchTrees(lhs, rhsInnerOperator(rhsOuterOperator(rhsLeftLeft, rhsRight), rhsOuterOperator(rhsLeftRight, rhsRight)), wrapper)
        distributivityStep <- rightDistributivity.reversedRearrangementStep(rhsLeftLeft.baseTerm, rhsLeftRight.baseTerm, rhsRight.baseTerm, wrapper, expansion, reversal)
      } yield innerSteps :+ distributivityStep

      directly orElse byLeftDistributingLhs orElse byRightDistributingLhs orElse byLeftDistributingRhs orElse byRightDistributingLhs orElse byRightDistributingRhs
    }
  }

  def rearrange(lhsTerm: Term, rhsTerm: Term, wrapper: Wrapper[Term, T]): Option[Seq[RearrangementStep[T]]] = {
    val baseLhs = disassemble(lhsTerm)
    val baseRhs = disassemble(rhsTerm)

    def rearrangeDirectly: Option[Seq[RearrangementStep[T]]] = matchTrees(baseLhs, baseRhs, wrapper)

    def rearrangeUsingPremise(premiseLhs: OperatorTree, premiseRhs: OperatorTree): Option[Seq[RearrangementStep[T]]] = {
      (for {
        lhsMatch <- matchTrees(baseLhs, premiseLhs, wrapper)
        rhsMatch <- matchTrees(premiseRhs, baseRhs, wrapper)
        joiner = RearrangementStep(wrapper(premiseRhs.baseTerm), Nil, _ => None)
      } yield (lhsMatch :+ joiner) ++ rhsMatch) orElse
        (for {
          firstMatch <- matchTrees(baseLhs, premiseRhs, wrapper)
          secondMatch <- matchTrees(premiseLhs, baseRhs, wrapper)
          joiner = equality.reversalRearrangementStep(premiseRhs.baseTerm, premiseLhs.baseTerm, wrapper, expansion)
        } yield (firstMatch :+ joiner) ++ secondMatch)
    }

    def rearrangeUsingPremises: Option[Seq[RearrangementStep[T]]] = (for {
      premise <- allPremises
      (premiseLhsTerm, premiseRhsTerm) <- equality.unapply(premise.statement).toSeq
      premiseLhs = disassemble(premiseLhsTerm)
      premiseRhs = disassemble(premiseRhsTerm)
      result <- rearrangeUsingPremise(premiseLhs, premiseRhs)
    } yield result).headOption

    rearrangeDirectly orElse rearrangeUsingPremises
  }
}

object TermRearranger {
  def rearrangeTerm[T <: Expression](
    lhs: Term,
    rhs: Term,
    wrapper: Wrapper[Term, T],
    equality: Equality,
    expansion: Expansion[T],
    reversal: Reversal[T])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep[T]]] = {
    if (lhs == rhs)
      Some(Nil)
    else
      TermRearranger(equality, expansion, reversal).rearrange(lhs, rhs, wrapper) orElse
        ((lhs, rhs) match {
          case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rearrangeComponents(premiseComponents, targetComponents, wrapper.insert((components, _) => premiseDefinition(components:_*)), equality, expansion, reversal)
          case (TermVariable(premiseName, premiseArguments), TermVariable(targetName, targetArguments)) if premiseName == targetName =>
            rearrangeComponents(premiseArguments, targetArguments, wrapper.insert((arguments, _) => TermVariable(premiseName, arguments.toType[Term].get)), equality, expansion, reversal)
          case _ =>
            None
        })
  }
  def rearrangeStatement[T <: Expression](
    lhsStatement: Statement,
    rhsStatement: Statement,
    wrapper: Wrapper[Statement, T],
    equality: Equality,
    expansion: Expansion[T],
    reversal: Reversal[T])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep[T]]] = {
    if (lhsStatement == rhsStatement)
      Some(Nil)
    else (lhsStatement, rhsStatement) match {
      case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
        rearrangeComponents(premiseComponents, targetComponents, wrapper.insert((components, _) => premiseDefinition(components:_*)), equality, expansion, reversal)
      case (StatementVariable(premiseName, premiseArguments), StatementVariable(targetName, targetArguments)) if premiseName == targetName =>
        rearrangeComponents(premiseArguments, targetArguments, wrapper.insert((arguments, _) => StatementVariable(premiseName, arguments.toType[Term].get)), equality, expansion, reversal)
      case _ =>
        None
    }
  }
  def rearrangeComponents[T <: Expression](
    lhsComponents: Seq[Expression],
    rhsComponents: Seq[Expression],
    wrapper: Wrapper[Seq[Expression], T],
    equality: Equality,
    expansion: Expansion[T],
    reversal: Reversal[T])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[RearrangementStep[T]]] = {
    def helper(previousComponents: Seq[(Expression, Expression)], nextComponents: Seq[(Expression, Expression)], currentSteps: Seq[RearrangementStep[T]]): Option[Seq[RearrangementStep[T]]] = {
      nextComponents match {
        case Nil =>
          Some(currentSteps)
        case (premise: Statement, target: Statement) +: moar =>
          rearrangeStatement(premise, target, wrapper.insert((s, _) => (previousComponents.map(_._2) :+ s) ++ moar.map(_._1)), equality, expansion, reversal)
            .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
        case (premise: Term, target: Term) +: moar =>
          rearrangeTerm(premise, target, wrapper.insert((t, _) => (previousComponents.map(_._2) :+ t) ++ moar.map(_._1)), equality, expansion, reversal)
            .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
        case _ =>
          None
      }
    }
    lhsComponents.zipStrict(rhsComponents).flatMap(helper(Nil, _, Nil))
  }

  def rearrangeEquality(targetStatement: Statement, equality: Equality)(implicit stepProvingContext: StepProvingContext): Option[Step] = {
    for {
      (lhs, rhs) <- equality.unapply(targetStatement)
      rearrangementSteps <- rearrangeTerm(lhs, rhs, Wrapper.identity, equality, equality.expansion, equality.reversal)
      steps = equality.transitivity.addToRearrangement(lhs, rearrangementSteps)
      result <- Step.Elided.ifNecessary(steps, "Rearranged")
    } yield result
  }

  def rearrangeByExpanding(targetStatement: Statement, equality: Equality)(implicit stepProvingContext: StepProvingContext): Option[Step] = {

    def byJoiner[T <: Expression](joiners: Seq[BinaryJoiner[T]], rearrange: (T, T, Wrapper[T, T], Equality, Expansion[T], Reversal[T]) => Option[Seq[RearrangementStep[T]]]): Option[Step] = (for {
      joiner <- joiners.iterator
      (lhs, rhs) <- joiner.unapply(targetStatement)
      expansion <- stepProvingContext.provingContext.expansions.ofType[Expansion[T]].find(e => e.sourceJoiner == equality.relation && e.resultJoiner == joiner)
      reversal <- stepProvingContext.provingContext.reversals.ofType[Reversal[T]].find(_.joiner == joiner)
      transitivity <- stepProvingContext.provingContext.transitivities.ofType[Transitivity[T]].find(_.isTransitivityForJoiner(joiner))
      rearrangementSteps <- rearrange(lhs, rhs, Wrapper.identity, equality, expansion, reversal)
      steps = transitivity.addToRearrangement(lhs, rearrangementSteps)
      result <- Step.Elided.ifNecessary(steps, "Rearranged")
    } yield result).headOption

    byJoiner(stepProvingContext.provingContext.definedBinaryConnectives, rearrangeStatement[Statement]) orElse
      byJoiner(stepProvingContext.provingContext.definedBinaryRelations, rearrangeTerm[Term])
  }

  def rearrange(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[Step] = {
    for {
      equality <- stepProvingContext.provingContext.equalityOption
      result <- rearrangeEquality(targetStatement, equality) orElse rearrangeByExpanding(targetStatement, equality)
    } yield result
  }
}
