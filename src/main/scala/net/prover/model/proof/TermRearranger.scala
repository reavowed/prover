package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{RearrangeableOperator, _}
import net.prover.model.expressions._

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

  def applyTransformToOneSide[TMetadata](
    lhs: OperatorTree,
    rhs: OperatorTree,
    wrapper: Wrapper[Term, T],
    extendWrapper: TMetadata => Wrapper[Term, T],
    recurse: (OperatorTree, OperatorTree) => Option[(Seq[RearrangementStep[T]], TMetadata)]
  ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {

    def leftOrRight(replaceTree: (OperatorTree, (OperatorTree, OperatorTree, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) => Option[(OperatorTree, RearrangementOperation, Seq[OperatorTree], Seq[RearrangementStep[T]])]): Option[(Seq[RearrangementStep[T]], TMetadata)] = {
      def left = for {
        (replacedLhs, rearrangementOperation, termTrees, replacementSteps) <- replaceTree(lhs, matchTrees)
        (innerSteps, metadata) <- recurse(replacedLhs, rhs)
        rearrangementStep <- rearrangementOperation.rearrangementStep(termTrees.map(_.baseTerm), wrapper, expansion)
      } yield ((replacementSteps :+ rearrangementStep) ++ innerSteps, metadata)
      def right = for {
        (replacedRhs, rearrangementOperation, termTrees, replacementSteps) <- replaceTree(rhs, (a, b, w) => matchTrees(b, a, w))
        (innerSteps, metadata) <- recurse(lhs, replacedRhs)
        rearrangementStep <- rearrangementOperation.reversedRearrangementStep(termTrees.map(_.baseTerm), extendWrapper(metadata), expansion, reversal)
      } yield ((innerSteps :+ rearrangementStep) ++ replacementSteps, metadata)
      left orElse right
    }

    def byDistributing = {
      def getDistributivity(operatorTree: OperatorTree, matchTrees: (OperatorTree, OperatorTree, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftDistributivity = for {
          OperatorNode(outerOperator, left, OperatorNode(innerOperator, rightLeft, rightRight)) <- operatorTree.asOptionalInstanceOf[OperatorNode]
          leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
        } yield (innerOperator(outerOperator(left, rightLeft), outerOperator(left, rightRight)), leftDistributivity, Seq(left, rightLeft, rightRight), Nil)
        def rightDistributivity = for {
          OperatorNode(outerOperator, OperatorNode(innerOperator, leftLeft, leftRight), right) <- operatorTree.asOptionalInstanceOf[OperatorNode]
          leftDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
        } yield (innerOperator(outerOperator(leftLeft, right), outerOperator(leftRight, right)), leftDistributivity, Seq(leftLeft, leftRight, right), Nil)
        leftDistributivity orElse rightDistributivity
      }
      leftOrRight(getDistributivity)
    }

    def byIdentity = {
      def getIdentity(operatorTree: OperatorTree, matchTrees: (OperatorTree, OperatorTree, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftIdentity = for {
          OperatorNode(operator, left, right) <- operatorTree.asOptionalInstanceOf[OperatorNode]
          leftIdentity <- operator.leftIdentityOption
          matchingSteps <- matchTrees(left, disassemble(leftIdentity.identityTerm), addRight(wrapper, operator, right))
        } yield (right, leftIdentity, Seq(right), matchingSteps)
        def rightIdentity = for {
          OperatorNode(operator, left, right) <- operatorTree.asOptionalInstanceOf[OperatorNode]
          rightIdentity <- operator.rightIdentityOption
          matchingSteps <- matchTrees(right, disassemble(rightIdentity.identityTerm), addLeft(wrapper, operator, left))
        } yield (left, rightIdentity, Seq(left), matchingSteps)
        leftIdentity orElse rightIdentity
      }
      leftOrRight(getIdentity)
    }

    def byAbsorber = {
      def getAbsorber(operatorTree: OperatorTree, matchTrees: (OperatorTree, OperatorTree, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftAbsorber = for {
          OperatorNode(operator, left, right) <- operatorTree.asOptionalInstanceOf[OperatorNode]
          leftAbsorber <- operator.leftAbsorberOption
          matchingSteps <- matchTrees(left, disassemble(leftAbsorber.absorberTerm), addRight(wrapper, operator, right))
        } yield (disassemble(leftAbsorber.absorberTerm), leftAbsorber, Seq(right), matchingSteps)
        def rightAbsorber = for {
          OperatorNode(operator, left, right) <- operatorTree.asOptionalInstanceOf[OperatorNode]
          rightAbsorber <- operator.rightAbsorberOption
          matchingSteps <- matchTrees(right, disassemble(rightAbsorber.absorberTerm), addLeft(wrapper, operator, left))
        } yield (disassemble(rightAbsorber.absorberTerm), rightAbsorber, Seq(left), matchingSteps)
        leftAbsorber orElse rightAbsorber
      }
      leftOrRight(getAbsorber)
    }

    byDistributing orElse byIdentity orElse byAbsorber
  }

  // Attempts to rearrange baseTree such that it is an instance of an expectedResultOperator expression whose LHS is targetLeft
  private def pullLeft(baseTree: OperatorTree, targetLeft: OperatorTree, expectedResultOperator: RearrangeableOperator, wrapper: Wrapper[Term, T]): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    def matchingLeft = for {
      OperatorNode(`expectedResultOperator`, currentLeft, currentRight) <- baseTree.asOptionalInstanceOf[OperatorNode]
      steps <- matchTrees(currentLeft, targetLeft, addRight(wrapper, expectedResultOperator, currentRight))
    } yield (steps, currentRight)

    def matchingRight = for {
      OperatorNode(`expectedResultOperator`, currentLeft, currentRight) <- baseTree.asOptionalInstanceOf[OperatorNode]
      steps <- matchTrees(currentRight, targetLeft, addRight(wrapper, expectedResultOperator, currentLeft))
      commutativityStep <- expectedResultOperator.commutativity.rearrangementStep(currentLeft.baseTerm, currentRight.baseTerm, wrapper, expansion)
    } yield (commutativityStep +: steps, currentLeft)

    def insideLeft = for {
      OperatorNode(`expectedResultOperator`, currentLeft, currentRight) <- baseTree.asOptionalInstanceOf[OperatorNode]
      (steps, remainingRight) <- pullLeft(currentLeft, targetLeft, expectedResultOperator, addRight(wrapper, expectedResultOperator, currentRight))
      associativityStep <- expectedResultOperator.associativity.reversedRearrangementStep(targetLeft.baseTerm, remainingRight.baseTerm, currentRight.baseTerm, wrapper, expansion, reversal)
    } yield (steps :+ associativityStep, expectedResultOperator(remainingRight, currentRight))

    def insideRight = for {
      OperatorNode(`expectedResultOperator`, currentLeft, currentRight) <- baseTree.asOptionalInstanceOf[OperatorNode]
      (steps, remainingRight) <- pullLeft(currentRight, targetLeft, expectedResultOperator, addRight(wrapper, expectedResultOperator, currentLeft))
      commutativityStep <- expectedResultOperator.commutativity.rearrangementStep(currentLeft.baseTerm, currentRight.baseTerm, wrapper, expansion)
      associativityStep <- expectedResultOperator.associativity.reversedRearrangementStep(targetLeft.baseTerm, remainingRight.baseTerm, currentLeft.baseTerm, wrapper, expansion, reversal)
    } yield ((commutativityStep +: steps) :+ associativityStep, expectedResultOperator(remainingRight, currentLeft))

    def byRecursingInTarget = for {
      OperatorNode(`expectedResultOperator`, _, _) <- baseTree.asOptionalInstanceOf[OperatorNode]
      OperatorNode(`expectedResultOperator`, targetLeftLeft, targetLeftRight) <- targetLeft.asOptionalInstanceOf[OperatorNode]
      (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(baseTree, targetLeftLeft, expectedResultOperator, wrapper)
      (stepsForLeftRight, treeWithoutLeft) <- pullLeft(treeWithoutLeftLeft, targetLeftRight, expectedResultOperator, addLeft(wrapper, expectedResultOperator, targetLeftLeft))
      associativityStep <- expectedResultOperator.associativity.rearrangementStep(targetLeftLeft.baseTerm, targetLeftRight.baseTerm, treeWithoutLeft.baseTerm, wrapper, expansion)
    } yield (stepsForLeftLeft ++ stepsForLeftRight :+ associativityStep, treeWithoutLeft)

    def byApplyingTransform = applyTransformToOneSide[OperatorTree](
      baseTree,
      targetLeft,
      wrapper,
      addRight(wrapper, expectedResultOperator, _),
      pullLeft(_, _, expectedResultOperator, wrapper))

    matchingLeft orElse
      matchingRight orElse
      insideLeft orElse
      insideRight orElse
      byRecursingInTarget orElse
      byApplyingTransform
  }


  private def matchTrees(lhs: OperatorTree, rhs: OperatorTree, wrapper: Wrapper[Term, T]): Option[Seq[RearrangementStep[T]]] = {
    if (lhs.baseTerm == rhs.baseTerm)
      Some(Nil)
    else {
      def directly = for {
        OperatorNode(lhsOperator, _, _) <- lhs.asOptionalInstanceOf[OperatorNode]
        OperatorNode(rhsOperator, rhsLeft, rhsRight) <- rhs.asOptionalInstanceOf[OperatorNode]
        if lhsOperator == rhsOperator
        (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, lhsOperator, wrapper)
        stepsToMatchRight <- matchTrees(
          lhsRight,
          rhsRight,
          addLeft(wrapper, lhsOperator, rhsLeft))
      } yield stepsToPullLeft ++ stepsToMatchRight

      def byApplyingTransform = applyTransformToOneSide[Unit](
        lhs,
        rhs,
        wrapper,
        _ => wrapper,
        matchTrees(_, _, wrapper).map(_ -> ())
      ).map(_._1)

      directly orElse byApplyingTransform
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
