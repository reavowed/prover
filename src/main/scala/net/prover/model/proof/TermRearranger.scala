package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions._
import net.prover.util.Direction

case class TermRearranger[T <: Expression](
    equality: Equality,
    expansion: Expansion[T],
    reversal: Reversal[T],
    mainWrapper: Wrapper[Term, T])(
    implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext._

  implicit class WrapperOps(wrapper: Wrapper[Term, T]) {
    def addLeft(operator: RearrangeableOperator, lhs: Term): Wrapper[Term, T] = {
      wrapper.insert(operator(lhs, _)(_))
    }
    def addRight(operator: RearrangeableOperator, rhs: Term): Wrapper[Term, T] = {
      wrapper.insert(operator(_, rhs)(_))
    }
    def addUnary(operator: UnaryOperator): Wrapper[Term, T] = {
      wrapper.insert(operator(_)(_))
    }
  }

  private implicit class DirectionOps(direction: Direction) {
    def rearrangementStep(rearrangementOperation: RearrangementOperation, terms: Seq[Term], wrapper: => Wrapper[Term, T], extendedWrapper: => Wrapper[Term, T]): Option[RearrangementStep[T]] = {
      direction.getSource(
        rearrangementOperation.rearrangementStep(terms, wrapper, expansion),
        rearrangementOperation.reversedRearrangementStep(terms, extendedWrapper, expansion, reversal))
    }
    def append[T](seq: Seq[T], t: T) = direction.getSource(seq :+ t, t +: seq)
    def concat[T](seq: Seq[T], seq2: Seq[T]) = direction.getSource(seq ++ seq2, seq2 ++ seq)
  }

//  def replaceTreeOnOneSide[TMetadata](
//    lhs: Term,
//    rhs: Term,
//    wrapper: Wrapper[Term, T],
//    extendWrapper: TMetadata => Wrapper[Term, T],
//    recurse: (Term, Term) => Option[(Seq[RearrangementStep[T]], TMetadata)],
//    replaceTree: (Term, (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) => Option[(Term, RearrangementOperation, Seq[Term], Seq[RearrangementStep[T]])]
//  ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {
//    def withDirection(direction: Direction): Option[(Seq[RearrangementStep[T]], TMetadata)] = for {
//      (replaced, rearrangementOperation, terms, replacementSteps) <- replaceTree(direction.getSource(lhs, rhs), matchTrees)
//      (innerSteps, metadata) <- recurse.tupled(direction.swapSourceAndResult(replaced, direction.getResult(lhs, rhs)))
//      rearrangementStep <- direction.rearrangementStep(rearrangementOperation, terms, wrapper, extendWrapper(metadata))
//    } yield (direction.concat(direction.append(replacementSteps, rearrangementStep), innerSteps), metadata)
//    withDirection(Direction.Forward) orElse withDirection(Direction.Reverse)
//  }
//
//  private def extractUnaryOperator(term: Term, unaryOperator: UnaryOperator, wrapper: Wrapper[Term, T], direction: Direction): Iterator[(Term, Seq[RearrangementStep[T]])] = {
//    def direct = for {
//      inner <- unaryOperator.unapply(term)
//    } yield (inner, Nil)
//    def leftRecurse = for {
//      (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//      extraction <- provingContext.leftOperatorExtractions.find(e => e.unaryOperator == unaryOperator && e.binaryOperator == operator.operator).iterator
//      (innerResult, innerRearrangement) <- extractUnaryOperator(left, unaryOperator, addRight(wrapper, operator, right), direction)
//      extractionStep <- direction.rearrangementStep(extraction, Seq(innerResult, right), wrapper, wrapper)
//    } yield (operator(innerResult, right), direction.append(innerRearrangement, extractionStep))
//    def rightRecurse = for {
//      (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//      extraction <- provingContext.rightOperatorExtractions.find(e => e.unaryOperator == unaryOperator && e.binaryOperator == operator.operator).iterator
//      (innerResult, innerRearrangement) <- extractUnaryOperator(right, unaryOperator, addLeft(wrapper, operator, left), direction)
//      extractionStep <- direction.rearrangementStep(extraction, Seq(left, innerResult), wrapper, wrapper)
//    } yield (operator(left, innerResult), direction.append(innerRearrangement, extractionStep))
//    direct.iterator ++ leftRecurse ++ rightRecurse
//  }
//
//  def applyTransformToOneSide[TMetadata](
//    lhs: Term,
//    rhs: Term,
//    wrapper: Wrapper[Term, T],
//    extendWrapper: TMetadata => Wrapper[Term, T],
//    recurse: (Term, Term) => Option[(Seq[RearrangementStep[T]], TMetadata)]
//  ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {
//
//    def applyTransform(
//      replaceTree: (Term, (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) => Option[(Term, RearrangementOperation, Seq[Term], Seq[RearrangementStep[T]])]
//    ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {
//      replaceTreeOnOneSide(lhs, rhs, wrapper, extendWrapper, recurse, replaceTree)
//    }
//
//    def byDistributing = {
//      def getDistributivity(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
//        def leftDistributivity = for {
//          (outerOperator, left, RearrangeableOperator(innerOperator, rightLeft, rightRight)) <- RearrangeableOperator.unapply(term)
//          leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
//        } yield (innerOperator(outerOperator(left, rightLeft), outerOperator(left, rightRight)), leftDistributivity, Seq(left, rightLeft, rightRight), Nil)
//        def rightDistributivity = for {
//          (outerOperator, RearrangeableOperator(innerOperator, leftLeft, leftRight), right) <- RearrangeableOperator.unapply(term)
//          leftDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
//        } yield (innerOperator(outerOperator(leftLeft, right), outerOperator(leftRight, right)), leftDistributivity, Seq(leftLeft, leftRight, right), Nil)
//        leftDistributivity orElse rightDistributivity
//      }
//      applyTransform(getDistributivity)
//    }
//
//    def byIdentity = {
//      def getIdentity(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
//        def leftIdentity = (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//          leftIdentity <- operator.leftIdentities.iterator
//          matchingSteps <- matchTrees(left, leftIdentity.identityTerm, addRight(wrapper, operator, right))
//        } yield (right, leftIdentity, Seq(right), matchingSteps)).headOption
//        def rightIdentity = (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//          rightIdentity <- operator.rightIdentities.iterator
//          matchingSteps <- matchTrees(right, rightIdentity.identityTerm, addLeft(wrapper, operator, left))
//        } yield (left, rightIdentity, Seq(left), matchingSteps)).headOption
//        leftIdentity orElse rightIdentity
//      }
//      applyTransform(getIdentity)
//    }
//
//    def byAbsorber = {
//      def getAbsorber(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
//        def leftAbsorber = (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//          leftAbsorber <- operator.leftAbsorbers.iterator
//          matchingSteps <- matchTrees(left, leftAbsorber.absorberTerm, addRight(wrapper, operator, right))
//        } yield (leftAbsorber.absorberTerm, leftAbsorber, Seq(right), matchingSteps)).headOption
//        def rightAbsorber = (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//          rightAbsorber <- operator.rightAbsorbers.iterator
//          matchingSteps <- matchTrees(right, rightAbsorber.absorberTerm, addLeft(wrapper, operator, left))
//        } yield (rightAbsorber.absorberTerm, rightAbsorber, Seq(left), matchingSteps)).headOption
//        leftAbsorber orElse rightAbsorber
//      }
//      applyTransform(getAbsorber)
//    }
//
//    def byInverse = {
//      def getInverse(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
//        def leftInverse = (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//          inverse <- operator.inverses.iterator
//          leftInverse = inverse.leftInverse
//          matchingSteps <- matchTrees(left, inverse.inverseOperator(right), addRight(wrapper, operator, right))
//        } yield (leftInverse.identityTerm, leftInverse, Seq(right), matchingSteps)).headOption
//        def rightInverse = (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
//          inverse <- operator.inverses.iterator
//          rightInverse = inverse.rightInverse
//          matchingSteps <- matchTrees(right, inverse.inverseOperator(left), addLeft(wrapper, operator, left))
//        } yield (rightInverse.identityTerm, rightInverse, Seq(left), matchingSteps)).headOption
//        leftInverse orElse rightInverse
//      }
//      applyTransform(getInverse)
//    }
//
//    byDistributing orElse byIdentity orElse byAbsorber orElse byInverse
//  }
//
//  // Attempts to rearrange baseTerm such that it is an instance of an expectedResultOperator expression whose LHS is targetLeft
//  private def pullLeft(baseTerm: Term, targetLeft: Term, expectedResultOperator: RearrangeableOperator, wrapper: Wrapper[Term, T]): Option[(Seq[RearrangementStep[T]], Term)] = {
//    def matchingLeft = for {
//      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
//      steps <- matchTrees(currentLeft, targetLeft, addRight(wrapper, expectedResultOperator, currentRight))
//    } yield (steps, currentRight)
//
//    def matchingRight = for {
//      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
//      steps <- matchTrees(currentRight, targetLeft, addRight(wrapper, expectedResultOperator, currentLeft))
//      commutativityStep <- expectedResultOperator.commutativity.rearrangementStep(currentLeft, currentRight, wrapper, expansion)
//    } yield (commutativityStep +: steps, currentLeft)
//
//    def insideLeft = for {
//      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
//      (steps, remainingRight) <- pullLeft(currentLeft, targetLeft, expectedResultOperator, addRight(wrapper, expectedResultOperator, currentRight))
//      associativityStep <- expectedResultOperator.associativity.reversedRearrangementStep(targetLeft, remainingRight, currentRight, wrapper, expansion, reversal)
//    } yield (steps :+ associativityStep, expectedResultOperator(remainingRight, currentRight))
//
//    def insideRight = for {
//      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
//      (steps, remainingRight) <- pullLeft(currentRight, targetLeft, expectedResultOperator, addRight(wrapper, expectedResultOperator, currentLeft))
//      commutativityStep <- expectedResultOperator.commutativity.rearrangementStep(currentLeft, currentRight, wrapper, expansion)
//      associativityStep <- expectedResultOperator.associativity.reversedRearrangementStep(targetLeft, remainingRight, currentLeft, wrapper, expansion, reversal)
//    } yield ((commutativityStep +: steps) :+ associativityStep, expectedResultOperator(remainingRight, currentLeft))
//
//    def byRecursingInTarget = for {
//      (`expectedResultOperator`, _, _) <- RearrangeableOperator.unapply(baseTerm)
//      (`expectedResultOperator`, targetLeftLeft, targetLeftRight) <- RearrangeableOperator.unapply(targetLeft)
//      (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(baseTerm, targetLeftLeft, expectedResultOperator, wrapper)
//      (stepsForLeftRight, treeWithoutLeft) <- pullLeft(treeWithoutLeftLeft, targetLeftRight, expectedResultOperator, addLeft(wrapper, expectedResultOperator, targetLeftLeft))
//      associativityStep <- expectedResultOperator.associativity.rearrangementStep(targetLeftLeft, targetLeftRight, treeWithoutLeft, wrapper, expansion)
//    } yield (stepsForLeftLeft ++ stepsForLeftRight :+ associativityStep, treeWithoutLeft)
//
//    // target: a*b + ???
//    // turn base term into a*x
//    // turn x into b+z
//    // base term is now a*(b+z)
//    // distribute to (a*b + x*z)
//    def byDistributingInTarget = for {
//      (targetLeftOperator, targetLeftLeft, targetLeftRight) <- RearrangeableOperator.unapply(targetLeft)
//      distributivity <- provingContext.leftDistributivities.find(d => d.distributor == targetLeftOperator && d.distributee == expectedResultOperator)
//      (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(baseTerm, targetLeftLeft, targetLeftOperator, wrapper)
//      (stepsForLeftRight, treeWithoutLeft) <- pullLeft(treeWithoutLeftLeft, targetLeftRight, expectedResultOperator, addLeft(wrapper, targetLeftOperator, targetLeftLeft))
//      distributivityStep <- distributivity.rearrangementStep(targetLeftLeft, targetLeftRight, treeWithoutLeft, wrapper, expansion)
//    } yield (stepsForLeftLeft ++ stepsForLeftRight :+ distributivityStep, targetLeftOperator(targetLeftLeft, treeWithoutLeft))
//
//    // target: a * ???
//    // base term is b+c
//    // turn b into a*x
//    // turn c into a*y
//    // base term is now (a*x)+(a*y)
//    // distribute to a*(x+y)
//    def byDistributingInBase = for {
//      (baseOperator, baseLeft, baseRight) <- RearrangeableOperator.unapply(baseTerm)
//      distributivity <- provingContext.leftDistributivities.find(d => d.distributor == expectedResultOperator && d.distributee == baseOperator)
//      (stepsForLeft, baseLeftWithoutTargetLeft) <- pullLeft(baseLeft, targetLeft, expectedResultOperator, addRight(wrapper, baseOperator, baseRight))
//      (stepsForRight, baseRightWithoutTargetLeft) <- pullLeft(baseRight, targetLeft, expectedResultOperator, addLeft(wrapper, baseOperator, expectedResultOperator(targetLeft, baseLeftWithoutTargetLeft)))
//      distributivityStep <- distributivity.reversedRearrangementStep(targetLeft, baseLeftWithoutTargetLeft, baseRightWithoutTargetLeft, wrapper, expansion, reversal)
//    } yield (stepsForLeft ++ stepsForRight :+ distributivityStep, baseOperator(baseLeftWithoutTargetLeft, baseRightWithoutTargetLeft))
//
//
//    def byApplyingTransform = applyTransformToOneSide[Term](
//      baseTerm,
//      targetLeft,
//      wrapper,
//      addRight(wrapper, expectedResultOperator, _),
//      pullLeft(_, _, expectedResultOperator, wrapper))
//
//    matchingLeft orElse
//      matchingRight orElse
//      insideLeft orElse
//      insideRight orElse
//      byRecursingInTarget orElse
//      byDistributingInBase orElse
//      byDistributingInTarget orElse
//      byApplyingTransform
//  }

//  private def rebuildOperators(operators: Seq[ExpectedOperator], terms: Seq[Term]): Option[Term] = {
//    // Operators and terms are supplied outermost first
//    // e.g. Seq(×, +), Seq(y, x, t) -> (t+x)y
//    (operators, terms) match {
//      case (Nil, Seq(term)) =>
//        Some(term)
//      case (otherOperators :+ ExpectedUnaryOperator(unaryOperator), otherTerms :+ lastTerm) =>
//        rebuildOperators(otherOperators, otherTerms :+ unaryOperator(lastTerm))
//      case (otherOperators :+ ExpectedBinaryOperator(binaryOperator, _), otherTerms :+ right :+ left) =>
//        rebuildOperators(otherOperators, otherTerms :+ binaryOperator(left, right))
//      case _ =>
//        None
//    }
//  }

//  trait ExpectedOperator
//  case class ExpectedBinaryOperator(operator: RearrangeableOperator, expectedRight: Option[Term]) extends ExpectedOperator
//  case class ExpectedUnaryOperator(operator: UnaryOperator) extends ExpectedOperator
//
//  trait ResultTree {
//    def term: Term
//  }
//  object ResultTree {
//    def unapply(resultTree: ResultTree): Option[Term] = Some(resultTree.term)
//  }
//  case class Root(term: Term) extends ResultTree
//  case class BinaryOperatorResult(operator: RearrangeableOperator, left: ResultTree, right: Term) extends ResultTree {
//    def term = operator(left.term, right)
//  }
//  case class UnaryOperatorResult(operator: UnaryOperator, inner: ResultTree) extends ResultTree {
//    def term = operator(inner.term)
//  }

  trait DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T]
  }
  case class LeftBinaryDeconstructionStep(operator: RearrangeableOperator, left: OperatorTree) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(left.term, _)(_))
  }
  case class RightBinaryDeconstructionStep(operator: RearrangeableOperator, right: OperatorTree) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(_, right.term)(_))
  }
  case class UnaryDeconstructionStep(operator: UnaryOperator) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(_)(_))
  }

  def getWrapper(deconstruction: Seq[DeconstructionStep]): Wrapper[Term, T] = {
    deconstruction.foldLeft(mainWrapper) { case (w, d) => d.addToWrapper(w) }
  }

//  trait LastOperation
//  case object Commutativity extends LastOperation
//  case object ForwardAssociativity extends LastOperation
//  case object ReverseAssociativity extends LastOperation
//
//  // Attempts to rearrange base such that it is a nested series of the given operators whose leftmost term is the target
//  // Operators are supplied outermost first, and terms are returned the same way
//  private def rearrangeOperators(base: Term, target: Term, expectedOperators: Seq[ExpectedOperator], baseDeconstruction: Seq[OperatorDeconstruction], targetDeconstruction: Seq[OperatorDeconstruction], lastOperation: Option[LastOperation] = None): Option[(Seq[RearrangementStep[T]], Seq[RearrangementStep[T]], ResultTree)] = {
//
//    def matchingDirectly = if (expectedOperators.isEmpty && base == target) {
//      Some((Nil, Nil, Root(target)))
//    } else {
//      None
//    }
//
//    def matchingLeftBinary = for {
//      (ExpectedBinaryOperator(expectedOperator, expectedRight), otherOperators) <- +:.unapply(expectedOperators)
//      (baseLeft, baseRight) <- expectedOperator.unapply(base)
//      (baseStepsForLeft, targetStepsForLeft, leftResultTree) <- rearrangeOperators(baseLeft, target, otherOperators, baseDeconstruction :+ RightBinaryOperatorDeconstruction(expectedOperator, baseRight), targetDeconstruction)
//      (baseStepsForRight, targetStepsForRight, rightResult) <- matchExpectedResult(baseRight, expectedRight, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, leftResultTree.term))
//    } yield (baseStepsForLeft ++ baseStepsForRight, targetStepsForRight ++ targetStepsForLeft, BinaryOperatorResult(expectedOperator, leftResultTree, rightResult))
//
//    def matchingRightBinary = for {
//      (ExpectedBinaryOperator(expectedOperator, expectedRight), otherOperators) <- +:.unapply(expectedOperators)
//      (baseLeft, baseRight) <- expectedOperator.unapply(base)
//      commutativityStep <- expectedOperator.commutativity.rearrangementStep(baseLeft, baseRight, getWrapper(baseDeconstruction), expansion)
//      (baseStepsForLeft, targetStepsForLeft, leftResultTree) <- rearrangeOperators(baseRight, target, otherOperators, baseDeconstruction :+ RightBinaryOperatorDeconstruction(expectedOperator, baseLeft), targetDeconstruction)
//      (baseStepsForRight, targetStepsForRight, rightResult) <- matchExpectedResult(baseLeft, expectedRight, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, leftResultTree.term))
//    } yield (commutativityStep +: (baseStepsForLeft ++ baseStepsForRight), targetStepsForRight ++ targetStepsForLeft, BinaryOperatorResult(expectedOperator, leftResultTree, rightResult))
//
//    def matchingUnary = for {
//      (ExpectedUnaryOperator(expectedOperator), otherOperators) <- +:.unapply(expectedOperators)
//      baseInner <- expectedOperator.unapply(base)
//      (baseSteps, targetSteps, resultTree) <- rearrangeOperators(baseInner, target, otherOperators, baseDeconstruction :+ UnaryOperatorDeconstruction(expectedOperator), targetDeconstruction)
//    } yield (baseSteps, targetSteps, UnaryOperatorResult(expectedOperator, resultTree))
//
//    // bL + bR = (iL + iR) + bR = iL + (iR + bR)
//    def associativityInBase = for {
//      (ExpectedBinaryOperator(expectedOperator, expectedRight), otherOperators) <- +:.unapply(expectedOperators)
//      (baseLeft, baseRight) <- expectedOperator.unapply(base)
//      (baseStepsForLeft, targetStepsForLeft, BinaryOperatorResult(`expectedOperator`, innerLeft, innerRight)) <-
//        rearrangeOperators(baseLeft, target, ExpectedBinaryOperator(expectedOperator, None) +: otherOperators, baseDeconstruction :+ RightBinaryOperatorDeconstruction(expectedOperator, baseRight), targetDeconstruction)
//      (baseStepsForRight, targetStepsForRight, rightResult) <- matchExpectedResult(expectedOperator(innerRight, baseRight), expectedRight, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, innerLeft.term))
//      associativityStep <- expectedOperator.associativity.reversedRearrangementStep(innerLeft.term, innerRight, baseRight, getWrapper(baseDeconstruction), expansion, reversal)
//    } yield ((baseStepsForLeft :+ associativityStep) ++ baseStepsForRight, targetStepsForRight ++ targetStepsForLeft, BinaryOperatorResult(expectedOperator, innerLeft, rightResult))
//
//    // b = tL + rR = tL + (tR + y) = (tL + tR) + y
//    def associativityInTarget = for {
//      ExpectedBinaryOperator(expectedOperator, expectedRight) <- expectedOperators.single
//      (targetLeft, targetRight) <- expectedOperator.unapply(target)
//      (baseStepsForTargetLeft, targetStepsForTargetLeft, BinaryOperatorResult(`expectedOperator`, ResultTree(`targetLeft`), baseAfterTargetLeft)) <-
//        rearrangeOperators(base, targetLeft, Seq(ExpectedBinaryOperator(expectedOperator, None)), baseDeconstruction, targetDeconstruction :+ RightBinaryOperatorDeconstruction(expectedOperator, targetRight))
//      (baseStepsForTargetRight, targetStepsForTargetRight, BinaryOperatorResult(`expectedOperator`, ResultTree(`targetRight`), baseAfterTargetRight)) <-
//        rearrangeOperators(baseAfterTargetLeft, targetRight, Seq(ExpectedBinaryOperator(expectedOperator, None)), baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, targetLeft), targetDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, targetLeft))
//      (baseStepsForExpectedRight, targetStepsForExpectedRight, resultTerm) <- matchExpectedResult(baseAfterTargetRight, expectedRight, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, target))
//      associativityStep <- expectedOperator.associativity.rearrangementStep(targetLeft, targetRight, baseAfterTargetRight, getWrapper(baseDeconstruction), expansion)
//    } yield ((baseStepsForTargetLeft ++ baseStepsForTargetRight :+ associativityStep) ++ baseStepsForExpectedRight, targetStepsForExpectedRight ++ targetStepsForTargetRight ++ targetStepsForTargetLeft, BinaryOperatorResult(expectedOperator, Root(target), resultTerm))
//
//    // b = bL + bR = t x + t y = t (x + y)
//    def reverseLeftDistributivityInBase = for {
//      (ExpectedBinaryOperator(expectedOperator, expectedRight), otherOperators) <- +:.unapply(expectedOperators)
//      (baseOperator, baseLeft, baseRight) <- RearrangeableOperator.unapply(base)
//      leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == expectedOperator && d.distributee == baseOperator)
//      (baseStepsForBaseLeft, targetStepsForBaseLeft, BinaryOperatorResult(`expectedOperator`, innerResult, x)) <- rearrangeOperators(baseLeft, target, ExpectedBinaryOperator(expectedOperator, None) +: otherOperators, baseDeconstruction :+ RightBinaryOperatorDeconstruction(baseOperator, baseRight), targetDeconstruction)
//      (baseStepsForBaseRight, targetStepsForBaseRight, BinaryOperatorResult(`expectedOperator`, `innerResult`, y)) <- rearrangeOperators(baseRight, target, ExpectedBinaryOperator(expectedOperator, None) +: otherOperators, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(baseOperator, baseLeft), targetDeconstruction)
//      (baseStepsForExpectedRight, targetStepsForExpectedRight, resultTerm) <- matchExpectedResult(baseOperator(x, y), expectedRight, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, target))
//      distributivityStep <- leftDistributivity.reversedRearrangementStep(innerResult.term, x, y, getWrapper(baseDeconstruction), expansion, reversal)
//    } yield ((baseStepsForBaseLeft ++ baseStepsForBaseRight :+ distributivityStep) ++ baseStepsForExpectedRight, targetStepsForExpectedRight ++ targetStepsForBaseRight ++ targetStepsForBaseLeft, BinaryOperatorResult(expectedOperator, innerResult, resultTerm))
//
//    // b = bL + bR = tL x + tR y = tL x + tR x = (tL + tR)x
//    def reverseRightDistributivityInBase = for {
//      (ExpectedBinaryOperator(expectedOperator, expectedRight), otherOperators) <- +:.unapply(expectedOperators)
//      (baseOperator, baseLeft, baseRight) <- RearrangeableOperator.unapply(base)
//      (targetLeft, targetRight) <- baseOperator.unapply(target)
//      rightDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == expectedOperator && d.distributee == baseOperator)
//      (baseStepsForBaseLeft, targetStepsForBaseLeft, BinaryOperatorResult(`expectedOperator`, leftResult, x)) <-
//        rearrangeOperators(baseLeft, targetLeft, ExpectedBinaryOperator(expectedOperator, None) +: otherOperators, baseDeconstruction :+ RightBinaryOperatorDeconstruction(baseOperator, baseRight), targetDeconstruction :+ RightBinaryOperatorDeconstruction(baseOperator, targetRight))
//      (baseStepsForBaseRight, targetStepsForBaseRight, BinaryOperatorResult(`expectedOperator`, rightResult, y)) <-
//        rearrangeOperators(baseRight, targetRight, ExpectedBinaryOperator(expectedOperator, None) +: otherOperators, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(baseOperator, baseLeft), targetDeconstruction :+ LeftBinaryOperatorDeconstruction(baseOperator, targetLeft))
//      stepsForMatching <- matchTrees(y, x, baseDeconstruction :+ RightBinaryOperatorDeconstruction(baseOperator, expectedOperator(leftResult.term, x)) :+ LeftBinaryOperatorDeconstruction(expectedOperator, rightResult.term))
//      (baseStepsForExpectedRight, targetStepsForExpectedRight, resultTerm) <- matchExpectedResult(x, expectedRight, baseDeconstruction :+ LeftBinaryOperatorDeconstruction(expectedOperator, target))
//      distributivityStep <- rightDistributivity.reversedRearrangementStep(leftResult.term, rightResult.term, x, getWrapper(baseDeconstruction), expansion, reversal)
//    } yield (
//      (baseStepsForBaseLeft ++ baseStepsForBaseRight ++ stepsForMatching :+ distributivityStep) ++ baseStepsForExpectedRight,
//      targetStepsForExpectedRight ++ targetStepsForBaseRight ++ targetStepsForBaseLeft,
//      BinaryOperatorResult(expectedOperator, Root(baseOperator(leftResult.term, rightResult.term)), resultTerm)
//    )
//
//    def replaceBaseOrTarget(f: (Term, Seq[OperatorDeconstruction], Direction) => Option[(Seq[RearrangementStep[T]], RearrangementOperation, Seq[Term])]) = {
//      def replaceBase = for {
//        (matchingSteps, rearrangementOperation, rearrangementTerms) <- f(base, baseDeconstruction, Direction.Forward)
//        newBase = rearrangementOperation.result(rearrangementTerms)
//        (baseInnerSteps, targetInnerSteps, result) <- rearrangeOperators(newBase, target, expectedOperators, baseDeconstruction, targetDeconstruction)
//        rearrangementStep <- rearrangementOperation.rearrangementStep(rearrangementTerms, getWrapper(baseDeconstruction), expansion)
//      } yield ((matchingSteps :+ rearrangementStep) ++ baseInnerSteps, targetInnerSteps, result)
//      def replaceTarget = for {
//        (matchingSteps, rearrangementOperation, rearrangementTerms) <- f(target, targetDeconstruction, Direction.Reverse)
//        newTarget = rearrangementOperation.result(rearrangementTerms)
//        (baseInnerSteps, targetInnerSteps, result) <- rearrangeOperators(base, newTarget, expectedOperators, baseDeconstruction, targetDeconstruction)
//        rearrangementStep <- rearrangementOperation.reversedRearrangementStep(rearrangementTerms, getWrapper(targetDeconstruction), expansion, reversal)
//      } yield (baseInnerSteps, targetInnerSteps ++ (rearrangementStep +: matchingSteps), result)
//      replaceBase orElse replaceTarget
//    }
//
//    def replaceDistributivity = {
//      def leftDistributivity = replaceBaseOrTarget { (term, _, _) =>
//        (for {
//          (outerOperator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          (innerOperator, rightLeft, rightRight) <- RearrangeableOperator.unapply(right).toSeq
//          leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
//        } yield (Nil, leftDistributivity, Seq(left, rightLeft, rightRight))).headOption
//      }
//      def rightDistributivity = replaceBaseOrTarget { (term, _, _) =>
//        (for {
//          (outerOperator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          (innerOperator, leftLeft, leftRight) <- RearrangeableOperator.unapply(left).toSeq
//          rightDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
//        } yield (Nil, rightDistributivity, Seq(leftLeft, leftRight, right))).headOption
//      }
//      leftDistributivity orElse rightDistributivity
//    }
//
//    def replaceIdentity = {
//      def leftIdentity = replaceBaseOrTarget { (term, deconstruction, direction) =>
//        (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          leftIdentity <- operator.leftIdentities
//          (source, result) = direction.swapSourceAndResult(left, leftIdentity.identityTerm)
//          matchingSteps <- matchTrees(source, result, deconstruction :+ RightBinaryOperatorDeconstruction(operator, right))
//        } yield (matchingSteps, leftIdentity, Seq(right))).headOption
//      }
//      def rightIdentity = replaceBaseOrTarget { (term, deconstruction, direction) =>
//        (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          rightIdentity <- operator.rightIdentities
//          (source, result) = direction.swapSourceAndResult(right, rightIdentity.identityTerm)
//          matchingSteps <- matchTrees(source, result, deconstruction :+ LeftBinaryOperatorDeconstruction(operator, left))
//        } yield (matchingSteps, rightIdentity, Seq(left))).headOption
//      }
//      leftIdentity orElse rightIdentity
//    }
//
//    def replaceAbsorber = {
//      def leftAbsorber = replaceBaseOrTarget { (term, deconstruction, direction) =>
//        (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          leftAbsorber <- operator.leftAbsorbers
//          (source, result) = direction.swapSourceAndResult(left, leftAbsorber.absorberTerm)
//          matchingSteps <- matchTrees(source, result, deconstruction :+ RightBinaryOperatorDeconstruction(operator, right))
//        } yield (matchingSteps, leftAbsorber, Seq(right))).headOption
//      }
//      def rightAbsorber = replaceBaseOrTarget { (term, deconstruction, direction) =>
//        (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          rightAbsorber <- operator.rightAbsorbers
//          (source, result) = direction.swapSourceAndResult(right, rightAbsorber.absorberTerm)
//          matchingSteps <- matchTrees(source, result, deconstruction :+ LeftBinaryOperatorDeconstruction(operator, left))
//        } yield (matchingSteps, rightAbsorber, Seq(left))).headOption
//      }
//      leftAbsorber orElse rightAbsorber
//    }
//
//    def replaceInverse = {
//      def leftInverse = replaceBaseOrTarget { (term, deconstruction, direction) =>
//        (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          inverse <- operator.inverses
//          (source, result) = direction.swapSourceAndResult(left, inverse.inverseOperator(right))
//          matchingSteps <- matchTrees(source, result, deconstruction :+ RightBinaryOperatorDeconstruction(operator, right))
//        } yield (matchingSteps, inverse.leftInverse, Seq(right))).headOption
//      }
//      def rightInverse = replaceBaseOrTarget { (term, deconstruction, direction) =>
//        (for {
//          (operator, left, right) <- RearrangeableOperator.unapply(term).toSeq
//          inverse <- operator.inverses
//          (source, result) = direction.swapSourceAndResult(right, inverse.inverseOperator(left))
//          matchingSteps <- matchTrees(source, result, deconstruction :+ LeftBinaryOperatorDeconstruction(operator, left))
//        } yield (matchingSteps, inverse.rightInverse, Seq(left))).headOption
//      }
//      leftInverse orElse rightInverse
//    }
//
//    // b = x + y = tL + y = tL + tR
//    def addBinaryOperatorFromTarget = for {
//      (operator, targetLeft, targetRight) <- RearrangeableOperator.unapply(target)
//      (baseSteps, targetSteps, resultTree) <- rearrangeOperators(base, targetLeft, expectedOperators :+ ExpectedBinaryOperator(operator, Some(targetRight)), baseDeconstruction, targetDeconstruction :+ RightBinaryOperatorDeconstruction(operator, targetRight))
//    } yield (baseSteps, targetSteps, resultTree)
//
//    def addUnaryOperatorFromTarget = for {
//      (operator, innerTerm) <- UnaryOperator.unapply(target)
//      (baseSteps, targetSteps, innerTerms) <- rearrangeOperators(base, innerTerm, expectedOperators :+ ExpectedUnaryOperator(operator), baseDeconstruction, targetDeconstruction :+ UnaryOperatorDeconstruction(operator))
//    } yield (baseSteps, targetSteps, innerTerms)
//
//    def commutingBase = for {
//      (operator, baseLeft, baseRight) <- RearrangeableOperator.unapply(base)
//      if !lastOperation.contains(Commutativity) && !lastOperation.contains(ForwardAssociativity) && !lastOperation.contains(ReverseAssociativity)
//      (baseSteps, targetSteps, result) <- rearrangeOperators(operator(baseRight, baseLeft), target, expectedOperators, baseDeconstruction, targetDeconstruction, Some(Commutativity))
//      commutativityStep <- operator.commutativity.rearrangementStep(baseLeft, baseRight, getWrapper(baseDeconstruction), expansion)
//    } yield (commutativityStep +: baseSteps, targetSteps, result)
//
//    def forwardAssociatingBase = for {
//      (ExpectedBinaryOperator(expectedOperator, _), _) <- +:.unapply(expectedOperators)
//      (operator, baseLeft, baseRight) <- RearrangeableOperator.unapply(base)
//      if (operator != expectedOperator)
//      if !lastOperation.contains(ReverseAssociativity)
//      (baseRightLeft, baseRightRight) <- operator.unapply(baseRight)
//      (baseSteps, targetSteps, result) <- rearrangeOperators(operator(operator(baseLeft, baseRightLeft), baseRightRight), target, expectedOperators, baseDeconstruction, targetDeconstruction, Some(ForwardAssociativity))
//      associativityStep <- operator.associativity.rearrangementStep(baseLeft, baseRightLeft, baseRightRight, getWrapper(baseDeconstruction), expansion)
//    } yield (associativityStep +: baseSteps, targetSteps, result)
//
//    def reverseAssociatingBase = for {
//      (ExpectedBinaryOperator(expectedOperator, _), _) <- +:.unapply(expectedOperators)
//      (operator, baseLeft, baseRight) <- RearrangeableOperator.unapply(base)
//      if (operator != expectedOperator)
//      if !lastOperation.contains(ForwardAssociativity)
//      (baseLeftLeft, baseLeftRight) <- operator.unapply(baseLeft)
//      (baseSteps, targetSteps, result) <- rearrangeOperators(operator(baseLeftLeft, operator(baseLeftRight, baseRight)), target, expectedOperators, baseDeconstruction, targetDeconstruction, Some(ReverseAssociativity))
//      associativityStep <- operator.associativity.reversedRearrangementStep(baseLeftLeft, baseLeftRight, baseRight, getWrapper(baseDeconstruction), expansion, reversal)
//    } yield (associativityStep +: baseSteps, targetSteps, result)
//
//    matchingDirectly orElse
//      matchingLeftBinary orElse
//      matchingRightBinary orElse
//      matchingUnary orElse
//      associativityInBase orElse
//      associativityInTarget orElse
//      reverseLeftDistributivityInBase orElse
//      reverseRightDistributivityInBase orElse
//      addBinaryOperatorFromTarget orElse
//      addUnaryOperatorFromTarget orElse
//      replaceDistributivity orElse
//      replaceIdentity orElse
//      replaceAbsorber orElse
//      replaceInverse orElse
//      commutingBase orElse
//      forwardAssociatingBase orElse
//      reverseAssociatingBase
//  }
//
//  private def matchExpectedResult(lhs: Term, rhsOption: Option[Term], deconstruction: Seq[OperatorDeconstruction]): Option[(Seq[RearrangementStep[T]], Seq[RearrangementStep[T]], Term)] = {
//    rhsOption.map(rearrangeOperators(lhs, _, Nil, deconstruction, deconstruction).map(_.map3(_.term))).getOrElse(Some((Nil, Nil, lhs)))
//  }
//
//  private def matchTrees(lhs: Term, rhs: Term, deconstruction: Seq[OperatorDeconstruction]): Option[Seq[RearrangementStep[T]]] = {
//    for {
//      (baseSteps, targetSteps, _) <- rearrangeOperators(lhs, rhs, Nil, deconstruction, deconstruction)
//    } yield baseSteps ++ targetSteps
//  }
//
//  private def matchTrees(lhs: Term, rhs: Term): Option[Seq[RearrangementStep[T]]] = {
//    matchTrees(lhs, rhs, Nil)
//  }

  private def pullLeftInMatchingOperatorWithoutDeconstructingTargetOrCommuting(
    operator: RearrangeableOperator,
    base: BinaryOperatorTree,
    targetLeft: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    def matchingLeft = for {
      matchingSteps <- matchTrees(base.left, targetLeft, deconstruction :+ RightBinaryDeconstructionStep(operator, base.right))
    } yield (matchingSteps, base.right)
    def insideLeft = for {
      (innerSteps, remainingOfBaseLeft) <- pullLeftWithoutDeconstructingTarget(base.left, operator, targetLeft, deconstruction :+ RightBinaryDeconstructionStep(operator, base.right))
      associativityStep <- operator.associativity.reversedRearrangementStep(targetLeft, remainingOfBaseLeft, base.right, getWrapper(deconstruction), expansion, reversal)
    } yield (innerSteps :+ associativityStep, BinaryOperatorTree(operator, remainingOfBaseLeft, base.right))
    matchingLeft orElse insideLeft
  }

  private def pullLeftInMatchingOperatorWithoutDeconstructingTarget(
    operator: RearrangeableOperator,
    base: BinaryOperatorTree,
    targetLeft: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    def withoutCommuting = pullLeftInMatchingOperatorWithoutDeconstructingTargetOrCommuting(operator, base, targetLeft, deconstruction)
    def byCommuting = for {
      (innerSteps, innerResult) <- pullLeftInMatchingOperatorWithoutDeconstructingTargetOrCommuting(operator, BinaryOperatorTree(operator, base.right, base.left), targetLeft, deconstruction)
      commutativityStep <- operator.commutativity.rearrangementStep(base.left, base.right, getWrapper(deconstruction), expansion)
    } yield (commutativityStep +: innerSteps, innerResult)
    withoutCommuting orElse byCommuting
  }

  private def pullLeftInNonMatchingOperatorWithoutDeconstructingTarget(
    base: BinaryOperatorTree,
    targetOperator: RearrangeableOperator,
    targetLeft: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    def byDirectlyDistributing = (for {
      (distributedBaseLeft, distributionSteps) <- findDistributivity(base, deconstruction, targetOperator, Direction.Forward)
      (innerSteps, remainingOfBaseLeft) <- pullLeftWithoutDeconstructingTarget(distributedBaseLeft, targetOperator, targetLeft, deconstruction)
    } yield (distributionSteps ++ innerSteps, remainingOfBaseLeft)).headOption
    def byPullingLeftInBothSides = for {
      distributivity <- provingContext.leftDistributivities.find(d => d.distributor == targetOperator && d.distributee == base.operator)
      (stepsForLeft, remainingOfBaseLeftLeft) <- pullLeftWithoutDeconstructingTarget(base.left, targetOperator, targetLeft, deconstruction :+ RightBinaryDeconstructionStep(base.operator, base.right))
      (stepsForRight, remainingOfBaseLeftRight) <- pullLeftWithoutDeconstructingTarget(base.right, targetOperator, targetLeft, deconstruction :+ LeftBinaryDeconstructionStep(base.operator, base.left))
      remainingOfBaseLeft = BinaryOperatorTree(base.operator, remainingOfBaseLeftLeft, remainingOfBaseLeftRight)
      distributivityStep <- distributivity.reversedRearrangementStep(targetLeft, remainingOfBaseLeftLeft, remainingOfBaseLeftRight, getWrapper(deconstruction), expansion, reversal)
    } yield (stepsForLeft ++ stepsForRight :+ distributivityStep, remainingOfBaseLeft)
    byDirectlyDistributing orElse byPullingLeftInBothSides
  }

  private def pullLeftWithoutDeconstructingTarget(
    base: OperatorTree,
    targetOperator: RearrangeableOperator,
    targetLeft: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    base match {
      case base @ BinaryOperatorTree(`targetOperator`, _, _) =>
        pullLeftInMatchingOperatorWithoutDeconstructingTarget(targetOperator, base, targetLeft, deconstruction)
      case base: BinaryOperatorTree =>
        pullLeftInNonMatchingOperatorWithoutDeconstructingTarget(base, targetOperator, targetLeft, deconstruction)
      case _ =>
        None
    }
  }

  private def pullLeftByDeconstructing(
    baseDeconstruction: Seq[DeconstructionStep],
    targetOperator: RearrangeableOperator,
    targetLeft: OperatorTree,
    targetDeconstruction: Seq[DeconstructionStep],
    recurse: OperatorTree => Option[(Seq[RearrangementStep[T]], OperatorTree)]
  ): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    def withoutDeconstructing = recurse(targetLeft)
    def byDeconstructing = targetLeft match {
      case BinaryOperatorTree(`targetOperator`, targetLeftLeft, targetLeftRight) =>
        for {
          (innerStepsForLeft, remainingAfterLeft) <- pullLeftByDeconstructing(baseDeconstruction, targetOperator, targetLeftLeft, targetDeconstruction :+ RightBinaryDeconstructionStep(targetOperator, targetLeftRight), recurse)
          (innerStepsForRight, remainingAfterRight) <- pullLeft(remainingAfterLeft, baseDeconstruction :+ LeftBinaryDeconstructionStep(targetOperator, targetLeftLeft), targetOperator, targetLeftRight, targetDeconstruction :+ LeftBinaryDeconstructionStep(targetOperator, targetLeftLeft))
          associativityStep <- targetOperator.associativity.rearrangementStep(targetLeftLeft, targetLeftRight, remainingAfterRight, getWrapper(baseDeconstruction), expansion)
        } yield (innerStepsForLeft ++ innerStepsForRight :+ associativityStep, remainingAfterRight)
      case targetLeft: BinaryOperatorTree =>
        (for {
          (distributedTargetLeft, distributionSteps) <- findDistributivity(targetLeft, targetDeconstruction, targetOperator, Direction.Reverse)
          (innerSteps, innerResult) <- pullLeftByDeconstructing(baseDeconstruction, targetOperator, distributedTargetLeft, targetDeconstruction, recurse)
        } yield (innerSteps ++ distributionSteps, innerResult)).headOption
      case _ =>
        None
    }
    withoutDeconstructing orElse byDeconstructing
  }

  private def pullLeft(
    base: OperatorTree,
    baseDeconstruction: Seq[DeconstructionStep],
    targetOperator: RearrangeableOperator,
    targetLeft: OperatorTree,
    targetDeconstruction: Seq[DeconstructionStep]
  ): Option[(Seq[RearrangementStep[T]], OperatorTree)] = {
    base match {
      case base @ BinaryOperatorTree(`targetOperator`, _, _) =>
        pullLeftByDeconstructing(baseDeconstruction, targetOperator, targetLeft, targetDeconstruction, pullLeftInMatchingOperatorWithoutDeconstructingTarget(targetOperator, base, _, baseDeconstruction))
      case base: BinaryOperatorTree =>
        pullLeftByDeconstructing(baseDeconstruction, targetOperator, targetLeft, targetDeconstruction, pullLeftInNonMatchingOperatorWithoutDeconstructingTarget(base, targetOperator, _, baseDeconstruction))
      case _ =>
        None
    }
  }

  private def findInMatchingBinaryOperator(
    operator: RearrangeableOperator,
    base: BinaryOperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    for {
      (stepsToPullLeft, remainingRight) <- pullLeft(base, deconstruction, operator, target.left, deconstruction :+ RightBinaryDeconstructionStep(operator, target.right))
      stepsToMatchRight <- matchTrees(remainingRight, target.right, deconstruction :+ LeftBinaryDeconstructionStep(operator, target.left))
    } yield stepsToPullLeft ++ stepsToMatchRight
  }

  private def findDistributivity(
    source: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep],
    resultOperator: RearrangeableOperator,
    searchDirection: Direction
  ): Seq[(BinaryOperatorTree, Seq[RearrangementStep[T]])] = {
    def left = for {
      distributivity <- provingContext.leftDistributivities.find(d => d.distributor == source.operator && d.distributee == resultOperator).toSeq
      (result, steps) <- findDistributivity(source, deconstruction, resultOperator, distributivity, Direction.Forward, searchDirection)
    } yield (result, steps)
    def right = for {
      distributivity <- provingContext.rightDistributivities.find(d => d.distributor == source.operator && d.distributee == resultOperator).toSeq
      (result, steps) <- findDistributivity(source, deconstruction, resultOperator, distributivity, Direction.Reverse, searchDirection)
    } yield (result, steps)
    left ++ right
  }

  private def findDistributivity(
    source: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep],
    resultOperator: RearrangeableOperator,
    distributivity: Distributivity,
    distributivityDirection: Direction,
    searchDirection: Direction
  ): Seq[(BinaryOperatorTree, Seq[RearrangementStep[T]])] = {
    val (distributor, distributee) = distributivityDirection.swapSourceAndResult(source.left, source.right)

    def recurse(newSource: BinaryOperatorTree): Seq[(BinaryOperatorTree, Seq[RearrangementStep[T]])] = findDistributivity(
      newSource,
      deconstruction,
      resultOperator,
      distributivity,
      distributivityDirection,
      searchDirection)

    distributee match {
      case BinaryOperatorTree(`resultOperator`, innerLeft, innerRight) =>
        val distributedSource = BinaryOperatorTree(
          resultOperator,
          BinaryOperatorTree(source.operator, distributor, innerLeft, distributivityDirection),
          BinaryOperatorTree(source.operator, distributor, innerRight, distributivityDirection))
        for {
          distributivityStep <- distributivity.rearrangementStep(distributivityDirection.prepend(distributor, Seq(innerLeft, innerRight)), searchDirection, getWrapper(deconstruction), expansion, reversal).toSeq
        } yield (distributedSource, Seq(distributivityStep))
      case BinaryOperatorTree(source.operator, innerLeft, innerRight) =>
        def withoutCommuting = for {
          (innerResult, innerSteps) <- recurse(BinaryOperatorTree(
            source.operator,
            BinaryOperatorTree(source.operator, distributor, innerLeft, distributivityDirection),
            innerRight,
            distributivityDirection))
          associativityStep <- source.operator.associativity.rearrangementStep(distributivityDirection.reverseIfNecessary(Seq(distributor, innerLeft, innerRight)), searchDirection, getWrapper(deconstruction), expansion, reversal)
        } yield (innerResult, associativityStep +: innerSteps)
        def withCommuting = for {
          (innerResult, innerSteps) <- recurse(BinaryOperatorTree(
            source.operator,
            BinaryOperatorTree(source.operator, distributor, innerRight, distributivityDirection),
            innerLeft,
            distributivityDirection))
          commutativityStep <- source.operator.commutativity.rearrangementStep(
            searchDirection.reverseIfNecessary(Seq(innerLeft, innerRight)),
            getWrapper(deconstruction :+ distributivityDirection.getSource(LeftBinaryDeconstructionStep(source.operator, distributor), RightBinaryDeconstructionStep(source.operator, distributor))),
            expansion)
          associativityStep <- source.operator.associativity.rearrangementStep(distributivityDirection.reverseIfNecessary(Seq(distributor, innerRight, innerLeft)), searchDirection, getWrapper(deconstruction), expansion, reversal)
        } yield (innerResult, commutativityStep +: associativityStep +: innerSteps)
        withoutCommuting ++ withCommuting
      case _ =>
        Nil
    }
  }

  private def findMismatchedBinaryOperators(
    base: BinaryOperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    def byDistributingBase = (for {
      (distributedBase, distributionSteps) <- findDistributivity(base, deconstruction, target.operator, Direction.Forward)
      innerSteps <- findInMatchingBinaryOperator(target.operator, distributedBase, target, deconstruction)
    } yield distributionSteps ++ innerSteps).headOption
    def byDistributingTarget = (for {
      (distributedTarget, distributionSteps) <- findDistributivity(target, deconstruction, base.operator, Direction.Reverse)
      innerSteps <- findInMatchingBinaryOperator(base.operator, base, distributedTarget, deconstruction)
    } yield innerSteps ++ distributionSteps.reverse).headOption

    byDistributingBase orElse byDistributingTarget
  }

  private def findBinaryOperator(
    base: OperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    base match {
      case baseAsBinaryOperatorTree @ BinaryOperatorTree(target.operator, _, _) =>
        findInMatchingBinaryOperator(target.operator, baseAsBinaryOperatorTree, target, deconstruction)
      case baseAsBinaryOperatorTree: BinaryOperatorTree =>
        findMismatchedBinaryOperators(baseAsBinaryOperatorTree, target, deconstruction)
      case _ =>
        None
    }
  }

  private def matchTrees(base: OperatorTree, target: OperatorTree, deconstruction: Seq[DeconstructionStep]): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else if (base.canonicalForm != target.canonicalForm)
      None
    else target match {
      case targetAsBinaryOperatorTree: BinaryOperatorTree =>
        findBinaryOperator(base, targetAsBinaryOperatorTree, deconstruction)
      case _ =>
        None
    }
  }
  private def matchTrees(base: Term, target: Term): Option[Seq[RearrangementStep[T]]] = {
    matchTrees(OperatorTree.parse(base), OperatorTree.parse(target), Nil)
  }

  def rearrange(baseLhs: Term, baseRhs: Term): Option[Seq[RearrangementStep[T]]] = {
    def rearrangeDirectly: Option[Seq[RearrangementStep[T]]] = matchTrees(baseLhs, baseRhs)

    def rearrangeUsingPremise(premiseLhs: Term, premiseRhs: Term): Option[Seq[RearrangementStep[T]]] = {
      (for {
        lhsMatch <- matchTrees(baseLhs, premiseLhs)
        rhsMatch <- matchTrees(premiseRhs, baseRhs)
        joiner = RearrangementStep(mainWrapper(premiseRhs), Nil, _ => None)
      } yield (lhsMatch :+ joiner) ++ rhsMatch) orElse
        (for {
          firstMatch <- matchTrees(baseLhs, premiseRhs)
          secondMatch <- matchTrees(premiseLhs, baseRhs)
          joiner = equality.reversalRearrangementStep(premiseRhs, premiseLhs, mainWrapper, expansion)
        } yield (firstMatch :+ joiner) ++ secondMatch)
    }

    def rearrangeUsingPremises: Option[Seq[RearrangementStep[T]]] = (for {
      premise <- allPremises
      (premiseLhs, premiseRhs) <- equality.unapply(premise.statement).toSeq
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
      TermRearranger(equality, expansion, reversal, wrapper).rearrange(lhs, rhs) orElse
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
