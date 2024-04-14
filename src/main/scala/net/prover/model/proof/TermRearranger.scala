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

  trait DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T]
  }
  case class LeftBinaryDeconstructionStep(operator: RearrangeableOperator, left: OperatorTree) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(left.term, _)(_))
  }
  object LeftBinaryDeconstructionStep {
    def apply(binaryOperatorTree: BinaryOperatorTree): LeftBinaryDeconstructionStep = LeftBinaryDeconstructionStep(binaryOperatorTree.operator, binaryOperatorTree.left)
  }
  case class RightBinaryDeconstructionStep(operator: RearrangeableOperator, right: OperatorTree) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(_, right.term)(_))
  }
  object RightBinaryDeconstructionStep {
    def apply(binaryOperatorTree: BinaryOperatorTree): RightBinaryDeconstructionStep = RightBinaryDeconstructionStep(binaryOperatorTree.operator, binaryOperatorTree.right)
  }
  case class UnaryDeconstructionStep(operator: UnaryOperator) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(_)(_))
  }
  def BinaryDeconstructionStep(binaryOperatorTree: BinaryOperatorTree, direction: Direction): DeconstructionStep = {
    direction.getSource(
      RightBinaryDeconstructionStep(binaryOperatorTree),
      LeftBinaryDeconstructionStep(binaryOperatorTree))
  }
  def BinaryDeconstructionStep(operator: RearrangeableOperator, leftOrRight: OperatorTree, direction: Direction): DeconstructionStep = {
    direction.getSource(
      LeftBinaryDeconstructionStep(operator, leftOrRight),
      RightBinaryDeconstructionStep(operator, leftOrRight))
  }

  def getWrapper(deconstruction: Seq[DeconstructionStep]): Wrapper[Term, T] = {
    deconstruction.foldLeft(mainWrapper) { case (w, d) => d.addToWrapper(w) }
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
        val (innerSource, innerResult) = distributivityDirection.swapSourceAndResult(innerLeft, innerRight)
        def withoutCommuting = for {
          (distributedSource, innerSteps) <- recurse(BinaryOperatorTree(
            source.operator,
            BinaryOperatorTree(source.operator, distributor, innerSource, distributivityDirection),
            innerResult,
            distributivityDirection))
          associativityStep <- source.operator.associativity.rearrangementStep(distributivityDirection.reverseIfNecessary(Seq(distributor, innerSource, innerResult)), searchDirection.combine(distributivityDirection), getWrapper(deconstruction), expansion, reversal)
        } yield (distributedSource, searchDirection.prepend(associativityStep, innerSteps))
        def withCommuting = for {
          (distributedSource, innerSteps) <- recurse(BinaryOperatorTree(
            source.operator,
            BinaryOperatorTree(source.operator, distributor, innerResult, distributivityDirection),
            innerSource,
            distributivityDirection))
          commutativityStep <- source.operator.commutativity.rearrangementStep(
            searchDirection.reverseIfNecessary(Seq(innerLeft, innerRight)),
            getWrapper(deconstruction :+ distributivityDirection.getSource(LeftBinaryDeconstructionStep(source.operator, distributor), RightBinaryDeconstructionStep(source.operator, distributor))),
            expansion)
          associativityStep <- source.operator.associativity.rearrangementStep(distributivityDirection.reverseIfNecessary(Seq(distributor, innerResult, innerSource)), searchDirection.combine(distributivityDirection), getWrapper(deconstruction), expansion, reversal)
        } yield (distributedSource, searchDirection.prepend(commutativityStep, searchDirection.prepend(associativityStep, innerSteps)))
        withoutCommuting ++ withCommuting
      case _ =>
        Nil
    }
  }

  private def pullTargetInBaseWithoutCommuting(
    base: OperatorTree,
    baseDeconstruction: Seq[DeconstructionStep],
    target: OperatorTree,
    operator: RearrangeableOperator,
    stepDirection: Direction,
    pullDirection: Direction
  ): Option[(OperatorTree, Seq[RearrangementStep[T]])] = {
    def matching = for {
      base <- base.asOptionalInstanceOf[BinaryOperatorTree]
      if base.operator == operator
      (source, result) = pullDirection.swapSourceAndResult(base.left, base.right)
      stepsToMatchLeft <- matchTrees(source, target, baseDeconstruction :+ BinaryDeconstructionStep(base, pullDirection), stepDirection)
    } yield (result, stepsToMatchLeft)
    def inside = for {
      base <- base.asOptionalInstanceOf[BinaryOperatorTree]
      if base.operator == operator
      (source, result) = pullDirection.swapSourceAndResult(base.left, base.right)
      (innerResult, innerSteps) <- pullTargetInBaseWithoutSplitting(source, baseDeconstruction :+ BinaryDeconstructionStep(base, pullDirection), target, operator, stepDirection, pullDirection)
      associativityStep <- operator.associativity.rearrangementStep(pullDirection.reverseIfNecessary(Seq(target, innerResult, result)), stepDirection.combine(pullDirection).reverse, getWrapper(baseDeconstruction), expansion, reversal)
    } yield (BinaryOperatorTree(operator, innerResult, result, pullDirection), stepDirection.append(innerSteps, associativityStep))
    matching orElse inside
  }

  private def pullTargetInBaseWithoutSplitting(
    base: OperatorTree,
    baseDeconstruction: Seq[DeconstructionStep],
    target: OperatorTree,
    operator: RearrangeableOperator,
    stepDirection: Direction,
    pullDirection: Direction
  ): Option[(OperatorTree, Seq[RearrangementStep[T]])] = {
    def withoutCommuting = pullTargetInBaseWithoutCommuting(base, baseDeconstruction, target, operator, stepDirection, pullDirection)
    def byCommuting = for {
      base <- base.asOptionalInstanceOf[BinaryOperatorTree]
      if base.operator == operator
      (innerResult, innerSteps) <- pullTargetInBaseWithoutCommuting(BinaryOperatorTree(operator, base.right, base.left), baseDeconstruction, target, operator, stepDirection, pullDirection)
      commutativityStep <- operator.commutativity.rearrangementStep(base.left, base.right, stepDirection, getWrapper(baseDeconstruction), expansion, reversal)
    } yield (innerResult, stepDirection.prepend(commutativityStep, innerSteps))
    def byDistributing = (for {
      base <- base.asOptionalInstanceOf[BinaryOperatorTree].toSeq
      if base.operator != operator
      (distributedBase, distributionSteps) <- findDistributivity(base, baseDeconstruction, operator, stepDirection)
      (innerResult, innerSteps) <- pullTargetInBaseWithoutSplitting(distributedBase, baseDeconstruction, target, operator, stepDirection, pullDirection)
    } yield (innerResult, stepDirection.concat(distributionSteps, innerSteps))).headOption
    def byPushingUnaryOperator = {
      def pushLeft = for {
        base <- base.asOptionalInstanceOf[UnaryOperatorTree]
        baseInner <- base.inner.asOptionalInstanceOf[BinaryOperatorTree]
        extraction <- provingContext.leftOperatorExtractions.find(e => e.unaryOperator == base.operator && e.binaryOperator == baseInner.operator.operator)
        (innerResult, innerSteps) <- pullTargetInBaseWithoutSplitting(BinaryOperatorTree(baseInner.operator, UnaryOperatorTree(base.operator, baseInner.left), baseInner.right), baseDeconstruction, target, operator, stepDirection, pullDirection)
        extractionStep <- extraction.rearrangementStep(baseInner.left, baseInner.right, stepDirection.reverse, getWrapper(baseDeconstruction), expansion, reversal)
      } yield (innerResult, stepDirection.prepend(extractionStep, innerSteps))
      def pushRight = for {
        base <- base.asOptionalInstanceOf[UnaryOperatorTree]
        baseInner <- base.inner.asOptionalInstanceOf[BinaryOperatorTree]
        extraction <- provingContext.rightOperatorExtractions.find(e => e.unaryOperator == base.operator && e.binaryOperator == baseInner.operator.operator)
        (innerResult, innerSteps) <- pullTargetInBaseWithoutSplitting(BinaryOperatorTree(baseInner.operator, baseInner.left, UnaryOperatorTree(base.operator, baseInner.right)), baseDeconstruction, target, operator, stepDirection, pullDirection)
        extractionStep <- extraction.rearrangementStep(baseInner.left, baseInner.right, stepDirection.reverse, getWrapper(baseDeconstruction), expansion, reversal)
      } yield (innerResult, stepDirection.prepend(extractionStep, innerSteps))
      pushLeft orElse pushRight
    }
    withoutCommuting orElse byCommuting orElse byDistributing orElse byPushingUnaryOperator
  }

  private def pullTargetInBase(
    base: OperatorTree,
    target: OperatorTree,
    deconstruction: Seq[DeconstructionStep],
    operator: RearrangeableOperator,
    stepDirection: Direction,
    pullDirection: Direction
  ): Option[(OperatorTree, Seq[RearrangementStep[T]])] = {
    def withoutSplitting = pullTargetInBaseWithoutSplitting(base, deconstruction, target, operator, stepDirection, pullDirection)
    def bySplittingTarget = for {
      BinaryOperatorTree(`operator`, targetLeft, targetRight) <- target.asOptionalInstanceOf[BinaryOperatorTree]
      (baseAfterTargetLeft, leftSteps) <- pullTargetInBase(base, targetLeft, deconstruction, operator, stepDirection, pullDirection)
      (baseAfterTarget, rightSteps) <- pullTargetInBase(baseAfterTargetLeft, targetRight, deconstruction :+ LeftBinaryDeconstructionStep(operator, targetLeft), operator, stepDirection, pullDirection)
      associativityStep <- operator.associativity.rearrangementStep(targetLeft, targetRight, baseAfterTarget, stepDirection, getWrapper(deconstruction), expansion, reversal)
    } yield (baseAfterTarget, stepDirection.append(stepDirection.concat(leftSteps, rightSteps), associativityStep))
    def byDistributingTargetLeft = (for {
      target <- target.asOptionalInstanceOf[BinaryOperatorTree].toSeq
      if target.operator != operator
      (distributedTarget, _) <- findDistributivity(target, Nil, operator, stepDirection.reverse)
      (result, innerSteps) <- pullTargetInBase(base, distributedTarget, deconstruction, operator, stepDirection, pullDirection)
      (_, distributionSteps) <- findDistributivity(target, deconstruction :+ RightBinaryDeconstructionStep(operator, result), operator, stepDirection.reverse).find(_._1 == distributedTarget)
    } yield (result, stepDirection.concat(innerSteps, distributionSteps))).headOption
    withoutSplitting orElse bySplittingTarget orElse byDistributingTargetLeft
  }

  private def removeLeftmostIdentity(tree: BinaryOperatorTree, deconstruction: Seq[DeconstructionStep], stepDirection: Direction): Option[(OperatorTree, Seq[RearrangementStep[T]])] = {
    def directly = (for {
      identity <- tree.operator.leftIdentities
      stepsToMatch <- matchTrees(tree.left, Leaf(identity.identityTerm), deconstruction :+ RightBinaryDeconstructionStep(tree), stepDirection)
      identityStep <- identity.rearrangementStep(tree.right, stepDirection, getWrapper(deconstruction), expansion, reversal)
    } yield (tree.right, stepDirection.append(stepsToMatch, identityStep))).headOption
    def insideLeft = for {
      targetLeft <- tree.left.asOptionalInstanceOf[BinaryOperatorTree]
      if targetLeft.operator == tree.operator
      (innerResult, innerSteps) <- removeLeftmostIdentity(targetLeft, deconstruction :+ RightBinaryDeconstructionStep(tree), stepDirection)
    } yield (BinaryOperatorTree(tree.operator, innerResult, tree.right), innerSteps)
    directly orElse insideLeft
  }

  private def extractUnaryOperator(tree: OperatorTree, unaryOperator: UnaryOperator, deconstruction: Seq[DeconstructionStep], direction: Direction): Seq[(OperatorTree, Seq[RearrangementStep[T]])] = {
    def direct = for {
      tree <- tree.asOptionalInstanceOf[UnaryOperatorTree].toSeq
      if tree.operator == unaryOperator
    } yield (tree.inner, Nil)

    def extractLeft = for {
      tree <- tree.asOptionalInstanceOf[BinaryOperatorTree].toSeq
      extraction <- provingContext.leftOperatorExtractions.find(e => e.unaryOperator == unaryOperator && e.binaryOperator == tree.operator.operator).iterator
      (innerResult, innerSteps) <- extractUnaryOperator(tree.left, unaryOperator, deconstruction :+ RightBinaryDeconstructionStep(tree), direction)
      extractionStep <- extraction.rearrangementStep(innerResult, tree.right, direction, getWrapper(deconstruction), expansion, reversal)
    } yield (BinaryOperatorTree(tree.operator, innerResult, tree.right), direction.append(innerSteps, extractionStep))

    def extractRight = for {
      tree <- tree.asOptionalInstanceOf[BinaryOperatorTree].toSeq
      extraction <- provingContext.rightOperatorExtractions.find(e => e.unaryOperator == unaryOperator && e.binaryOperator == tree.operator.operator).iterator
      (innerResult, innerSteps) <- extractUnaryOperator(tree.right, unaryOperator, deconstruction :+ LeftBinaryDeconstructionStep(tree), direction)
      extractionStep <- extraction.rearrangementStep(tree.left, innerResult, direction, getWrapper(deconstruction), expansion, reversal)
    } yield (BinaryOperatorTree(tree.operator, tree.left, innerResult), direction.append(innerSteps, extractionStep))

    direct ++ extractLeft ++ extractRight
  }
  
  private def removeLeftmostInverse(tree: BinaryOperatorTree, deconstruction: Seq[DeconstructionStep], stepDirection: Direction): Option[(OperatorTree, Seq[RearrangementStep[T]])] = {
    def directRightInverse = for {
      inverse <- tree.operator.inverse
      stepsToMatch <- matchTrees(tree.right, UnaryOperatorTree(inverse.inverseOperator, tree.left), deconstruction :+ LeftBinaryDeconstructionStep(tree), stepDirection)
      inverseStep <- inverse.rightInverse.rearrangementStep(tree.left, stepDirection, getWrapper(deconstruction), expansion, reversal)
    } yield (Leaf(inverse.identity.identityTerm), stepDirection.append(stepsToMatch, inverseStep))
    def directLeftInverse = (for {
      inverse <- tree.operator.inverse.toSeq
      (innerLeft, stepsToExtractOperator) <- extractUnaryOperator(tree.left, inverse.inverseOperator, deconstruction :+ RightBinaryDeconstructionStep(tree), stepDirection)
      stepsToMatch <- matchTrees(tree.right, innerLeft, deconstruction :+ LeftBinaryDeconstructionStep(tree.operator, UnaryOperatorTree(inverse.inverseOperator, innerLeft)), stepDirection)
      inverseStep <- inverse.leftInverse.rearrangementStep(innerLeft, stepDirection, getWrapper(deconstruction), expansion, reversal)
    } yield (Leaf(inverse.identity.identityTerm), stepDirection.concat(stepsToExtractOperator, stepDirection.append(stepsToMatch, inverseStep)))).headOption
    // a = 0 + a = (b + -b) + a = b + (-b + a) = b + (a + -b)
    def rightInversePullingLeft = for {
      inverse <- tree.operator.inverse
      invertedLeft = UnaryOperatorTree(inverse.inverseOperator, tree.left)
      (result, stepsToPullLeft) <- pullTargetInBase(tree.right, invertedLeft, deconstruction :+ LeftBinaryDeconstructionStep(tree), tree.operator, stepDirection, Direction.Forward)
      identityStep <- inverse.identity.leftIdentity.rearrangementStep(result, stepDirection, getWrapper(deconstruction), expansion, reversal)
      inverseStep <- inverse.rightInverse.rearrangementStep(tree.left, stepDirection, getWrapper(deconstruction :+ RightBinaryDeconstructionStep(tree.operator, result)), expansion, reversal)
      associativityStep <- tree.operator.associativity.rearrangementStep(tree.left, invertedLeft, result, stepDirection, getWrapper(deconstruction), expansion, reversal)
    } yield (result, stepDirection.concat(stepsToPullLeft, stepDirection.reverseIfNecessary(Seq(associativityStep, inverseStep, identityStep))))
    // a = 0 + a = (-b + b) + a = -b + (b + a) = -b + (a + b)
    def leftInversePullingLeft = (for {
      inverse <- tree.operator.inverse.toSeq
      (innerLeft, stepsToExtractOperator) <- extractUnaryOperator(tree.left, inverse.inverseOperator, deconstruction :+ RightBinaryDeconstructionStep(tree), stepDirection)
      (result, stepsToPullLeft) <- pullTargetInBase(tree.right, innerLeft, deconstruction :+ LeftBinaryDeconstructionStep(tree.operator, UnaryOperatorTree(inverse.inverseOperator, innerLeft)), tree.operator, stepDirection, Direction.Forward)
      identityStep <- inverse.identity.leftIdentity.rearrangementStep(result, stepDirection, getWrapper(deconstruction), expansion, reversal)
      inverseStep <- inverse.leftInverse.rearrangementStep(innerLeft, stepDirection, getWrapper(deconstruction :+ RightBinaryDeconstructionStep(tree.operator, result)), expansion, reversal)
      associativityStep <- tree.operator.associativity.rearrangementStep(UnaryOperatorTree(inverse.inverseOperator, innerLeft), innerLeft, result, stepDirection, getWrapper(deconstruction), expansion, reversal)
    } yield (result, stepDirection.concat(stepsToExtractOperator, stepDirection.concat(stepsToPullLeft, stepDirection.reverseIfNecessary(Seq(associativityStep, inverseStep, identityStep)))))).headOption
    // (a + b) + -a = a + (b + -a) = ... = b
    def insideLeft = for {
      left <- tree.left.asOptionalInstanceOf[BinaryOperatorTree]
      if left.operator == tree.operator
      (innerResult, innerSteps) <- removeLeftmostInverse(BinaryOperatorTree(tree.operator, left.left, BinaryOperatorTree(tree.operator, left.right, tree.right)), deconstruction, stepDirection)
      associativityStep <- tree.operator.associativity.rearrangementStep(left.left, left.right, tree.right, stepDirection.reverse, getWrapper(deconstruction), expansion, reversal)
    } yield (innerResult, stepDirection.prepend(associativityStep, innerSteps))
    directRightInverse orElse directLeftInverse orElse rightInversePullingLeft orElse leftInversePullingLeft orElse insideLeft
  }

  private def matchBaseToAbsorber(
    base: OperatorTree,
    target: Leaf,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    def withDirection(direction: Direction): Seq[Seq[RearrangementStep[T]]] = {
      for {
        base <- base.asOptionalInstanceOf[BinaryOperatorTree].toSeq
        (source, result) = direction.swapSourceAndResult(base.left, base.right)
        absorber <- direction.getSource(base.operator.leftAbsorbers, base.operator.rightAbsorbers)
        if target.rootTerm == absorber.absorberTerm
        stepsToMatch <- matchTrees(source, target, deconstruction :+ BinaryDeconstructionStep(base, direction))
        absorberStep <- absorber.rearrangementStep(result, getWrapper(deconstruction), expansion)
      } yield stepsToMatch :+ absorberStep
    }
    (withDirection(Direction.Forward) ++ withDirection(Direction.Reverse)).minByLength
  }

  private def matchBaseToIdentity(
    base: OperatorTree,
    target: Leaf,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else {
      def removeIdentity = for {
        base <- base.asOptionalInstanceOf[BinaryOperatorTree]
        (simplifiedBase, stepsToMatchInverse) <- removeLeftmostIdentity(base, deconstruction, Direction.Forward)
        innerSteps <- matchBaseToIdentity(simplifiedBase, target, deconstruction)
      } yield stepsToMatchInverse ++ innerSteps
      def removeInverse = for {
        base <- base.asOptionalInstanceOf[BinaryOperatorTree]
        (simplifiedBase, stepsToMatchInverse) <- removeLeftmostInverse(base, deconstruction, Direction.Forward)
        innerSteps <- matchBaseToIdentity(simplifiedBase, target, deconstruction)
      } yield stepsToMatchInverse ++ innerSteps
      removeIdentity orElse removeInverse
    }
  }

  private def matchTreeWithLeafTarget(
    base: OperatorTree,
    target: Leaf,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else {
      matchByPullingAndIdentity(base, target, deconstruction) orElse (base match {
        case base: BinaryOperatorTree =>
          matchBaseToAbsorber(base, target, deconstruction) orElse matchBaseToIdentity(base, target, deconstruction)
        case _ =>
          None
      })
    }
  }

  private def matchTreeWithUnaryTarget(
    base: OperatorTree,
    target: UnaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else {
      (for {
        (baseInner, stepsToExtract) <- extractUnaryOperator(base, target.operator, deconstruction, Direction.Forward)
        innerSteps <- matchTrees(baseInner, target.inner, deconstruction :+ UnaryDeconstructionStep(target.operator))
      } yield stepsToExtract ++ innerSteps).minByLength
    }
  }

  // Match "a" by matching "a + b" then matching "b" to "0"
  private def matchByPullingAndIdentity(base: OperatorTree, target: OperatorTree, deconstruction: Seq[DeconstructionStep]): Option[Seq[RearrangementStep[T]]] = {
    def withDirection(direction: Direction): Seq[Seq[RearrangementStep[T]]] = {
      for {
        base <- base.asOptionalInstanceOf[BinaryOperatorTree].toSeq
        identity <- direction.getSource(base.operator.rightIdentities, base.operator.leftIdentities)
        (result, stepsToPull) <- pullTargetInBase(base, target, deconstruction, base.operator, Direction.Forward, direction)
        stepsToMatch <- matchTrees(result, Leaf(identity.identityTerm), deconstruction :+ BinaryDeconstructionStep(base.operator, target, direction))
        identityStep <- identity.rearrangementStep(target, getWrapper(deconstruction), expansion)
      } yield stepsToPull ++ stepsToMatch :+ identityStep
    }
    (withDirection(Direction.Forward) ++ withDirection(Direction.Reverse)).minByLength
  }

  // Match "a + b" by matching "a" then matching "b" to "0"
  private def matchByMatchingAndIdentity(base: OperatorTree, target: BinaryOperatorTree, deconstruction: Seq[DeconstructionStep]): Option[Seq[RearrangementStep[T]]] = {
    def directly = (for {
      identity <- target.operator.rightIdentities
      stepsToMatchSource <- matchTrees(base, target.left, deconstruction)
      identityStep <- identity.reversedRearrangementStep(target.left, getWrapper(deconstruction), expansion, reversal)
      stepsToMatchResult <- matchTrees(Leaf(identity.identityTerm), target.right, deconstruction :+ LeftBinaryDeconstructionStep(target))
    } yield (stepsToMatchSource :+ identityStep) ++ stepsToMatchResult).headOption
    def insideLeft = for {
      targetLeft <- target.left.asOptionalInstanceOf[BinaryOperatorTree]
      if targetLeft.operator == target.operator
      innerSteps <- matchByMatchingAndIdentity(base, BinaryOperatorTree(target.operator, targetLeft.left, BinaryOperatorTree(target.operator, targetLeft.right, target.right)), deconstruction)
      associativityStep <- target.operator.associativity.rearrangementStep(targetLeft.left, targetLeft.right, target.right, getWrapper(deconstruction), expansion)
    } yield innerSteps :+ associativityStep
    directly orElse insideLeft
  }

  // Match "a * b" by matching "0" then matching "b" to "0"
  private def matchByAbsorber(base: OperatorTree, target: BinaryOperatorTree, deconstruction: Seq[DeconstructionStep]): Option[Seq[RearrangementStep[T]]] = {
    def withDirection(direction: Direction): Seq[Seq[RearrangementStep[T]]] = {
      for {
        absorber <- direction.getSource(target.operator.rightAbsorbers, target.operator.leftAbsorbers)
        (source, result) = direction.swapSourceAndResult(target.left, target.right)
        stepsToMatchBase <- matchTrees(base, Leaf(absorber.absorberTerm), deconstruction)
        stepsToMatchResult <- matchTrees(Leaf(absorber.absorberTerm), result, deconstruction)
        absorberStep <- absorber.reversedRearrangementStep(source, getWrapper(deconstruction), expansion, reversal)
      } yield (stepsToMatchBase :+ absorberStep) ++ stepsToMatchResult
    }
    (withDirection(Direction.Forward) ++ withDirection(Direction.Reverse)).minByLength
  }

  private def matchTreeWithBinaryTarget(
    base: OperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep],
    alreadyDistributedOperators: Seq[RearrangeableOperator]
  ): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else {
      def byPullingLeft = for {
        (baseAfterLeft, leftSteps) <- pullTargetInBase(base, target.left, deconstruction, target.operator, Direction.Forward, Direction.Forward)
        rightSteps <- matchTrees(baseAfterLeft, target.right, deconstruction :+ LeftBinaryDeconstructionStep(target))
      } yield leftSteps ++ rightSteps
      def byPullingAndIdentity = matchByPullingAndIdentity(base, target, deconstruction)
      def byMatchingAndIdentity = matchByMatchingAndIdentity(base, target, deconstruction)
      def byRemovingLeftmostIdentity = for {
        (targetAfterRemovingIdentity, stepsToRemoveIdentity) <- removeLeftmostIdentity(target, deconstruction, Direction.Reverse)
        stepsToMatch <- matchTrees(base, targetAfterRemovingIdentity)
      } yield stepsToMatch ++ stepsToRemoveIdentity
      def byRemovingLeftmostInverse = for {
        (targetAfterRemovingInverse, stepsToRemoveInverse) <- removeLeftmostInverse(target, deconstruction, Direction.Reverse)
        stepsToMatch <- matchTrees(base, targetAfterRemovingInverse, deconstruction)
      } yield stepsToMatch ++ stepsToRemoveInverse
      def byAbsorber = matchByAbsorber(base, target, deconstruction)
      def byDistributing = (for {
        distributivityOperator <- (provingContext.leftDistributivities ++ provingContext.rightDistributivities).filter(_.distributor == target.operator).map(_.distributee).distinct
        if !alreadyDistributedOperators.contains(distributivityOperator)
        (distributedTarget, targetDistributionSteps) <- findDistributivity(target, deconstruction, distributivityOperator, Direction.Reverse)
        innerSteps <- matchTreeWithBinaryTarget(base, distributedTarget, deconstruction, alreadyDistributedOperators :+ target.operator)
      } yield innerSteps ++ targetDistributionSteps).headOption
      byPullingLeft orElse byPullingAndIdentity orElse byMatchingAndIdentity orElse byRemovingLeftmostIdentity orElse byRemovingLeftmostInverse orElse byAbsorber /*orElse byInverses*/ orElse byDistributing
    }
  }

  private def matchTrees(base: OperatorTree, target: OperatorTree, deconstruction: Seq[DeconstructionStep]): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else if (base.canonicalForm != target.canonicalForm)
      None
    else target match {
      case target: Leaf =>
        matchTreeWithLeafTarget(base, target, deconstruction)
      case target: UnaryOperatorTree =>
        matchTreeWithUnaryTarget(base, target, deconstruction)
      case target: BinaryOperatorTree =>
        matchTreeWithBinaryTarget(base, target, deconstruction, Nil)
      case _ =>
        None
    }
  }

  private def matchTrees(source: OperatorTree, result: OperatorTree, deconstruction: Seq[DeconstructionStep], direction: Direction): Option[Seq[RearrangementStep[T]]] = {
    matchTrees(
      direction.getSource(source, result),
      direction.getResult(source, result),
      deconstruction)
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
      result <- Step.ElidedStep.ifNecessary(steps, "Rearranged")
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
      result <- Step.ElidedStep.ifNecessary(steps, "Rearranged")
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
