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
  case class RightBinaryDeconstructionStep(operator: RearrangeableOperator, right: OperatorTree) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(_, right.term)(_))
  }
  case class UnaryDeconstructionStep(operator: UnaryOperator) extends DeconstructionStep {
    def addToWrapper(wrapper: Wrapper[Term, T]): Wrapper[Term, T] =  wrapper.insert(operator(_)(_))
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
        def withoutCommuting = for {
          (innerResult, innerSteps) <- recurse(BinaryOperatorTree(
            source.operator,
            BinaryOperatorTree(source.operator, distributor, innerLeft, distributivityDirection),
            innerRight,
            distributivityDirection))
          associativityStep <- source.operator.associativity.rearrangementStep(distributivityDirection.reverseIfNecessary(Seq(distributor, innerLeft, innerRight)), searchDirection, getWrapper(deconstruction), expansion, reversal)
        } yield (innerResult, searchDirection.prepend(associativityStep, innerSteps))
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
        } yield (innerResult, searchDirection.prepend(commutativityStep, searchDirection.prepend(associativityStep, innerSteps)))
        withoutCommuting ++ withCommuting
      case _ =>
        Nil
    }
  }

  private def matchBinaryTreesWithSameOperatorWithoutCommuting(
    operator: RearrangeableOperator,
    base: BinaryOperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    def directly = for {
      leftSteps <- matchTrees(base.left, target.left, deconstruction :+ RightBinaryDeconstructionStep(operator, base.right))
      rightSteps <- matchTrees(base.right, target.right, deconstruction :+ LeftBinaryDeconstructionStep(operator, target.left))
    } yield leftSteps ++ rightSteps
    def byAssociating = for {
      BinaryOperatorTree(`operator`, baseLeftLeft, baseLeftRight) <- base.left.asOptionalInstanceOf[BinaryOperatorTree]
      innerSteps <- matchBinaryTreesWithSameOperatorWithoutCommuting(operator, BinaryOperatorTree(operator, baseLeftLeft, BinaryOperatorTree(operator, baseLeftRight, base.right)), target, deconstruction)
      associativityStep <- operator.associativity.reversedRearrangementStep(baseLeftLeft, baseLeftRight, base.right, getWrapper(deconstruction), expansion, reversal)
    } yield associativityStep +: innerSteps
    def byAssociatingAndCommuting = for {
      BinaryOperatorTree(`operator`, baseLeftLeft, baseLeftRight) <- base.left.asOptionalInstanceOf[BinaryOperatorTree]
      innerSteps <- matchBinaryTreesWithSameOperatorWithoutCommuting(operator, BinaryOperatorTree(operator, baseLeftRight, BinaryOperatorTree(operator, baseLeftLeft, base.right)), target, deconstruction)
      commutativityStep <- base.operator.commutativity.rearrangementStep(baseLeftLeft, baseLeftRight, getWrapper(deconstruction :+ RightBinaryDeconstructionStep(operator, base.right)), expansion)
      associativityStep <- operator.associativity.reversedRearrangementStep(baseLeftRight, baseLeftLeft, base.right, getWrapper(deconstruction), expansion, reversal)
    } yield commutativityStep +: associativityStep +: innerSteps
    def byDistributingInsideBaseLeft = (for {
      baseLeft <- base.left.asOptionalInstanceOf[BinaryOperatorTree].toSeq
      if baseLeft.operator != operator
      (distributedBaseLeft, distributionSteps) <- findDistributivity(baseLeft, deconstruction :+ RightBinaryDeconstructionStep(operator, base.right), operator, Direction.Forward)
      innerSteps <- matchBinaryTreesWithSameOperatorWithoutCommuting(operator, BinaryOperatorTree(operator, distributedBaseLeft, base.right), target, deconstruction)
    } yield distributionSteps ++ innerSteps).headOption
    def byDistributingBoth = (for {
      distributivityOperator <- (provingContext.leftDistributivities ++ provingContext.rightDistributivities).filter(_.distributor == operator).map(_.distributee).distinct
      (distributedBase, baseDistributionSteps) <- findDistributivity(base, deconstruction, distributivityOperator, Direction.Forward)
      (distributedTarget, targetDistributionSteps) <- findDistributivity(target, deconstruction, distributivityOperator, Direction.Reverse)
      innerSteps <- matchBinaryTreesWithSameOperatorWithoutCommuting(distributivityOperator, distributedBase, distributedTarget, deconstruction)
    } yield (baseDistributionSteps ++ innerSteps ++ targetDistributionSteps)).headOption
    directly orElse byAssociating orElse byAssociatingAndCommuting orElse byDistributingInsideBaseLeft orElse byDistributingBoth
  }

  private def matchBinaryTreesWithSameOperator(
    operator: RearrangeableOperator,
    base: BinaryOperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    def withoutCommuting = matchBinaryTreesWithSameOperatorWithoutCommuting(operator, base, target, deconstruction)
    def byCommuting = for {
      innerSteps <- matchBinaryTreesWithSameOperatorWithoutCommuting(operator, BinaryOperatorTree(operator, base.right, base.left), target, deconstruction)
      commutativityStep <- operator.commutativity.rearrangementStep(base.left, base.right, getWrapper(deconstruction), expansion)
    } yield commutativityStep +: innerSteps
    withoutCommuting orElse byCommuting
  }

  private def matchBinaryTreesWithDifferentOperator(
    base: BinaryOperatorTree,
    target: BinaryOperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    def byDistributingBase = (for {
      (distributedBase, distributionSteps) <- findDistributivity(base, deconstruction, target.operator, Direction.Forward)
      innerSteps <- matchBinaryTreesWithSameOperator(target.operator, distributedBase, target, deconstruction)
    } yield distributionSteps ++ innerSteps).headOption
    def byDistributingTarget = (for {
      (distributedTarget, distributionSteps) <- findDistributivity(target, deconstruction, base.operator, Direction.Reverse)
      innerSteps <- matchBinaryTreesWithSameOperator(base.operator, base, distributedTarget, deconstruction)
    } yield innerSteps ++ distributionSteps).headOption
    byDistributingBase orElse byDistributingTarget
  }

  private def matchTreesByTransformingBase(
    base: OperatorTree,
    target: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    if (base == target) {
      Some(Nil)
    } else {
      (base, target) match {
        case (base: BinaryOperatorTree, target: BinaryOperatorTree) if base.operator == target.operator =>
          matchBinaryTreesWithSameOperator(base.operator, base, target, deconstruction)
        case (base: BinaryOperatorTree, target: BinaryOperatorTree) =>
          matchBinaryTreesWithDifferentOperator(base, target, deconstruction)
        case _ =>
          None
      }
    }
  }

  private def simplify(treeToSimplify: OperatorTree, deconstruction: Seq[DeconstructionStep], direction: Direction): Option[(OperatorTree, Seq[RearrangementStep[T]])] = {
    treeToSimplify match {
      case treeToSimplify: BinaryOperatorTree =>
        def leftIdentity = (for {
          leftIdentity <- treeToSimplify.operator.leftIdentities
          (source, result) = direction.swapSourceAndResult(treeToSimplify.left, OperatorTree.parse(leftIdentity.identityTerm))
          stepsToMatchLeft <- matchTrees(source, result, deconstruction :+ RightBinaryDeconstructionStep(treeToSimplify.operator, treeToSimplify.right))
          identityStep <- leftIdentity.rearrangementStep(treeToSimplify.right, direction, getWrapper(deconstruction), expansion, reversal)
        } yield (treeToSimplify.right, direction.append(stepsToMatchLeft, identityStep))).headOption
        def rightIdentity = (for {
          rightIdentity <- treeToSimplify.operator.rightIdentities
          (source, result) = direction.swapSourceAndResult(treeToSimplify.right, OperatorTree.parse(rightIdentity.identityTerm))
          stepsToMatchRight <- matchTrees(source, result, deconstruction :+ LeftBinaryDeconstructionStep(treeToSimplify.operator, treeToSimplify.left))
          identityStep <- rightIdentity.rearrangementStep(treeToSimplify.left, direction, getWrapper(deconstruction), expansion, reversal)
        } yield (treeToSimplify.left, direction.append(stepsToMatchRight, identityStep))).headOption
        def leftAbsorber = (for {
          leftAbsorber <- treeToSimplify.operator.leftAbsorbers
          absorberTree = OperatorTree.parse(leftAbsorber.absorberTerm)
          (source, result) = direction.swapSourceAndResult(treeToSimplify.left, absorberTree)
          stepsToMatchLeft <- matchTrees(source, result, deconstruction :+ RightBinaryDeconstructionStep(treeToSimplify.operator, treeToSimplify.right))
          absorberStep <- leftAbsorber.rearrangementStep(treeToSimplify.right, direction, getWrapper(deconstruction), expansion, reversal)
        } yield (absorberTree, direction.append(stepsToMatchLeft, absorberStep))).headOption
        def rightAbsorber = (for {
          rightAbsorber <- treeToSimplify.operator.rightAbsorbers
          absorberTree = OperatorTree.parse(rightAbsorber.absorberTerm)
          (source, result) = direction.swapSourceAndResult(treeToSimplify.right, absorberTree)
          stepsToMatchRight <- matchTrees(source, result, deconstruction :+ LeftBinaryDeconstructionStep(treeToSimplify.operator, treeToSimplify.left))
          absorberStep <- rightAbsorber.rearrangementStep(treeToSimplify.left, direction, getWrapper(deconstruction), expansion, reversal)
        } yield (absorberTree, direction.append(stepsToMatchRight, absorberStep))).headOption
        leftIdentity orElse rightIdentity orElse leftAbsorber orElse rightAbsorber
      case _ =>
        None
    }
  }

  private def matchTreesBySimplifyingBase(
    base: OperatorTree,
    target: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    matchTreesByTransformingBase(base, target, deconstruction) orElse (for {
      (simplifiedBase, simplificationSteps) <- simplify(base ,deconstruction, Direction.Forward)
      innerSteps <- matchTreesBySimplifyingBase(simplifiedBase, target, deconstruction)
    } yield simplificationSteps ++ innerSteps)
  }

  private def matchTreesBySimplifyingTarget(
    base: OperatorTree,
    target: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    matchTreesBySimplifyingBase(base, target, deconstruction) orElse (for {
      (simplifiedTarget, simplificationSteps) <- simplify(target ,deconstruction, Direction.Reverse)
      innerSteps <- matchTrees(base, simplifiedTarget, deconstruction)
    } yield simplificationSteps ++ innerSteps)
  }

  private def matchTreesByTransformingTarget(
    base: OperatorTree,
    target: OperatorTree,
    deconstruction: Seq[DeconstructionStep]
  ): Option[Seq[RearrangementStep[T]]] = {
    matchTreesBySimplifyingTarget(base, target, deconstruction) orElse (target match {
      case target: BinaryOperatorTree =>
        def byAssociating = for {
          BinaryOperatorTree(target.operator, targetLeftLeft, targetLeftRight) <- target.left.asOptionalInstanceOf[BinaryOperatorTree]
          innerSteps <- matchTreesByTransformingTarget(base, BinaryOperatorTree(target.operator, targetLeftLeft, BinaryOperatorTree(target.operator, targetLeftRight, target.right)), deconstruction)
          associativityStep <- target.operator.associativity.rearrangementStep(targetLeftLeft, targetLeftRight, target.right, getWrapper(deconstruction), expansion)
        } yield innerSteps :+ associativityStep
        def byDistributingOverTargetLeft = for {
          targetLeft <- target.left.asOptionalInstanceOf[BinaryOperatorTree]
          distributivity <- provingContext.rightDistributivities.find(d => d.distributor == target.operator && d.distributee == targetLeft.operator)
          innerSteps <- matchTreesByTransformingTarget(base, BinaryOperatorTree(targetLeft.operator, BinaryOperatorTree(target.operator, targetLeft.left, target.right), BinaryOperatorTree(target.operator, targetLeft.right, target.right)), deconstruction)
          distributivityStep <- distributivity.reversedRearrangementStep(targetLeft.left, targetLeft.right, target.right, getWrapper(deconstruction), expansion, reversal)
        } yield innerSteps :+ distributivityStep
        def byDistributingInsideTargetLeft = (for {
          targetLeft <- target.left.asOptionalInstanceOf[BinaryOperatorTree].toSeq
          (distributedTargetLeft, distributionSteps) <- findDistributivity(targetLeft, deconstruction :+ RightBinaryDeconstructionStep(target.operator, target.right), target.operator, Direction.Reverse)
          innerSteps <- matchTreesByTransformingTarget(base, BinaryOperatorTree(target.operator, distributedTargetLeft, target.right), deconstruction)
        } yield innerSteps ++ distributionSteps).headOption
        byAssociating orElse byDistributingOverTargetLeft orElse byDistributingInsideTargetLeft
      case _ =>
        None
    })
  }

  private def matchTrees(base: OperatorTree, target: OperatorTree, deconstruction: Seq[DeconstructionStep]): Option[Seq[RearrangementStep[T]]] = {
    if (base == target)
      Some(Nil)
    else if (base.canonicalForm != target.canonicalForm)
      None
    else
      matchTreesByTransformingTarget(base, target, deconstruction)
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
