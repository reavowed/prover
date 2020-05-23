package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{RearrangeableOperator, _}
import net.prover.model.expressions._
import net.prover.util.Direction

case class TermRearranger[T <: Expression](
    equality: Equality,
    expansion: Expansion[T],
    reversal: Reversal[T])(
    implicit stepProvingContext: StepProvingContext)
{
  import stepProvingContext._

  private def addRight(wrapper: Wrapper[Term, T], operator: RearrangeableOperator, rhs: Term): Wrapper[Term, T] = {
    wrapper.insert(operator(_, rhs)(_))
  }
  private def addLeft(wrapper: Wrapper[Term, T], operator: RearrangeableOperator, lhs: Term): Wrapper[Term, T] = {
    wrapper.insert(operator(lhs, _)(_))
  }
  private def addUnary(wrapper: Wrapper[Term, T], operator: UnaryOperator): Wrapper[Term, T] = {
    wrapper.insert(operator(_)(_))
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

  def replaceTreeOnOneSide[TMetadata](
    lhs: Term,
    rhs: Term,
    wrapper: Wrapper[Term, T],
    extendWrapper: TMetadata => Wrapper[Term, T],
    recurse: (Term, Term) => Option[(Seq[RearrangementStep[T]], TMetadata)],
    replaceTree: (Term, (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) => Option[(Term, RearrangementOperation, Seq[Term], Seq[RearrangementStep[T]])]
  ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {
    def withDirection(direction: Direction): Option[(Seq[RearrangementStep[T]], TMetadata)] = for {
      (replaced, rearrangementOperation, terms, replacementSteps) <- replaceTree(direction.getSource(lhs, rhs), matchTrees)
      (innerSteps, metadata) <- recurse.tupled(direction.swapSourceAndResult(replaced, direction.getResult(lhs, rhs)))
      rearrangementStep <- direction.rearrangementStep(rearrangementOperation, terms, wrapper, extendWrapper(metadata))
    } yield (direction.concat(direction.append(replacementSteps, rearrangementStep), innerSteps), metadata)
    withDirection(Direction.Forward) orElse withDirection(Direction.Reverse)
  }

  private def extractUnaryOperator(term: Term, unaryOperator: UnaryOperator, wrapper: Wrapper[Term, T], direction: Direction): Iterator[(Term, Seq[RearrangementStep[T]])] = {
    def direct = for {
      inner <- unaryOperator.unapply(term)
    } yield (inner, Nil)
    def leftRecurse = for {
      (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
      extraction <- provingContext.leftOperatorExtractions.find(e => e.unaryOperator == unaryOperator && e.binaryOperator == operator.operator).iterator
      (innerResult, innerRearrangement) <- extractUnaryOperator(left, unaryOperator, addRight(wrapper, operator, right), direction)
      extractionStep <- direction.rearrangementStep(extraction, Seq(innerResult, right), wrapper, wrapper)
    } yield (operator(innerResult, right), direction.append(innerRearrangement, extractionStep))
    def rightRecurse = for {
      (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
      extraction <- provingContext.rightOperatorExtractions.find(e => e.unaryOperator == unaryOperator && e.binaryOperator == operator.operator).iterator
      (innerResult, innerRearrangement) <- extractUnaryOperator(right, unaryOperator, addLeft(wrapper, operator, left), direction)
      extractionStep <- direction.rearrangementStep(extraction, Seq(left, innerResult), wrapper, wrapper)
    } yield (operator(left, innerResult), direction.append(innerRearrangement, extractionStep))
    direct.iterator ++ leftRecurse ++ rightRecurse
  }

  def applyTransformToOneSide[TMetadata](
    lhs: Term,
    rhs: Term,
    wrapper: Wrapper[Term, T],
    extendWrapper: TMetadata => Wrapper[Term, T],
    recurse: (Term, Term) => Option[(Seq[RearrangementStep[T]], TMetadata)]
  ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {

    def applyTransform(
      replaceTree: (Term, (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) => Option[(Term, RearrangementOperation, Seq[Term], Seq[RearrangementStep[T]])]
    ): Option[(Seq[RearrangementStep[T]], TMetadata)] = {
      replaceTreeOnOneSide(lhs, rhs, wrapper, extendWrapper, recurse, replaceTree)
    }

    def byDistributing = {
      def getDistributivity(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftDistributivity = for {
          (outerOperator, left, RearrangeableOperator(innerOperator, rightLeft, rightRight)) <- RearrangeableOperator.unapply(term)
          leftDistributivity <- provingContext.leftDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
        } yield (innerOperator(outerOperator(left, rightLeft), outerOperator(left, rightRight)), leftDistributivity, Seq(left, rightLeft, rightRight), Nil)
        def rightDistributivity = for {
          (outerOperator, RearrangeableOperator(innerOperator, leftLeft, leftRight), right) <- RearrangeableOperator.unapply(term)
          leftDistributivity <- provingContext.rightDistributivities.find(d => d.distributor == outerOperator && d.distributee == innerOperator)
        } yield (innerOperator(outerOperator(leftLeft, right), outerOperator(leftRight, right)), leftDistributivity, Seq(leftLeft, leftRight, right), Nil)
        leftDistributivity orElse rightDistributivity
      }
      applyTransform(getDistributivity)
    }

    def byIdentity = {
      def getIdentity(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftIdentity = (for {
          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
          leftIdentity <- operator.leftIdentities.iterator
          matchingSteps <- matchTrees(left, leftIdentity.identityTerm, addRight(wrapper, operator, right))
        } yield (right, leftIdentity, Seq(right), matchingSteps)).headOption
        def rightIdentity = (for {
          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
          rightIdentity <- operator.rightIdentities.iterator
          matchingSteps <- matchTrees(right, rightIdentity.identityTerm, addLeft(wrapper, operator, left))
        } yield (left, rightIdentity, Seq(left), matchingSteps)).headOption
        leftIdentity orElse rightIdentity
      }
      applyTransform(getIdentity)
    }

    def byAbsorber = {
      def getAbsorber(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftAbsorber = (for {
          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
          leftAbsorber <- operator.leftAbsorbers.iterator
          matchingSteps <- matchTrees(left, leftAbsorber.absorberTerm, addRight(wrapper, operator, right))
        } yield (leftAbsorber.absorberTerm, leftAbsorber, Seq(right), matchingSteps)).headOption
        def rightAbsorber = (for {
          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
          rightAbsorber <- operator.rightAbsorbers.iterator
          matchingSteps <- matchTrees(right, rightAbsorber.absorberTerm, addLeft(wrapper, operator, left))
        } yield (rightAbsorber.absorberTerm, rightAbsorber, Seq(left), matchingSteps)).headOption
        leftAbsorber orElse rightAbsorber
      }
      applyTransform(getAbsorber)
    }

    def byInverse = {
      def getInverse(term: Term, matchTrees: (Term, Term, Wrapper[Term, T]) => Option[Seq[RearrangementStep[T]]]) = {
        def leftInverse = (for {
          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
          inverse <- operator.inverses.iterator
          leftInverse = inverse.leftInverse
          matchingSteps <- matchTrees(left, inverse.inverseOperator(right), addRight(wrapper, operator, right))
        } yield (leftInverse.identityTerm, leftInverse, Seq(right), matchingSteps)).headOption
        def rightInverse = (for {
          (operator, left, right) <- RearrangeableOperator.unapply(term).iterator
          inverse <- operator.inverses.iterator
          rightInverse = inverse.rightInverse
          matchingSteps <- matchTrees(right, inverse.inverseOperator(left), addLeft(wrapper, operator, left))
        } yield (rightInverse.identityTerm, rightInverse, Seq(left), matchingSteps)).headOption
        leftInverse orElse rightInverse
      }
      applyTransform(getInverse)
    }

    byDistributing orElse byIdentity orElse byAbsorber orElse byInverse
  }

  // Attempts to rearrange baseTerm such that it is an instance of an expectedResultOperator expression whose LHS is targetLeft
  private def pullLeft(baseTerm: Term, targetLeft: Term, expectedResultOperator: RearrangeableOperator, wrapper: Wrapper[Term, T]): Option[(Seq[RearrangementStep[T]], Term)] = {
    def matchingLeft = for {
      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
      steps <- matchTrees(currentLeft, targetLeft, addRight(wrapper, expectedResultOperator, currentRight))
    } yield (steps, currentRight)

    def matchingRight = for {
      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
      steps <- matchTrees(currentRight, targetLeft, addRight(wrapper, expectedResultOperator, currentLeft))
      commutativityStep <- expectedResultOperator.commutativity.rearrangementStep(currentLeft, currentRight, wrapper, expansion)
    } yield (commutativityStep +: steps, currentLeft)

    def insideLeft = for {
      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
      (steps, remainingRight) <- pullLeft(currentLeft, targetLeft, expectedResultOperator, addRight(wrapper, expectedResultOperator, currentRight))
      associativityStep <- expectedResultOperator.associativity.reversedRearrangementStep(targetLeft, remainingRight, currentRight, wrapper, expansion, reversal)
    } yield (steps :+ associativityStep, expectedResultOperator(remainingRight, currentRight))

    def insideRight = for {
      (`expectedResultOperator`, currentLeft, currentRight) <- RearrangeableOperator.unapply(baseTerm)
      (steps, remainingRight) <- pullLeft(currentRight, targetLeft, expectedResultOperator, addRight(wrapper, expectedResultOperator, currentLeft))
      commutativityStep <- expectedResultOperator.commutativity.rearrangementStep(currentLeft, currentRight, wrapper, expansion)
      associativityStep <- expectedResultOperator.associativity.reversedRearrangementStep(targetLeft, remainingRight, currentLeft, wrapper, expansion, reversal)
    } yield ((commutativityStep +: steps) :+ associativityStep, expectedResultOperator(remainingRight, currentLeft))

    def byRecursingInTarget = for {
      (`expectedResultOperator`, _, _) <- RearrangeableOperator.unapply(baseTerm)
      (`expectedResultOperator`, targetLeftLeft, targetLeftRight) <- RearrangeableOperator.unapply(targetLeft)
      (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(baseTerm, targetLeftLeft, expectedResultOperator, wrapper)
      (stepsForLeftRight, treeWithoutLeft) <- pullLeft(treeWithoutLeftLeft, targetLeftRight, expectedResultOperator, addLeft(wrapper, expectedResultOperator, targetLeftLeft))
      associativityStep <- expectedResultOperator.associativity.rearrangementStep(targetLeftLeft, targetLeftRight, treeWithoutLeft, wrapper, expansion)
    } yield (stepsForLeftLeft ++ stepsForLeftRight :+ associativityStep, treeWithoutLeft)

    def byApplyingTransform = applyTransformToOneSide[Term](
      baseTerm,
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


  private def matchTrees(lhs: Term, rhs: Term, wrapper: Wrapper[Term, T]): Option[Seq[RearrangementStep[T]]] = {
    def extendWrapper: Unit => Wrapper[Term, T] = _ => wrapper
    def recurse: (Term, Term) => Option[(Seq[RearrangementStep[T]], Unit)] = matchTrees(_, _, wrapper).map(_ -> ())

    def directly = if (lhs == rhs) Some(Nil) else None

    def byPullingBinaryOperationLeft = for {
      (lhsOperator, _, _) <- RearrangeableOperator.unapply(lhs)
      (rhsOperator, rhsLeft, rhsRight) <- RearrangeableOperator.unapply(rhs)
      if lhsOperator == rhsOperator
      (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, lhsOperator, wrapper)
      stepsToMatchRight <- matchTrees(
        lhsRight,
        rhsRight,
        addLeft(wrapper, lhsOperator, rhsLeft))
    } yield stepsToPullLeft ++ stepsToMatchRight

    def byApplyingTransform = applyTransformToOneSide[Unit](lhs, rhs, wrapper, extendWrapper, recurse).map(_._1)

    def byExtractingUnaryOperator = {
      def left = (for {
        (unaryOperator, leftInner) <- UnaryOperator.unapply(lhs).toSeq
        (rightInner, steps) <- extractUnaryOperator(rhs, unaryOperator, wrapper, Direction.Reverse)
        innerMatch <- matchTrees(leftInner, rightInner, addUnary(wrapper, unaryOperator))
      } yield innerMatch ++ steps).headOption
      def right = (for {
        (unaryOperator, rightInner) <- UnaryOperator.unapply(rhs).toSeq
        (leftInner, steps) <- extractUnaryOperator(lhs, unaryOperator, wrapper, Direction.Forward)
        innerMatch <- matchTrees(leftInner, rightInner, addUnary(wrapper, unaryOperator))
      } yield steps ++ innerMatch).headOption
      left orElse right
    }

    directly orElse byPullingBinaryOperationLeft orElse byApplyingTransform orElse byExtractingUnaryOperator
  }

  def rearrange(baseLhs: Term, baseRhs: Term, wrapper: Wrapper[Term, T]): Option[Seq[RearrangementStep[T]]] = {
    def rearrangeDirectly: Option[Seq[RearrangementStep[T]]] = matchTrees(baseLhs, baseRhs, wrapper)

    def rearrangeUsingPremise(premiseLhs: Term, premiseRhs: Term): Option[Seq[RearrangementStep[T]]] = {
      (for {
        lhsMatch <- matchTrees(baseLhs, premiseLhs, wrapper)
        rhsMatch <- matchTrees(premiseRhs, baseRhs, wrapper)
        joiner = RearrangementStep(wrapper(premiseRhs), Nil, _ => None)
      } yield (lhsMatch :+ joiner) ++ rhsMatch) orElse
        (for {
          firstMatch <- matchTrees(baseLhs, premiseRhs, wrapper)
          secondMatch <- matchTrees(premiseLhs, baseRhs, wrapper)
          joiner = equality.reversalRearrangementStep(premiseRhs, premiseLhs, wrapper, expansion)
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
