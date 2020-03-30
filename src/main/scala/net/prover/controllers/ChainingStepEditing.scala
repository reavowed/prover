package net.prover.controllers

import net.prover.controllers.models.{InsertionAndMultipleReplacementProps, MultipleStepReplacementProps, PathData, StepInsertionProps}
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term, TypedExpression}
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.model.proof.{Step, StepProvingContext, StepReference, SubstitutionContext}
import org.springframework.http.ResponseEntity

import scala.util.{Failure, Try}

trait ChainingStepEditing extends BookModification {
  protected def withRelation[T](
    statement: Statement,
    forConnective: (BinaryConnective, Statement, Statement) => Try[T],
    forRelation: (BinaryRelation, Term, Term) => Try[T])(
    implicit stepProvingContext: StepProvingContext
  ): Try[T] = {
    stepProvingContext.provingContext.definedBinaryJoiners.mapFind {
      case connective: BinaryConnective =>
        for {
          (lhs, rhs) <- connective.unapply(statement)(stepProvingContext.stepContext)
        } yield () => forConnective(connective, lhs, rhs)
      case relation: BinaryRelation =>
        for {
          (lhs, rhs) <- relation.unapply(statement)(stepProvingContext.stepContext)
        } yield () => forRelation(relation, lhs, rhs)
      case _ =>
        None
    }.map(_())
      .orBadRequest("Target step is not a relation").flatten
  }

  case class ChainingStepDefinition[TComponent <: Expression : ChainingMethods](
      lhs: TComponent,
      rhs: TComponent,
      joiner: BinaryJoiner[TComponent],
      step: Option[Step]) {
    def getChainingSteps(
      precedingLhs: TComponent,
      precedingJoiner: BinaryJoiner[TComponent])(
      implicit provingContext: ProvingContext,
      substitutionContext: SubstitutionContext
    ): Option[(BinaryJoiner[TComponent], Seq[Step])] = {
      for {
        (chainingJoiner, chainingStep) <- ChainingMethods.getTransitivityStep(precedingLhs, lhs, rhs, precedingJoiner, joiner)
      } yield {
        if (lhs == rhs && chainingJoiner == precedingJoiner)
          (joiner, Nil)
        else
          (chainingJoiner, step.toSeq :+ chainingStep)
      }
    }
  }
  object ChainingStepDefinition {
    def forTarget[T <: Expression : ChainingMethods](lhs: T, rhs: T, joiner: BinaryJoiner[T])(implicit stepProvingContext: StepProvingContext): ChainingStepDefinition[T] = {
      val stepOption = Some(Step.Target(joiner(lhs, rhs))).filter(step => !stepProvingContext.allPremises.exists(_.statement == step.statement))
      ChainingStepDefinition(lhs, rhs, joiner, stepOption)
    }
  }

  trait CreateChainingSteps {
    def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.Target])]
    def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])]
  }
  trait CreateChainingStepsCommon extends CreateChainingSteps {
    def createSteps[T <: Expression with TypedExpression[T] : ChainingMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[T], ChainingStepDefinition[T], Seq[Step.Target])]
    def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.Target])] = {
      createSteps(targetConnective, targetLhs, targetRhs, stepProvingContext)
    }
    def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.Target])] = {
      createSteps(targetRelation, targetLhs, targetRhs, stepProvingContext)
    }
  }

  protected def insertTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    createSteps: CreateChainingSteps
  ): ResponseEntity[_] = {
    (stepPath.indexes match {
      case Nil =>
        Failure(NotFoundException(s"Step $stepPath"))
      case init :+ last =>
        bookService.replaceSteps[WithValue[InsertionAndMultipleReplacementProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { (steps, outerStepProvingContext) =>
          steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            val outerStepContext = outerStepProvingContext.stepContext
            implicit val stepContext = outerStepContext.addSteps(before).atIndex(last)
            implicit val stepProvingContext = StepProvingContext(stepContext, outerStepProvingContext.provingContext)

            def forConnective(connective: BinaryConnective, lhs: Statement, rhs: Statement): Try[(Seq[Step], Seq[Step.Target], MultipleStepReplacementProps)] = {
              for {
                (firstChainingStep, secondChainingStep, targetSteps) <- createSteps.createStepsForConnective(connective, lhs, rhs, stepProvingContext)
                (transitivitySteps, replacementProps) <- getTransitivitySteps(firstChainingStep, secondChainingStep, connective)
              } yield (transitivitySteps, targetSteps, replacementProps)
            }
            def forRelation(relation: BinaryRelation, lhs: Term, rhs: Term): Try[(Seq[Step], Seq[Step.Target], MultipleStepReplacementProps)] = {
              for {
                (firstChainingStep, secondChainingStep, targetSteps) <- createSteps.createStepsForRelation(relation, lhs, rhs, stepProvingContext)
                (transitivitySteps, replacementProps) <- getTransitivitySteps(firstChainingStep, secondChainingStep, relation)
              } yield (transitivitySteps, targetSteps, replacementProps)
            }
            def getTransitivitySteps[T <: Expression : ChainingMethods](
              firstChainingStep: ChainingStepDefinition[T],
              secondChainingStep: ChainingStepDefinition[T],
              targetJoiner: BinaryJoiner[T]
            ): Try[(Seq[Step], MultipleStepReplacementProps)] = {
              def getWithFollowingTransitivity: Option[Option[(Seq[Step], MultipleStepReplacementProps)]] = for {
                (followingStep, remainingSteps) <- after.headAndTailOption
                followingStatement <- followingStep.provenStatement
                (followingRelation, followingLhs, followingRhs) <- ChainingMethods.getRelation(followingStatement)
                if followingRhs == secondChainingStep.rhs
                stepPath = stepContext.stepReference.stepPath
                followingStepPath = outerStepContext.atIndex(last + 1).stepReference.stepPath
                followingPremises = followingStep.recursivePremises.filter(p => !p.asOptionalInstanceOf[SingleLinePremise].flatMap(_.referencedLine.asOptionalInstanceOf[StepReference]).exists(_.stepPath.startsWith(followingStepPath)))
                if followingPremises.exists(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath)))
                otherPremise <- followingPremises.removeWhere(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath))).single
                (initialRelation, initialLhs, _) <- ChainingMethods.getRelation(otherPremise.statement)
                if (initialLhs == followingLhs)
              } yield {
                for {
                  (intermediateRelation, stepsForFirst) <- firstChainingStep.getChainingSteps(initialLhs, initialRelation)
                  (lastRelation, stepsForSecond) <- secondChainingStep.getChainingSteps(initialLhs, intermediateRelation)
                  if (lastRelation == followingRelation)
                } yield (stepsForFirst ++ stepsForSecond ++ remainingSteps, MultipleStepReplacementProps(init, before.length, before.length + 2, stepsForFirst ++ stepsForSecond))
              }
              def withNewTransitivity = {
                val newStepsOption = if (secondChainingStep.lhs == firstChainingStep.lhs && secondChainingStep.joiner == targetJoiner) {
                  secondChainingStep.step.map(Seq(_))
                } else if (firstChainingStep.rhs == secondChainingStep.rhs && firstChainingStep.joiner == targetJoiner) {
                  firstChainingStep.step.map(Seq(_))
                } else for {
                  (joiner, secondSteps) <- secondChainingStep.getChainingSteps(firstChainingStep.lhs, firstChainingStep.joiner)
                  if (joiner == targetJoiner)
                } yield {
                  firstChainingStep.step.toSeq ++ secondSteps
                }
                newStepsOption.map(s => (s ++ after, MultipleStepReplacementProps(init, before.length, before.length + 1, s)))
              }
              getWithFollowingTransitivity getOrElse withNewTransitivity orBadRequest "Could not chain steps"
            }

            for {
              targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest(s"Step was not target")
              (transitivitySteps, targetSteps, replacementProps) <- withRelation(targetStep.statement, forConnective, forRelation)
              (finalSteps, insertionProps) = insertTargetsBeforeTransitivity(init, before, transitivitySteps, targetSteps)
            } yield (finalSteps, InsertionAndMultipleReplacementProps(insertionProps, replacementProps))
          }.orNotFound(s"Step $stepPath").flatten
        }.map { case (proofUpdateProps, stepUpdateProps) =>
          proofUpdateProps.withNewStepUpdateProps(InsertionAndMultipleReplacementProps(
            stepUpdateProps.insertion.updateStepsFrom(proofUpdateProps.stepUpdates),
            stepUpdateProps.replacement.updateStepsFrom(proofUpdateProps.stepUpdates)))
        }
    }).toResponseEntity
  }
}


