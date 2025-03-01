package net.prover.controllers

import net.prover.controllers.models.{InsertionAndMultipleReplacementProps, MultipleStepReplacementProps, PathData}
import net.prover.entries.StepWithContext
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.model.proof.{Step, StepProvingContext, StepReference, SubstitutionContext}
import net.prover.proving.stepReplacement.AddTargetsBeforeChain
import net.prover.proving.structure.statements.{BinaryConnective, BinaryJoiner, BinaryRelation}
import net.prover.theorems.GetReferencedPremises
import net.prover.util.FunctorTypes._
import org.springframework.http.ResponseEntity

import scala.util.{Failure, Try}

trait ChainingStepEditing {
  def bookService: BookService

  protected def withRelation[T](
    statement: Statement,
    forConnective: (BinaryConnective, Statement, Statement) => Try[T],
    forRelation: (BinaryRelation, Term, Term) => Try[T])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[T] = {
    provingContext.definedBinaryJoiners.mapFind {
      case connective: BinaryConnective =>
        for {
          (lhs, rhs) <- connective.unapply(statement)
        } yield forConnective(connective, lhs, rhs)
      case relation: BinaryRelation =>
        for {
          (lhs, rhs) <- relation.unapply(statement)
        } yield forRelation(relation, lhs, rhs)
      case _ =>
        None
    }
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
        (chainingJoiner, chainingStep) <- ChainingMethods.getChainingStep(precedingLhs, lhs, rhs, precedingJoiner, joiner)
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
      val stepOption = Some(Step.TargetStep(joiner(lhs, rhs))).filter(step => !stepProvingContext.allPremises.exists(_.statement == step.statement))
      ChainingStepDefinition(lhs, rhs, joiner, stepOption)
    }
  }

  trait CreateChainingSteps {
    def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Statement], ChainingStepDefinition[Statement], Seq[Step.TargetStep])]
    def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term)(implicit stepWithContext: StepWithContext): Try[(ChainingStepDefinition[Term], ChainingStepDefinition[Term], Seq[Step.TargetStep])]
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
        bookService.replaceSteps[WithValue[InsertionAndMultipleReplacementProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { outerStepsWithContext =>
          outerStepsWithContext.stepsWithContexts.splitAtIndexIfValid(last).map { case (before, stepWithContext, after) =>
            implicit val swc = stepWithContext
            def forConnective(connective: BinaryConnective, lhs: Statement, rhs: Statement): Try[(Seq[Step], Seq[Step.TargetStep], MultipleStepReplacementProps)] = {
              for {
                (firstChainingStep, secondChainingStep, targetSteps) <- createSteps.createStepsForConnective(connective, lhs, rhs)
                (transitivitySteps, replacementProps) <- getTransitivitySteps(firstChainingStep, secondChainingStep, connective)
              } yield (transitivitySteps, targetSteps, replacementProps)
            }
            def forRelation(relation: BinaryRelation, lhs: Term, rhs: Term): Try[(Seq[Step], Seq[Step.TargetStep], MultipleStepReplacementProps)] = {
              for {
                (firstChainingStep, secondChainingStep, targetSteps) <- createSteps.createStepsForRelation(relation, lhs, rhs)
                (transitivitySteps, replacementProps) <- getTransitivitySteps(firstChainingStep, secondChainingStep, relation)
              } yield (transitivitySteps, targetSteps, replacementProps)
            }
            def getTransitivitySteps[T <: Expression : ChainingMethods](
              firstChainingStep: ChainingStepDefinition[T],
              secondChainingStep: ChainingStepDefinition[T],
              targetJoiner: BinaryJoiner[T]
            ): Try[(Seq[Step], MultipleStepReplacementProps)] = {
              def getWithFollowingTransitivity: Option[Option[(Seq[Step], MultipleStepReplacementProps)]] = for {
                (followingStepWithContext, remainingStepWithContexts) <- after.headAndTailOption
                (followingRelation, followingLhs, followingRhs) <- ChainingMethods.getJoiner(followingStepWithContext.step.statement)
                if followingRhs == secondChainingStep.rhs
                stepPath = stepWithContext.stepContext.stepReference.stepPath
                followingStepPath = outerStepsWithContext.atIndex(last + 1).get.stepContext.stepReference.stepPath
                followingPremises = GetReferencedPremises(followingStepWithContext)
                if followingPremises.exists(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath)))
                otherPremise <- followingPremises.filter(p => !p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath))).single
                (initialRelation, initialLhs, _) <- ChainingMethods.getJoiner(otherPremise.statement)
                if (initialLhs == followingLhs)
              } yield {
                for {
                  (intermediateRelation, stepsForFirst) <- firstChainingStep.getChainingSteps(initialLhs, initialRelation)
                  (lastRelation, stepsForSecond) <- secondChainingStep.getChainingSteps(initialLhs, intermediateRelation)
                  if (lastRelation == followingRelation)
                } yield (stepsForFirst ++ stepsForSecond ++ remainingStepWithContexts.map(_.step), MultipleStepReplacementProps(init, before.length, before.length + 2, stepsForFirst ++ stepsForSecond))
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
                newStepsOption.map(s => (s ++ after.map(_.step), MultipleStepReplacementProps(init, before.length, before.length + 1, s)))
              }
              getWithFollowingTransitivity getOrElse withNewTransitivity orBadRequest "Could not chain steps"
            }

            for {
              targetStep <- stepWithContext.step.asOptionalInstanceOf[Step.TargetStep].orBadRequest(s"Step was not target")
              (transitivitySteps, targetSteps, replacementProps) <- withRelation(targetStep.statement, forConnective, forRelation)
              (finalSteps, insertionProps) = AddTargetsBeforeChain(init, before.map(_.step), transitivitySteps, targetSteps)(outerStepsWithContext)
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


