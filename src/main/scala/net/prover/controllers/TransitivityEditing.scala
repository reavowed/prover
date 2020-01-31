package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term, TypedExpression}
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.model.proof.{Step, StepProvingContext, StepReference, SubstitutionContext}
import org.springframework.http.ResponseEntity

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

trait TransitivityEditing extends BookModification {
  protected def withRelation[T](
    statement: Statement,
    forConnective: (BinaryConnective, Statement, Statement) => Try[T],
    forRelation: (BinaryRelation, Term, Term) => Try[T])(
    implicit stepProvingContext: StepProvingContext
  ): Try[T] = {
    stepProvingContext.provingContext.definedBinaryStatements.reverse.mapFind {
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


  trait CreateStepsForTransitivity {
    def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[Statement], Option[Step], BinaryJoiner[Statement], Option[Step], Statement, Seq[Step.Target])]
    def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[Term], Option[Step], BinaryJoiner[Term], Option[Step], Term, Seq[Step.Target])]
  }
  trait CreateStepsForTransitivityCommon extends CreateStepsForTransitivity {
    def createSteps[T <: (Expression with TypedExpression[T]) : TransitivityMethods](targetJoiner: BinaryJoiner[T], targetLhs: T, targetRhs: T, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[T], Option[Step], BinaryJoiner[T], Option[Step], T, Seq[Step.Target])]
    def createStepsForConnective(targetConnective: BinaryConnective, targetLhs: Statement, targetRhs: Statement, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[Statement], Option[Step], BinaryJoiner[Statement], Option[Step], Statement, Seq[Step.Target])] = {
      createSteps(targetConnective, targetLhs, targetRhs, stepProvingContext)
    }
    def createStepsForRelation(targetRelation: BinaryRelation, targetLhs: Term, targetRhs: Term, stepProvingContext: StepProvingContext): Try[(BinaryJoiner[Term], Option[Step], BinaryJoiner[Term], Option[Step], Term, Seq[Step.Target])] = {
      createSteps(targetRelation, targetLhs, targetRhs, stepProvingContext)
    }
  }

  protected def insertTransitivity(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData,
    createSteps: CreateStepsForTransitivity
  ): ResponseEntity[_] = {
    (stepPath.indexes match {
      case Nil =>
        Failure(NotFoundException(s"Step $stepPath"))
      case init :+ last =>
        modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, provingContext) =>
          theorem.modifySteps(proofIndex, init) { (steps, outerStepContext) =>
            steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
              implicit val stepContext = outerStepContext.addSteps(before).atIndex(last)
              implicit val stepProvingContext = StepProvingContext(stepContext, provingContext)

              def forConnective(connective: BinaryConnective, lhs: Statement, rhs: Statement): Try[(Seq[Step], Seq[Step.Target])] = {
                for {
                  (firstJoiner, firstStep, secondJoiner, secondStep, intermediate, targetSteps) <- createSteps.createStepsForConnective(connective, lhs, rhs, stepProvingContext)
                  transitivitySteps <- getTransitivitySteps(firstJoiner, firstStep, secondJoiner, secondStep, lhs, intermediate, rhs, connective)
                } yield (transitivitySteps, targetSteps)
              }
              def forRelation(relation: BinaryRelation, lhs: Term, rhs: Term): Try[(Seq[Step], Seq[Step.Target])] = {
                for {
                  (firstJoiner, firstStep, secondJoiner, secondStep, intermediate, targetSteps) <- createSteps.createStepsForRelation(relation, lhs, rhs, stepProvingContext)
                  transitivitySteps <- getTransitivitySteps(firstJoiner, firstStep, secondJoiner, secondStep, lhs, intermediate, rhs, relation)
                } yield (transitivitySteps, targetSteps)
              }
              def getTransitivitySteps[T <: Expression : TransitivityMethods](
                firstJoiner: BinaryJoiner[T],
                firstStep: Option[Step],
                secondJoiner: BinaryJoiner[T],
                secondStep: Option[Step],
                targetLhs: T,
                intermediate: T,
                targetRhs: T,
                targetJoiner: BinaryJoiner[T]
              ): Try[Seq[Step]] = {
                def getWithFollowingTransitivity = for {
                  (followingStep, remainingSteps) <- after.headAndTailOption
                  followingStatement <- followingStep.provenStatement
                  (followingRelation, followingLhs, followingRhs) <- TransitivityMethods.getRelation(followingStatement)
                  if followingRhs == targetRhs
                  stepPath = outerStepContext.atIndex(last).stepReference.stepPath
                  followingStepPath = outerStepContext.atIndex(last + 1).stepReference.stepPath
                  followingPremises = followingStep.recursivePremises.filter(p => !p.asOptionalInstanceOf[SingleLinePremise].flatMap(_.referencedLine.asOptionalInstanceOf[StepReference]).exists(_.stepPath.startsWith(followingStepPath)))
                  if followingPremises.exists(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath)))
                  otherPremise <- followingPremises.removeWhere(p => p.asOptionalInstanceOf[SingleLinePremise].exists(_.referencedLine == StepReference(stepPath))).single
                  (initialRelation, initialLhs, _) <- TransitivityMethods.getRelation(otherPremise.statement)
                  if (initialLhs == followingLhs)
                } yield {
                  for {
                    (intermediateRelation, firstTransitivityStep) <- TransitivityMethods.getTransitivityStep(initialLhs, targetLhs, intermediate, initialRelation, firstJoiner)
                    (lastRelation, secondTransitivityStep) <- TransitivityMethods.getTransitivityStep(initialLhs, intermediate, targetRhs, intermediateRelation, secondJoiner)
                    if lastRelation == followingRelation
                    stepsForFirst = if (targetLhs == intermediate && initialRelation == intermediateRelation) Nil else firstStep.toSeq :+ firstTransitivityStep
                    stepsForSecond = if (intermediate == targetRhs && intermediateRelation == lastRelation) Nil else secondStep.toSeq :+ secondTransitivityStep
                  } yield stepsForFirst ++ stepsForSecond ++ remainingSteps
                }
                def withNewTransitivity = {
                  if (targetLhs == intermediate && secondJoiner == targetJoiner) {
                    secondStep.map(_ +: after)
                  } else if (intermediate == targetRhs && firstJoiner == targetJoiner) {
                    firstStep.map(_ +: after)
                  } else for {
                    (_, transitivityStep) <- TransitivityMethods.getTransitivityStep(targetLhs, intermediate, targetRhs, firstJoiner, secondJoiner)
                  } yield {
                    (firstStep.toSeq ++ secondStep.toSeq :+ transitivityStep) ++ after
                  }
                }
                getWithFollowingTransitivity getOrElse withNewTransitivity orBadRequest "Could not chain steps"
              }

              for {
                targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest(s"Step was not target")
                (transitivitySteps, targetSteps) <- withRelation(targetStep.statement, forConnective, forRelation)
              } yield insertTargetsBeforeTransitivity(before, transitivitySteps, targetSteps)
            }
          }.orNotFound(s"Step $stepPath").flatten
        }
    }).toResponseEntity
  }
}

sealed trait TransitivityMethods[T <: Expression] {
  def getRelation(statement: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(BinaryJoiner[T], T, T)]
  def getTransitivityStep(
    source: T,
    intermediate: T,
    target: T,
    firstJoiner: BinaryJoiner[T],
    secondJoiner: BinaryJoiner[T])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[(BinaryJoiner[T], Step)] = {
    for {
      transitivity <- provingContext.transitivities.ofType[Transitivity[T]].find(_.statement == firstJoiner)
      if firstJoiner == secondJoiner
    } yield firstJoiner -> transitivity.assertionStep(source, intermediate, target)
  }
  def parser(implicit expressionParsingContext: ExpressionParsingContext): Parser[T]
}
object TransitivityMethods {
  abstract class TransitivityMethodsAux[TExpression <: Expression, TStatement <: BinaryJoiner[TExpression] : ClassTag] extends TransitivityMethods[TExpression] {
    override def getRelation(statement: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(BinaryJoiner[TExpression], TExpression, TExpression)] = {
      provingContext.definedBinaryStatements.ofType[TStatement].reverse.mapFind { relation =>
        for {
          (lhs, rhs) <- relation.unapply(statement)
        } yield (relation, lhs, rhs)
      }
    }
  }

  implicit object ForStatement extends TransitivityMethodsAux[Statement, BinaryConnective] with TransitivityMethods[Statement] {
    override def parser(implicit expressionParsingContext: ExpressionParsingContext): Parser[Statement] = Statement.parser
  }
  implicit object ForTerm extends TransitivityMethodsAux[Term, BinaryRelation] with TransitivityMethods[Term] {
    override def getTransitivityStep(
      source: Term,
      intermediate: Term,
      target: Term,
      firstRelation: BinaryJoiner[Term],
      secondRelation: BinaryJoiner[Term])(
      implicit provingContext: ProvingContext,
      substitutionContext: SubstitutionContext
    ): Option[(BinaryJoiner[Term], Step)] = {
      def bySubstitutionFromFirst = for {
        substitution <- provingContext.substitutions.find(_.relation == firstRelation)
        reversal <- provingContext.reversals.ofType[Reversal[Term]].find(_.relation == firstRelation)
      } yield {
        secondRelation -> Step.Elided.forInference(substitution.inference)(Seq(
          reversal.assertionStep(intermediate, source),
          substitution.assertionStep(intermediate, source, new Wrapper(secondRelation(_, target)(_)))))
      }
      def bySubstitutionFromSecond = for {
        substitution <- provingContext.substitutions.find(_.relation == secondRelation)
      } yield firstRelation -> substitution.assertionStep(intermediate, target, new Wrapper(firstRelation(source, _)(_)))
      super.getTransitivityStep(source, intermediate, target, firstRelation, secondRelation) orElse bySubstitutionFromFirst orElse bySubstitutionFromSecond
    }
    override def parser(implicit expressionParsingContext: ExpressionParsingContext): Parser[Term] = Term.parser
  }

  def getRelation[T <: Expression : TransitivityMethods](statement: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(BinaryJoiner[T], T, T)] = {
    implicitly[TransitivityMethods[T]].getRelation(statement)
  }
  def getTransitivityStep[T <: Expression : TransitivityMethods](
    source: T,
    intermediate: T,
    target: T,
    firstJoiner: BinaryJoiner[T],
    secondJoiner: BinaryJoiner[T])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[(BinaryJoiner[T], Step)] = {
    implicitly[TransitivityMethods[T]].getTransitivityStep(source, intermediate, target, firstJoiner, secondJoiner)
  }
  def parser[T <: Expression : TransitivityMethods](implicit expressionParsingContext: ExpressionParsingContext): Parser[T] = implicitly[TransitivityMethods[T]].parser
}
