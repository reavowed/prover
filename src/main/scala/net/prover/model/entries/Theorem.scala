package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.controllers.Identity
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import scalaz.Functor
import scalaz.syntax.functor._

@JsonIgnoreProperties(Array("rearrangementType"))
case class Theorem(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement,
    proofs: Seq[Proof],
    rearrangementType: RearrangementType)
  extends Inference.Entry
{
  override def withName(newName: String): Theorem = copy(name = newName)
  override def referencedInferenceIds: Set[String] = proofs.flatMap(_.referencedInferenceIds).toSet
  override def referencedEntries: Set[ChapterEntry] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions ++ proofs.flatMap(_.referencedDefinitions).toSet
  override def inferences: Seq[Inference] = Seq(this)

  def isComplete: Boolean = proofs.exists(_.isComplete)
  def initialStepContext(entryContext: EntryContext): StepContext = StepContext.withPremisesAndTerms(premises, requiredSubstitutions.terms, entryContext)

  private def replaceProof(index: Int, newProof: Proof): Theorem = copy(proofs = proofs.updated(index, newProof))

  private def modifyProof[F[_] : Functor](proofIndex: Int, f: Proof => Option[F[Proof]]): Option[F[Theorem]] = {
    proofs.splitAtIndexIfValid(proofIndex).flatMap { case (before, proof, after) =>
      f(proof).map(_.map(newProof => copy(proofs = (before :+ newProof) ++ after)))
    }
  }


  def findStep(proofIndex: Int, stepIndexes: Seq[Int], entryContext: EntryContext): Option[(Step, StepContext)] = {
    proofs.lift(proofIndex).flatMap(_.findStep(stepIndexes, initialStepContext(entryContext)))
  }
  def modifySteps[F[_] : Functor](proofIndex: Int, stepIndexes: Seq[Int], entryContext: EntryContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Theorem]] = {
    modifyProof(proofIndex, _.modifySteps(stepIndexes, initialStepContext(entryContext), f))
  }
  def modifyStep[F[_] : Functor](proofIndex: Int, stepIndexes: Seq[Int], entryContext: EntryContext, f: (Step, StepContext) => F[Step]): Option[F[Theorem]] = {
    modifyProof(proofIndex, _.modifyStep(stepIndexes, initialStepContext(entryContext), f))
  }
  def insertStep(proofIndex: Int, indexes: Seq[Int], newStep: Step, entryContext: EntryContext): Option[Theorem] = {
    indexes match {
      case Nil =>
        None
      case init :+ last =>
        modifySteps[Identity](proofIndex, init, entryContext) { (steps, _) =>
          val (before, after) = steps.splitAt(last)
          Some((before :+ newStep) ++ after)
        }
    }
  }
  def recalculateReferences(entryContext: EntryContext): Theorem = {
    copy(proofs = proofs.map(_.recalculateReferences(initialStepContext(entryContext))))
  }

  def findAssertions(entryContext: EntryContext): Seq[(Step.Assertion, StepContext)] = {
    def forStep(step: Step, context: StepContext): Seq[(Step.Assertion, StepContext)] = {
      step match {
        case assertion: Step.Assertion =>
          Seq((assertion, context))
        case stepWithSubsteps: Step.WithSubsteps =>
          forSteps(stepWithSubsteps.substeps, stepWithSubsteps.specifyStepContext(context))
        case _ =>
          Nil
      }
    }
    def forSteps(steps: Seq[Step], context: StepContext): Seq[(Step.Assertion, StepContext)] = {
      steps.zipWithIndex.flatMap { case (step, index) => forStep(step, context.atIndex(index))}
    }
    proofs.flatMap(proof => forSteps(proof.steps, initialStepContext(entryContext)))
  }

  override def serializedLines: Seq[String] = Seq(s"theorem $name") ++
    rearrangementType.serialized.toSeq ++
    premises.map("premise " + _.serialized) ++
    Seq("conclusion " + conclusion.serialized) ++
    proofs.flatMap(_.serializedLines)

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): Theorem = {
    Theorem(
      name,
      premises.map(_.replaceDefinition(oldDefinition, newDefinition)),
      conclusion.replaceDefinition(oldDefinition, newDefinition),
      proofs.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)),
      rearrangementType)
  }
}

object Theorem extends Inference.EntryParser {
  override val name: String = "theorem"

  case class Proof(steps: Seq[Step]) {
    def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    def referencedDefinitions: Set[ExpressionDefinition] = steps.flatMap(_.referencedDefinitions).toSet
    def isComplete: Boolean = steps.forall(_.isComplete)

    def findStep(indexes: Seq[Int], initialStepContext: StepContext): Option[(Step, StepContext)] = {
      indexes match {
        case Nil =>
          None
        case head +: tail =>
          val initialStepContextAndPathOption = steps.splitAtIndexIfValid(head).map { case (before, step, _) =>
            (step, initialStepContext.addSteps(before).atIndex(head), Seq(head))
          }
          tail.foldLeft(initialStepContextAndPathOption) { case (currentStepContextAndPathOption, index) =>
            currentStepContextAndPathOption.flatMap { case (step, stepContext, path) =>
              step.getSubstep(index, stepContext).map { case (newStep, newStepContext) => (newStep, newStepContext, path :+ index) }
            }
          }.map { case (step, stepContext, _) => (step, stepContext) }
      }
    }
    def modifySteps[F[_] : Functor](indexes: Seq[Int], initialStepContext: StepContext, f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Proof]] = {
      def helper(indexes: Seq[Int], steps: Seq[Step], outerContext: StepContext): Option[F[Seq[Step]]] = {
        indexes match {
          case Nil =>
            f(steps, outerContext)
          case head +: tail =>
            steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
                step.modifySubsteps(outerContext.addSteps(before).atIndex(head), (substeps, innerContext) => helper(tail, substeps, innerContext))
                  .map(_.map(updatedStep => (before :+ updatedStep) ++ after))
            }
        }
      }
      helper(indexes, steps, initialStepContext).map(_.map(Proof))
    }
    def modifyStep[F[_] : Functor](indexes: Seq[Int], initialStepContext: StepContext, f: (Step, StepContext) => F[Step]): Option[F[Proof]] = {
      indexes match {
        case Nil =>
          None
        case init :+ last =>
          modifySteps(init, initialStepContext, (steps, context) => steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            f(step, context.atIndex(last)).map { updatedStep =>
              (before :+ updatedStep) ++ after
            }
          })
      }
    }
    def insertStep(indexes: Seq[Int], newStep: Step, initialStepContext: StepContext): Option[Proof] = {
      indexes match {
        case Nil =>
          None
        case init :+ last =>
          modifySteps[Identity](init, initialStepContext, (steps, _) => {
            val (before, after) = steps.splitAt(last)
            Some((before :+ newStep) ++ after)
          })
      }
    }
    def recalculateReferences(initialStepContext: StepContext): Proof = {
      Proof(steps.recalculateReferences(initialStepContext))
    }

    def findAssertions(initialStepContext: StepContext): Seq[(Step.Assertion, StepContext)] = {
      def forStep(step: Step, context: StepContext): Seq[(Step.Assertion, StepContext)] = {
        step match {
          case assertion: Step.Assertion =>
            Seq((assertion, context))
          case stepWithSubsteps: Step.WithSubsteps =>
            forSteps(stepWithSubsteps.substeps, stepWithSubsteps.specifyStepContext(context))
          case _ =>
            Nil
        }
      }
      def forSteps(steps: Seq[Step], context: StepContext): Seq[(Step.Assertion, StepContext)] = {
        steps.zipWithIndex.flatMap { case (step, index) => forStep(step, context.atIndex(index))}
      }
      forSteps(steps, initialStepContext)
    }

    def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): Proof = {
      Proof(steps.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)))
    }

    def serializedLines: Seq[String] = Seq("{") ++ steps.flatMap(_.serializedLines).indent ++ Seq("}")
  }

  def proofsParser(premises: Seq[Statement], conclusion: Statement)(implicit entryContext: EntryContext): Parser[Seq[Proof]] = {
    val proofParser = for {
      steps <- Step.listParser(
        entryContext,
        StepContext.withPremisesAndTerms(premises, (premises :+ conclusion).map(_.requiredSubstitutions).foldTogether.terms, entryContext)).inBraces
      _ = if (!steps.mapCollect(_.provenStatement).lastOption.contains(conclusion)) throw new Exception(s"Proof of theorem '$name' did not prove $conclusion")
    } yield Proof(steps)

    for {
      first <- proofParser
      rest <- proofParser.tryOrNone.whileDefined
    } yield first +: rest
  }

  override def parser(implicit entryContext: EntryContext): Parser[Theorem] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- premisesParser
      conclusion <- conclusionParser
      proofs <- proofsParser(premises, conclusion)
    } yield {
      Theorem(
        name,
        premises,
        conclusion,
        proofs,
        rearrangementType)
    }
  }
}
