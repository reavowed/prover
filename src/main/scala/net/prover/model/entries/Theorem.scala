package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.controllers.Identity
import net.prover.controllers.models.StepWithReferenceChange
import net.prover.exceptions.InferenceReplacementException
import net.prover.model._
import net.prover.model.definitions.{Definitions, ExpressionDefinition}
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import scalaz.Functor
import scalaz.syntax.functor._

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

@JsonIgnoreProperties(Array("rearrangementType"))
case class Theorem(
    name: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement,
    proofs: Seq[Proof])
  extends Inference.Entry
{
  override def withName(newName: String): Theorem = copy(name = newName)
  override def referencedInferenceIds: Set[String] = proofs.flatMap(_.referencedInferenceIds).toSet
  override def referencedEntries: Set[ChapterEntry] =  ((premises :+ conclusion).flatMap(_.referencedDefinitions).toSet ++ proofs.flatMap(_.referencedDefinitions).toSet).map(_.associatedChapterEntry)
  override def inferences: Seq[Inference.FromEntry] = Seq(this)

  def isComplete(definitions: Definitions): Boolean = proofs.exists(_.isComplete(definitions))
  def initialStepContext: StepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)

  private def modifyProof[F[_] : Functor](proofIndex: Int, f: Proof => Option[F[Proof]]): Option[F[Theorem]] = {
    proofs.splitAtIndexIfValid(proofIndex).flatMap { case (before, proof, after) =>
      f(proof).map(_.map(newProof => copy(proofs = (before :+ newProof) ++ after)))
    }
  }

  def findStep(proofIndex: Int, stepIndexes: Seq[Int]): Option[(Step, StepContext)] = {
    proofs.lift(proofIndex).flatMap(_.findStep(stepIndexes, initialStepContext))
  }
  def replaceSteps[F[_] : Functor](proofIndex: Int, stepIndexes: Seq[Int])(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Theorem]] = {
    modifyProof(proofIndex, _.modifySteps(stepIndexes, initialStepContext, f))
  }
  def replaceStep[F[_] : Functor](proofIndex: Int, stepIndexes: Seq[Int])(f: (Step, StepContext) => F[Seq[Step]]): Option[F[Theorem]] = {
    modifyProof(proofIndex, _.replaceStep(stepIndexes, initialStepContext, f))
  }
  def recalculateReferences(provingContext: ProvingContext): (Theorem, Seq[Seq[StepWithReferenceChange]]) = {
    proofs.map(_.recalculateReferences(initialStepContext, provingContext, conclusion)).split.mapLeft(newProofs => copy(proofs = newProofs))
  }

  def findSteps[T <: Step : ClassTag]: Seq[(T, StepContext)] = {
    def forStep(step: Step, context: StepContext): Seq[(T, StepContext)] = {
      step match {
        case assertion: T =>
          Seq((assertion, context))
        case stepWithSubsteps: Step.WithSubsteps =>
          forSteps(stepWithSubsteps.substeps, stepWithSubsteps.specifyStepContext(context))
        case _ =>
          Nil
      }
    }
    def forSteps(steps: Seq[Step], context: StepContext): Seq[(T, StepContext)] = {
      steps.zipWithIndex.flatMap { case (step, index) => forStep(step, context.atIndex(index))}
    }
    proofs.flatMap(proof => forSteps(proof.steps, initialStepContext))
  }

  override def serializedLines: Seq[String] = Seq(s"theorem $name") ++
    variableDefinitions.serializedLines ++
    premises.map("premise " + _.serialized) ++
    Seq("conclusion " + conclusion.serialized) ++
    proofs.flatMap(_.serializedLines)

  def replaceInference(
    oldInference: Inference,
    newInference: Inference,
    provingContext: ProvingContext
  ): Try[Theorem] = {
    proofs.zipWithIndex.map { case (proof, index) =>
      proof.replaceInference(oldInference, newInference, StepProvingContext(initialStepContext, provingContext)).recoverWith {
        case InferenceReplacementException.AtStep(message, stepPath) => Failure(InferenceReplacementException.AtTheorem(message, stepPath, index, name))
      }
    }.traverseTry.map(proofs => copy(proofs = proofs))
  }
  def clearInference(inference: Inference): Theorem = {
    copy(proofs = proofs.map(_.clearInference(inference)))
  }
  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): Theorem = {
    Theorem(
      name,
      variableDefinitions,
      premises.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      conclusion.replaceDefinitions(expressionDefinitionReplacements),
      proofs.map(_.replaceDefinitions(expressionDefinitionReplacements, entryContext)))
  }
}

object Theorem extends Inference.EntryParser {
  override val name: String = "theorem"

  case class Proof(steps: Seq[Step]) {
    def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    def referencedDefinitions: Set[ExpressionDefinition] = steps.flatMap(_.referencedDefinitions).toSet
    def isComplete(definitions: Definitions): Boolean = steps.forall(_.isComplete(definitions))

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
      Proof.modifySteps(steps, indexes, initialStepContext)(f).map(_.map(Proof(_)))
    }
    def replaceStep[F[_] : Functor](indexes: Seq[Int], initialStepContext: StepContext, f: (Step, StepContext) => F[Seq[Step]]): Option[F[Proof]] = {
      indexes match {
        case Nil =>
          None
        case init :+ last =>
          modifySteps(init, initialStepContext, (steps, context) => steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            f(step, context.atIndex(last)).map { updatedSteps =>
              before ++ updatedSteps ++ after
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
    def recalculateReferences(initialStepContext: StepContext, provingContext: ProvingContext, expectedConclusion: Statement): (Proof, Seq[StepWithReferenceChange]) = {
      val (newSteps, changedSteps) = steps.recalculateReferences(initialStepContext, provingContext)
      val newStepsWithTarget = if (newSteps.mapCollect(_.provenStatement).lastOption.contains(expectedConclusion)) newSteps else newSteps :+ Step.Target(expectedConclusion)
      (Proof(newStepsWithTarget), changedSteps)
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

    def replaceInference(
      oldInference: Inference,
      newInference: Inference,
      stepProvingContext: StepProvingContext
    ): Try[Proof] = {
      steps.replaceInference(oldInference, newInference, stepProvingContext).map(Proof(_))
    }
    def clearInference(inference: Inference): Proof = {
      Proof(steps.clearInference(inference))
    }
    def replaceDefinitions(
      expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
      entryContext: EntryContext
    ): Proof = {
      Proof(steps.map(_.replaceDefinitions(expressionDefinitionReplacements, entryContext)))
    }

    def serializedLines: Seq[String] = Seq("{") ++ steps.flatMap(_.serializedLines).indent ++ Seq("}")
  }

  object Proof {
    def modifySteps[F[_] : Functor](steps: Seq[Step], indexes: Seq[Int], outerContext: StepContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Seq[Step]]] = {
      indexes match {
        case Nil =>
          f(steps, outerContext)
        case head +: tail =>
          steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
            step.modifySubsteps(outerContext.addSteps(before).atIndex(head)) { (substeps, innerContext) => modifySteps(substeps, tail, innerContext)(f) }
              .map(_.map(updatedStep => (before :+ updatedStep) ++ after))
          }
      }
    }
  }

  def proofsParser(
    theoremName: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement)(
    implicit entryContext: EntryContext
  ): Parser[Seq[Proof]] = {
    val initialStepContext = StepContext.withPremisesAndVariables(premises, variableDefinitions)
    val proofParser = for {
      steps <- Step.listParser(entryContext, initialStepContext).inBraces
      _ = if (!steps.mapCollect(_.provenStatement).lastOption.contains(conclusion)) throw new Exception(s"Proof of theorem '$theoremName' did not prove $conclusion")
    } yield Proof(steps)

    for {
      first <- proofParser
      rest <- proofParser.tryOrNone.whileDefined
    } yield first +: rest
  }

  override def parser(implicit entryContext: EntryContext): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      variableDefinitions <- VariableDefinitions.parser
      expressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
      premises <- premisesParser(expressionParsingContext)
      conclusion <- conclusionParser(expressionParsingContext)
      proofs <- proofsParser(name, variableDefinitions, premises, conclusion)
    } yield Theorem(name, VariableDefinitions.fromStatements(premises :+ conclusion), premises, conclusion, proofs)
  }
}
