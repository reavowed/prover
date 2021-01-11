package net.prover.model.proof

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.core.expressions.Statement
import net.prover.core.substitutions.{SubstitutionApplier, Substitutions}
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.proof.Premise.Pending
import net.prover.model.simplification.ComponentFinder
import net.prover.model.substitutions.{ParameterRemover, SubstitutionCalculator}
import net.prover.model.substitutions.ParameterRemover.ParameterRemovalContext
import net.prover.structure.EntryContext

sealed trait Premise {
  def statement: Statement
  def referencedInferenceIds: Set[String]
  def referencedLines: Set[PreviousLineReference]
  def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending]
  def insertExternalParameters(numberOfParametersToInsert: Int)(implicit stepModificationContext: StepModificationContext): Premise
  def removeExternalParameters(numberOfParametersToRemove: Int)(implicit stepModificationContext: StepModificationContext): Option[Premise] = {
    ParameterRemover.WithContext.removeExternalParameters(statement, numberOfParametersToRemove)(ParameterRemovalContext(stepModificationContext.internalDepth))
      .map(Pending)
  }
  def toPending: Pending = Pending(statement)
  def isComplete: Boolean
}
object Premise {
  def serialize(premises: Seq[Premise]): String = premises.map(serialize).mkString(" ")
  def serialize(premise: Premise): String = {
    def helper(p: Premise, suffix: String): String = p match {
      case Pending(_) => "???"
      case Given(_, ref) => ref.serialize + suffix
      case Simplification(_, inner, inference, _, _) => helper(inner, " " + inference.id + suffix)
    }
    helper(premise, "")
  }

  def parser(statement: Statement)(implicit stepContext: StepContext, entryContext: EntryContext): Parser[Premise] = {
    @scala.annotation.tailrec
    def helper(inferences: Seq[Inference], premise: SingleLinePremise): SingleLinePremise = {
      inferences match {
        case Nil =>
          premise
        case inference +: tailInferences =>
          val inferencePremise = inference.premises.single.getOrElse(throw new Exception("Given simplification inference did not have a single premise"))
          val substitutions = SubstitutionCalculator.calculateSubstitutions(inferencePremise, premise.statement).flatMap(_.confirmTotality(inference.variableDefinitions)).getOrElse(throw new Exception("Could not calculate substitutions for simplification"))
          val result = SubstitutionApplier.applySubstitutions(inference.conclusion, substitutions).getOrElse(throw new Exception("Could not apply substitutions for simplification"))
          val path = ComponentFinder.findComponentPath(inferencePremise, inference.conclusion).getOrElse(throw new Exception("Could not find simplification path"))
          helper(tailInferences, Simplification(result, premise, inference.summary, substitutions, path))
      }
    }
    val pendingParser = Parser.optionalWord("???").mapMap(_ => Premise.Pending(statement))
    val singleLinePremiseParser = for {
      ref <- PreviousLineReference.parser
      baseRef = ref.withoutInternalPath
      givenPremise = stepContext.premises.find(_.referencedLine == baseRef).getOrElse(throw new Exception(s"Could not find reference ${baseRef.serialize}"))
      inferences <- Inference.parser.tryOrNone.whileDefined
      result = helper(inferences, givenPremise)
      _ = if (result.statement != statement) throw new Exception(s"Given premise ${result.statement} did not match expected premise $statement")
    } yield helper(inferences, givenPremise)
    pendingParser orElse singleLinePremiseParser
  }

  sealed trait SingleLinePremise extends Premise {
    @JsonSerialize
    def referencedLine: PreviousLineReference
    override def referencedLines: Set[PreviousLineReference] = Set(referencedLine)
  }

  case class Pending(statement: Statement) extends Premise {
    val `type` = "pending"
    override def referencedInferenceIds: Set[String] = Set.empty
    override def referencedLines: Set[PreviousLineReference] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map(path -> this)
    override def isComplete: Boolean = false
  }

  case class Given(statement: Statement, referencedLine: PreviousLineReference) extends SingleLinePremise {
    val `type` = "given"
    override def referencedInferenceIds: Set[String] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map.empty
    override def isComplete: Boolean = true
  }

  case class Simplification(statement: Statement, premise: Premise.SingleLinePremise, inference: Inference.Summary, substitutions: Substitutions, path: Seq[Int]) extends SingleLinePremise {
    val `type` = "simplification"
    override def referencedLine: PreviousLineReference = premise.referencedLine.addInternalPath(path)
    override def referencedInferenceIds: Set[String] = premise.referencedInferenceIds + inference.id
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Pending] = premise.getPendingPremises(path :+ 0)
    override def isComplete: Boolean = premise.isComplete
  }
}
