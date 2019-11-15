package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise.Pending

sealed trait Premise {
  def statement: Statement
  def referencedInferenceIds: Set[String]
  def referencedLines: Set[PreviousLineReference]
  def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending]
  def insertExternalParameters(numberOfParametersToInsert: Int): Premise
  def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
    statement.removeExternalParameters(numberOfParametersToRemove).map(Pending)
  }
  def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Premise
  def toPending: Pending = Pending(statement)
  def isIncomplete: Boolean
}
object Premise {
  def serialize(premises: Seq[Premise]): String = premises.map(serialize).mkString(" ")
  def serialize(premise: Premise): String = {
    def helper(p: Premise, suffix: String): String = p match {
      case Pending(_) => throw new Exception("Cannot serialize pending premise")
      case Given(_, ref) => ref.serialize + suffix
      case Simplification(_, inner, inference, _, _) => helper(inner, " " + inference.id + suffix)
    }
    helper(premise, "")
  }

  def parser(statement: Statement)(implicit stepContext: StepContext, entryContext: EntryContext): Parser[SingleLinePremise] = {
    @scala.annotation.tailrec
    def helper(inferences: Seq[Inference], premise: SingleLinePremise): SingleLinePremise = {
      inferences match {
        case Nil =>
          premise
        case inference +: tailInferences =>
          val inferencePremise = inference.premises.single.getOrElse(throw new Exception("Given simplification inference did not have a single premise"))
          val substitutions = inferencePremise.calculateSubstitutions(premise.statement).flatMap(_.confirmTotality).getOrElse(throw new Exception("Could not calculate substitutions for simplification"))
          val result = inference.conclusion.applySubstitutions(substitutions).getOrElse(throw new Exception("Could not apply substitutions for simplification"))
          val path = inferencePremise.findComponentPath(inference.conclusion).getOrElse(throw new Exception("Could not find simplification path"))
          helper(tailInferences, Simplification(result, premise, inference.summary, substitutions, path))
      }
    }
    for {
      ref <- PreviousLineReference.parser
      baseRef = ref.withoutInternalPath
      givenPremise = stepContext.premises.find(_.referencedLine == baseRef).getOrElse(throw new Exception(s"Could not find reference ${baseRef.serialize}"))
      inferences <- Inference.parser.tryOrNone.whileDefined
      result = helper(inferences, givenPremise)
      _ = if (result.statement != statement) throw new Exception(s"Given premise ${result.statement} did not match expected premise $statement")
    } yield helper(inferences, givenPremise)
  }

  sealed trait SingleLinePremise extends Premise {
    def referencedLine: PreviousLineReference
    override def referencedLines: Set[PreviousLineReference] = Set(referencedLine)
  }

  case class Pending(statement: Statement) extends Premise {
    val `type` = "pending"
    override def referencedInferenceIds: Set[String] = Set.empty
    override def referencedLines: Set[PreviousLineReference] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map(path -> this)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Premise.Pending = {
      copy(statement = statement.insertExternalParameters(numberOfParametersToInsert))
    }
    def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Pending = {
      Pending(statement.replaceDefinition(oldDefinition, newDefinition))
    }
    override def isIncomplete: Boolean = true
  }

  case class Given(statement: Statement, referencedLine: PreviousLineReference) extends SingleLinePremise {
    val `type` = "given"
    override def referencedInferenceIds: Set[String] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map.empty
    override def insertExternalParameters(numberOfParametersToInsert: Int): Premise.Given = {
      copy(statement = statement.insertExternalParameters(numberOfParametersToInsert))
    }
    def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Given = {
      Given(
        statement.replaceDefinition(oldDefinition, newDefinition),
        referencedLine)
    }
    override def isIncomplete: Boolean = false
  }

  case class Simplification(statement: Statement, premise: Premise.SingleLinePremise, inference: Inference.Summary, substitutions: Substitutions, path: Seq[Int]) extends SingleLinePremise {
    val `type` = "simplification"
    override def referencedLine: PreviousLineReference = premise.referencedLine.addInternalPath(path)
    override def referencedInferenceIds: Set[String] = premise.referencedInferenceIds + inference.id
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Pending] = premise.getPendingPremises(path :+ 0)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Premise.Simplification = {
      copy(
        statement = statement.insertExternalParameters(numberOfParametersToInsert),
        substitutions = substitutions.insertExternalParameters(numberOfParametersToInsert))
    }
    def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Simplification = {
      Simplification(
        statement.replaceDefinition(oldDefinition, newDefinition),
        premise.replaceDefinition(oldDefinition, newDefinition).asInstanceOf[Premise.SingleLinePremise],
        inference.replaceDefinition(oldDefinition, newDefinition),
        substitutions.replaceDefinition(oldDefinition, newDefinition),
        path)
    }
    override def isIncomplete: Boolean = premise.isIncomplete
  }
}
