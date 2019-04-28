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
