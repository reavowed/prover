package net.prover.model.proof

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise.Pending

sealed trait Premise {
  def statement: Statement
  def referencedInferences: Set[Inference]
  def referencedLines: Option[PreviousLineReference]
  def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending]
  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Premise
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Premise] = {
    statement.removeExternalParameters(numberOfParametersToRemove, internalDepth).map(Pending)
  }
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Premise
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

  def parser(statement: Statement)(implicit stepContext: StepContext): Parser[Premise] = {
    @scala.annotation.tailrec
    def helper(inferences: Seq[Inference], premise: SingleLinePremise): SingleLinePremise = {
      inferences match {
        case Nil =>
          premise
        case inference +: tailInferences =>
          val inferencePremise = inference.premises.single.getOrElse(throw new Exception("Given simplification inference did not have a single premise"))
          val substitutions = inferencePremise.calculateSubstitutions(premise.statement).flatMap(_.confirmTotality(inference.variableDefinitions)).getOrElse(throw new Exception("Could not calculate substitutions for simplification"))
          val result = inference.conclusion.applySubstitutions(substitutions).getOrElse(throw new Exception("Could not apply substitutions for simplification"))
          val path = inferencePremise.findComponentPath(inference.conclusion).getOrElse(throw new Exception("Could not find simplification path"))
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
    override def referencedLines: Option[PreviousLineReference] = Some(referencedLine)
  }

  case class Pending(statement: Statement) extends Premise {
    val `type` = "pending"
    override def referencedInferences: Set[Inference] = Set.empty
    override def referencedLines: Option[PreviousLineReference] = None
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map(path -> this)
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Premise.Pending = {
      copy(statement = statement.insertExternalParameters(numberOfParametersToInsert, internalDepth))
    }
    def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Pending = {
      Pending(statement.replaceDefinitions(expressionDefinitionReplacements))
    }
    override def isComplete: Boolean = false
  }

  case class Given(statement: Statement, referencedLine: PreviousLineReference) extends SingleLinePremise {
    val `type` = "given"
    override def referencedInferences: Set[Inference] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map.empty
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Premise.Given = {
      copy(statement = statement.insertExternalParameters(numberOfParametersToInsert, internalDepth))
    }
    def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Given = {
      Given(
        statement.replaceDefinitions(expressionDefinitionReplacements),
        referencedLine)
    }
    override def isComplete: Boolean = true
  }

  case class Simplification(statement: Statement, premise: Premise.SingleLinePremise, inference: Inference.Summary, substitutions: Substitutions, path: Seq[Int]) extends SingleLinePremise {
    val `type` = "simplification"
    override def referencedLine: PreviousLineReference = premise.referencedLine.addInternalPath(path)
    override def referencedInferences: Set[Inference] = premise.referencedInferences + inference
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Pending] = premise.getPendingPremises(path :+ 0)
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Premise.Simplification = {
      copy(
        statement = statement.insertExternalParameters(numberOfParametersToInsert, internalDepth),
        substitutions = substitutions.insertExternalParameters(numberOfParametersToInsert, internalDepth))
    }
    def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Simplification = {
      Simplification(
        statement.replaceDefinitions(expressionDefinitionReplacements),
        premise.replaceDefinitions(expressionDefinitionReplacements).asInstanceOf[Premise.SingleLinePremise],
        inference.replaceDefinitions(expressionDefinitionReplacements),
        substitutions.replaceDefinitions(expressionDefinitionReplacements),
        path)
    }
    override def isComplete: Boolean = premise.isComplete
  }
}
