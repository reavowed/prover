package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement

sealed trait Premise {
  def statement: Statement
  def referencedInferenceIds: Set[String]
  def referencedLines: Set[PreviousLineReference]
  def serializedLines: Seq[String]
  def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending]
  def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise]
  def isIncomplete: Boolean
}
object Premise {
  sealed trait SingleLinePremise extends Premise {
    def referencedLine: PreviousLineReference
    override def referencedLines: Set[PreviousLineReference] = Set(referencedLine)
  }

  case class Pending(statement: Statement) extends Premise {
    val `type` = "pending"
    override def serializedLines: Seq[String] = Seq(s"pending ${statement.serialized}")
    override def referencedInferenceIds: Set[String] = Set.empty
    override def referencedLines: Set[PreviousLineReference] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map(path -> this)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
      statement.removeExternalParameters(numberOfParametersToRemove).map(Pending(_))
    }
    override def isIncomplete: Boolean = true
  }
  object Pending {
    def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Pending] = {
      Statement.parser.map(Pending(_))
    }
  }

  case class Given(statement: Statement, referencedLine: PreviousLineReference) extends SingleLinePremise {
    val `type` = "given"
    override def serializedLines: Seq[String] = Seq("given")
    override def referencedInferenceIds: Set[String] = Set.empty
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Premise.Pending] = Map.empty
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
      statement.removeExternalParameters(numberOfParametersToRemove).map(Pending(_))
    }
    override def isIncomplete: Boolean = false
  }

  object Given {
    def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Given] = {
      val provenStatement = stepContext.findProvenStatement(targetStatement).getOrElse(throw new Exception(s"Statement $targetStatement was not given"))
      Parser.constant(Given(provenStatement.statement, provenStatement.reference))
    }
  }

  case class Simplification(statement: Statement, premise: Premise.SingleLinePremise, inference: Inference.Summary, substitutions: Substitutions, path: Seq[Int]) extends SingleLinePremise {
    val `type` = "simplification"
    override def referencedLine: PreviousLineReference = premise.referencedLine.addInternalPath(path)
    override def referencedInferenceIds: Set[String] = premise.referencedInferenceIds + inference.id
    override def serializedLines: Seq[String] = Seq(s"simplified ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++ premise.serializedLines.indent
    override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Pending] = premise.getPendingPremises(path :+ 0)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
      statement.removeExternalParameters(numberOfParametersToRemove).map(Pending(_))
    }
    override def isIncomplete: Boolean = premise.isIncomplete
  }
  object Simplification {
    def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Simplification] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremisesAndValidateConclusion(statement, substitutions, parsingContext.parameterDepth)
        premiseStatement = premiseStatements.single.getOrElse(throw new Exception("Simplification inference must have a single premise"))
        premise <- Premise.parser(premiseStatement)
      } yield {
        if (statement != targetStatement) {
          throw new Exception(s"Statement $statement did not match target $targetStatement")
        }
        val path = premiseStatement.findComponentPath(statement).getOrElse(throw new Exception(s"Could not find $statement in $premiseStatement"))
        val singleLinePremise = premise.asOptionalInstanceOf[SingleLinePremise].getOrElse(throw new Exception("Simplification premise must be single line"))
        Premise.Simplification(statement, singleLinePremise, inference, substitutions, path)
      }
    }
  }

  def parser(statement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Premise] = {
    Parser.selectWordParser("premise") {
      case "pending" =>
        Pending.parser(statement)
      case "given" =>
        Given.parser(statement)
      case "simplified" =>
        Simplification.parser(statement)
    }
  }
}
