package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference._

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  def id: String
  def keyOption: Option[String]
  def name: String
  def premises: Seq[Premise]
  def conclusion: ProvenStatement
  def rearrangementType: RearrangementType
  def allowsRearrangement: Boolean
  def summary: Inference.Summary

  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion.statement)
  }
}

trait EntryInference extends Inference {
  def key: String
  def chapterKey: String
  def bookKey: String

  val id = calculateHash()
  override def keyOption = Some(key)
  override def summary = Inference.EntrySummary(name, id, key, chapterKey, bookKey)
}

case class DefinitionInference(
    nameOfDefinedStatement: String,
    premisesStatements: Seq[Statement],
    conclusionStatement: Statement,
    distinctVariables: DistinctVariables)
  extends Inference
{
  override def id = calculateHash()
  override def premises = premisesStatements.map(DirectPremise)
  override def conclusion = ProvenStatement(conclusionStatement, Conditions(Set.empty, distinctVariables))
  override def name: String = s"Definition of $nameOfDefinedStatement"
  override def summary: Inference.Summary = StubSummary(name)
  override def allowsRearrangement = true
  override def rearrangementType = RearrangementType.NotRearrangement
  override def keyOption = None
}

object Inference {
  sealed trait RearrangementType
  object RearrangementType {
    object NotRearrangement extends RearrangementType
    object Simplification extends RearrangementType
    object Expansion extends RearrangementType

    def parser: Parser[RearrangementType] = {
      Parser.singleWord.map {
        case "simplification" => Some(Simplification)
        case "expansion" => Some(Expansion)
        case _ => None
      }.getOrElse(NotRearrangement)
    }
  }

  def unapply(inference: Inference): Option[(String, Seq[Premise], ProvenStatement)] = {
    Some(inference.name, inference.premises, inference.conclusion)
  }

  def calculateHash(premises: Seq[Premise], conclusion: Statement): String = {
    val serialized = (premises.map(_.serialized) :+ conclusion.serialized).mkString("\n")
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(serialized.getBytes("UTF-8"))
    String.format("%064x", new java.math.BigInteger(1, sha.digest()))
  }

  sealed trait Premise {
    def statements: Seq[Statement]
    def applySubstitutions(substitutions: Substitutions): Option[Premise]
    def html: String
    def serialized: String
  }

  case class DirectPremise(statement: Statement) extends Premise {
    override def statements = Seq(statement)
    override def applySubstitutions(substitutions: Substitutions) = {
      for {
        substitutedStatement <- statement.applySubstitutions(substitutions)
      } yield DirectPremise(substitutedStatement)
    }
    override def html = statement.html
    override def serialized = statement.serialized
  }
  case class DeducedPremise(antecedent: Statement, consequent: Statement) extends Premise {
    override def statements = Seq(antecedent, consequent)
    override def applySubstitutions(substitutions: Substitutions) = {
      for {
        substitutedAntecedent <- antecedent.applySubstitutions(substitutions)
        substitutedConsequent <- consequent.applySubstitutions(substitutions)
      } yield DeducedPremise(substitutedAntecedent, substitutedConsequent)
    }

    override def html = antecedent.html + " ⊢ " + consequent.html
    override def serialized = Seq("proves", antecedent.serialized, consequent.serialized).mkString(" ")
  }

  trait Summary {
    def name: String
    def id: Option[String]
  }
  case class StubSummary(name: String) extends Summary {
    override def id = None
  }
  case class EntrySummary(name: String, inferenceId: String, key: String, chapterKey: String, bookKey: String) extends Summary {
    override def id = Some(inferenceId)
  }
}

trait InferenceParser {
  private def deducedPremiseParser(implicit context: Context): Parser[DeducedPremise] = {
    for {
      antecedent <- Statement.parser
      consequent <- Statement.parser
    } yield {
      DeducedPremise(antecedent, consequent)
    }
  }

  private def directPremiseParser(implicit context: Context): Parser[DirectPremise] = {
    Statement.parser.map(DirectPremise)
  }

  private def premiseParser(implicit context: Context): Parser[Option[Premise]] = {
    Parser.optionalWord("premise")
      .mapFlatMap { _ =>
        Parser.optionalWord("proves")
          .mapFlatMap(_ => deducedPremiseParser)
          .orElse(directPremiseParser)
      }
  }

  def premisesParser(implicit context: Context): Parser[Seq[Premise]] = {
    premiseParser.whileDefined
  }
}
