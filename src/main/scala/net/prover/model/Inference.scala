package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference._

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  val id: String = calculateHash()
  def keyOption: Option[String]
  def name: String
  def premises: Seq[Premise]
  def conclusion: ProvenStatement
  def rearrangementType: RearrangementType
  def allowsRearrangement: Boolean
  def summary: Inference.Summary

  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion)
  }
}

trait EntryInference extends Inference {
  def key: String
  def chapterKey: String
  def bookKey: String

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
  override def premises = premisesStatements.map(DirectPremise.apply)
  override def conclusion = ProvenStatement(conclusionStatement, Conditions(Set.empty, distinctVariables))
  override def name: String = s"Definition of $nameOfDefinedStatement"
  override def summary: Inference.Summary = StubSummary(name)
  override def allowsRearrangement = true
  override def rearrangementType = RearrangementType.NotRearrangement
  override def keyOption = None
}

case class TransformedInference(baseInference: Inference, premises: Seq[Premise], conclusion: ProvenStatement) extends Inference {
  override def name: String = baseInference.name
  override def summary: Summary = baseInference.summary
  override def allowsRearrangement: Boolean = baseInference.allowsRearrangement
  override def rearrangementType: RearrangementType = baseInference.rearrangementType
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

  def calculateHash(premises: Seq[Premise], conclusion: ProvenStatement): String = {
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

  case class DirectPremise(statement: Statement, isElidable: Boolean) extends Premise {
    override def statements = Seq(statement)
    override def applySubstitutions(substitutions: Substitutions) = {
      for {
        substitutedStatement <- statement.applySubstitutions(substitutions)
      } yield DirectPremise(substitutedStatement)
    }
    override def html = statement.html
    override def serialized = statement.serialized
  }
  object DirectPremise {
    def apply(statement: Statement): DirectPremise = DirectPremise(statement, isElidable = false)
    def unapply(obj: Object): Option[Statement] = obj match {
      case directPremise: DirectPremise => Some(directPremise.statement)
      case _ => None
    }
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
    for {
      statement <- Statement.parser
      isElidable <- Parser.optionalWord("elidable").isDefined
    } yield {
      DirectPremise(statement, isElidable)
    }
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
