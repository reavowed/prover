package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, RearrangementType}

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  def id: String
  def keyOption: Option[String]
  def name: String
  def premises: Seq[Premise]
  def conclusion: ProvenStatement
  def rearrangementType: RearrangementType
  def allowsRearrangement: Boolean

  def applySubstitutions(substitutions: Substitutions): Option[Inference] = {
    for {
      updatedPremises <- premises.map(_.applySubstitutions(substitutions)).traverseOption
      updatedConclusion <- conclusion.applySubstitutions(substitutions)
    } yield DerivedInference(Some(id), name, updatedPremises, updatedConclusion, rearrangementType, allowsRearrangement)
  }

  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion.statement)
  }
}

case class DerivedInference(
    idOption: Option[String],
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    rearrangementType: RearrangementType,
    allowsRearrangement: Boolean)
  extends Inference {
  override def id: String = idOption.getOrElse(calculateHash())
  override def keyOption: Option[String] = None
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
