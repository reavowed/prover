package net.prover.model

import java.security.MessageDigest

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

trait Inference {
  def id: String
  def name: String
  def premises: Seq[Premise]
  def conclusion: ProvenStatement

  def applySubstitutions(substitutions: Substitutions): Option[Inference] = {
    for {
      updatedPremises <- premises.map(_.applySubstitutions(substitutions)).traverseOption
      updatedConclusion <- conclusion.applySubstitutions(substitutions)
    } yield DerivedInference(Some(id), name, updatedPremises, updatedConclusion)
  }

  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion.statement)
  }
}

case class DerivedInference(
    idOption: Option[String],
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement)
  extends Inference {
  def id: String = idOption.getOrElse(calculateHash())
}

object Inference {
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
    def serialized: String
  }

  case class DirectPremise(statement: Statement) extends Premise {
    override def statements = Seq(statement)
    override def applySubstitutions(substitutions: Substitutions) = {
      for {
        substitutedStatement <- statement.applySubstitutions(substitutions)
      } yield DirectPremise(substitutedStatement)
    }
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
    Parser.singleWord.onlyIf(_ == "premise").mapFlatMap { _ =>
      Parser.singleWord.onlyIf(_ == "proves").mapFlatMap(_ => deducedPremiseParser)
        .orElse(directPremiseParser)
    }
  }

  def premisesParser(implicit context: Context): Parser[Seq[Premise]] = {
    premiseParser.whileDefined
  }
}
