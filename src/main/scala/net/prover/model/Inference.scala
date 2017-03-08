package net.prover.model

import java.security.MessageDigest

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

trait Inference {
  def name: String
  def premises: Seq[Premise]
  def conclusion: ProvenStatement

  def applySubstitutions(substitutions: Substitutions): Inference = new Inference {
    override def name = Inference.this.name
    override def premises = Inference.this.premises.map(_.applySubstitutions(substitutions))
    override def conclusion = Inference.this.conclusion.applySubstitutions(substitutions)
  }

  def calculateHash(): String = {
    val serialized = (premises.map(_.serialized) :+ conclusion.statement.serialized).mkString("\n")
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(serialized.getBytes("UTF-8"))
    String.format("%064x", new java.math.BigInteger(1, sha.digest()))
  }
}

object Inference {
  def unapply(inference: Inference): Option[(String, Seq[Premise], ProvenStatement)] = {
    Some(inference.name, inference.premises, inference.conclusion)
  }

  sealed trait Premise {
    def applySubstitutions(substitutions: Substitutions): Premise
    def serialized: String
  }

  case class DirectPremise(statement: Statement) extends Premise {
    override def applySubstitutions(substitutions: Substitutions) = {
      val substitutedStatement = statement.applySubstitutions(substitutions)
      DirectPremise(substitutedStatement)
    }
    override def serialized = statement.serialized
  }
  case class DeducedPremise(antecedent: Statement, consequent: Statement) extends Premise {
    override def applySubstitutions(substitutions: Substitutions) = {
      val substitutedAntecedent = antecedent.applySubstitutions(substitutions)
      val substitutedConsequent = consequent.applySubstitutions(substitutions)
      DeducedPremise(substitutedAntecedent, substitutedConsequent)
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

  def arbitraryVariablesParser(implicit context: Context): Parser[Seq[TermVariable]] = {
    Parser.optional("arbitrary-variables", Term.variableListParser, Nil)
  }

  def distinctVariablesParser(implicit context: Context): Parser[DistinctVariables] = {
    Parser.optional("distinct-variables", DistinctVariables.parser, DistinctVariables.empty)
  }
}
