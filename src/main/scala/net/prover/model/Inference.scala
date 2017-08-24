package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference._
import net.prover.model.components.{Component, Statement, Variable}
import net.prover.model.proof.Fact

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  val id: String = calculateHash()
  def keyOption: Option[String]
  def name: String
  def premises: Seq[Premise]
  def conclusion: Statement
  def rearrangementType: RearrangementType
  def allowsRearrangement: Boolean
  def summary: Inference.Summary

  def variables: Seq[Variable] = (premises.flatMap(_.variables) ++ conclusion.variables).distinct
  def specifySubstitutions(substitutions: Substitutions): Option[Inference.Substitutions] = {
    variables.map(substitutions.componentsByVariable.get).traverseOption.map(Inference.Substitutions.apply)
  }
  def generalizeSubstitutions(inferenceSubstitutions: Inference.Substitutions): Option[Substitutions] = {
    variables.zipStrict(inferenceSubstitutions.components).map(_.toMap).map(Substitutions.apply)
  }
  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion)
  }
}

object Inference {
  trait Entry extends Inference {
    def key: String
    def chapterKey: String
    def bookKey: String

    override def keyOption = Some(key)
    override def summary = Summary(name, id, key, chapterKey, bookKey)
  }
  case class Definition(
      nameOfDefinition: String,
      chapterKey: String,
      bookKey: String,
      premiseStatements: Seq[Statement],
      conclusion: Statement)
    extends Inference
  {
    override def premises = premiseStatements.map(Fact.Direct.apply).zipWithIndex.map { case (premiseStatement, index) =>
      Premise(premiseStatement, index)(isElidable = false)
    }
    override def name: String = s"Definition of $nameOfDefinition"
    override def summary: Inference.Summary = Summary(name, id, name.formatAsKey, chapterKey, bookKey)
    override def allowsRearrangement = true
    override def rearrangementType = RearrangementType.NotRearrangement
    override def keyOption = None
  }

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

  case class Substitutions(components: Seq[Component]) {
    def serialized: String = "(" + components.map(_.serialized).mkString(", ") + ")"
  }
  object Substitutions {
    def parser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
      Component.parser.listInParens(Some(",")).map(Substitutions.apply)
    }
  }

  def unapply(inference: Inference): Option[(String, Seq[Premise], Statement)] = {
    Some(inference.name, inference.premises, inference.conclusion)
  }

  def calculateHash(premises: Seq[Premise], conclusion: Statement): String = {
    val serialized = (premises.map(_.serialized) :+ conclusion.serialized).mkString("\n")
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(serialized.getBytes("UTF-8"))
    String.format("%064x", new java.math.BigInteger(1, sha.digest()))
  }

  case class Summary(name: String, id: String, key: String, chapterKey: String, bookKey: String) {
    def serialized: String = s"$id ($name)"
  }
  object Summary {
    def parser: Parser[Summary] = {
      for {
        id <- Parser.singleWord
        _ <- Parser.allInParens
      } yield Inference.Summary("", id, "", "", "")
    }
  }
}
