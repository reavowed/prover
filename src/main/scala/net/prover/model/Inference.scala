package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference._
import net.prover.model.components.Statement

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

  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion)
  }
}

trait EntryInference extends Inference {
  def key: String
  def chapterKey: String
  def bookKey: String

  override def keyOption = Some(key)
  override def summary = Summary(name, id, key, chapterKey, bookKey)
}

case class DefinitionInference(
    nameOfDefinition: String,
    chapterKey: String,
    bookKey: String,
    premisesStatements: Seq[Statement],
    conclusion: Statement)
  extends Inference
{
  override def premises = premisesStatements.map(Premise.DirectPremise.apply)
  override def name: String = s"Definition of $nameOfDefinition"
  override def summary: Inference.Summary = Summary(name, id, name.formatAsKey, chapterKey, bookKey)
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
        _ <- Parser.toEndOfLine
      } yield Inference.Summary("", id, "", "", "")
    }
  }
}
