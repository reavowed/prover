package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference._
import net.prover.model.expressions._
import net.prover.model.proof.Fact

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  val id: String = calculateHash()
  def chapterKey: String
  def bookKey: String
  def keyOption: Option[String]
  def name: String
  def premises: Seq[Premise]
  def conclusion: Statement
  def rearrangementType: RearrangementType
  def allowsRearrangement: Boolean

  def requiredSubstitutions: Substitutions.Required = {
    (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether
  }

  def specifySubstitutions(substitutions: Substitutions): Option[Inference.Substitutions] = {
    for {
      statements <- requiredSubstitutions.statements.map(substitutions.statements.get).traverseOption
      terms <- requiredSubstitutions.terms.map(substitutions.terms.get).traverseOption
      predicates <- requiredSubstitutions.predicates.map(name => substitutions.predicates.get(name)).traverseOption
      functions <- requiredSubstitutions.functions.map(name => substitutions.functions.get(name)).traverseOption
    } yield Inference.Substitutions(statements, terms, predicates, functions)
  }
  def generalizeSubstitutions(inferenceSubstitutions: Inference.Substitutions, depth: Int): Option[Substitutions] = {
    for {
      statementsByName <- requiredSubstitutions.statements.zipStrict(inferenceSubstitutions.statements).map(_.toMap)
      termsByName <- requiredSubstitutions.terms.zipStrict(inferenceSubstitutions.terms).map(_.toMap)
      predicatesByName <- requiredSubstitutions.predicates.zipStrict(inferenceSubstitutions.predicates).map(_.toMap)
      functionsByName <- requiredSubstitutions.functions.zipStrict(inferenceSubstitutions.functions).map(_.toMap)
    } yield Substitutions(statementsByName, termsByName, predicatesByName, functionsByName, depth)
  }
  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion)
  }
}

object Inference {
  trait Entry extends Inference {
    def key: String
    override def keyOption = Some(key)
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
    override def allowsRearrangement = true
    override def rearrangementType = RearrangementType.NotRearrangement
    override def keyOption = None
  }

  case class Transformed(
      inner: Inference,
      premises: Seq[Premise],
      conclusion: Statement)
    extends Inference
  {
    override def keyOption = inner.keyOption
    override def chapterKey = inner.chapterKey
    override def bookKey = inner.bookKey
    override def name = inner.name
    override def rearrangementType = inner.rearrangementType
    override def allowsRearrangement = inner.allowsRearrangement
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

  case class Substitutions(
      statements: Seq[Statement],
      terms: Seq[Term],
      predicates: Seq[Statement],
      functions: Seq[Term])
  {
    def serialized: String = Seq(statements, terms, predicates, functions)
      .map(x => "(" + x.map(_.serialized).mkString(", ") + ")")
      .mkString(" ")
  }
  object Substitutions {
    def parser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
      for {
        statements <- Statement.parser.listInParens(Some(","))
        terms <- Term.parser.listInParens(Some(","))
        predicates <- Statement.parser(parsingContext.addParameterList(Nil)).listInParens(Some(","))
        functions <- Term.parser(parsingContext.addParameterList(Nil)).listInParens(Some(","))
      } yield Substitutions(statements, terms, predicates, functions)
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
}
