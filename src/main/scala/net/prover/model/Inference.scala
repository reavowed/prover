package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference._
import net.prover.model.entries.ChapterEntry
import net.prover.model.expressions._
import net.prover.model.proof.Transformation

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  val id: String = calculateHash()
  def entryKeyOption: Option[ChapterEntry.Key]
  def name: String
  def premises: Seq[Premise]
  def conclusion: Statement
  def rearrangementType: RearrangementType

  def requiredSubstitutions: Substitutions.Required = {
    (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether
  }

  def substitutionsParser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
    for {
      statements <- Statement.parser.listInParens(Some(","))
      terms <- Term.parser.listInParens(Some(","))
      predicates <- Statement.parser(parsingContext.addParameterList(Nil)).listInParens(Some(","))
      functions <- Term.parser(parsingContext.addParameterList(Nil)).listInParens(Some(","))
    } yield {
      Substitutions(
        requiredSubstitutions.statements.zip(statements).toMap,
        requiredSubstitutions.terms.zip(terms).toMap,
        requiredSubstitutions.predicates.zip(predicates).toMap,
        requiredSubstitutions.functions.zip(functions).toMap)
    }
  }

  def serializeSubstitutions(substitutions: Substitutions): String = {
    Seq(
      requiredSubstitutions.statements.map(substitutions.statements.apply),
      requiredSubstitutions.terms.map(substitutions.terms.apply),
      requiredSubstitutions.predicates.map(substitutions.predicates.apply),
      requiredSubstitutions.functions.map(substitutions.functions.apply)
    )
      .map(x => "(" + x.map(_.serialized).mkString(", ") + ")")
      .mkString(" ")
  }

  def calculateHash(): String = {
    Inference.calculateHash(premises, conclusion)
  }
}

object Inference {
  trait Entry extends Inference with ChapterEntry.WithKey {
    override def entryKeyOption = Some(key)
  }

  case class Definition(
      nameOfDefinition: String,
      premiseStatements: Seq[Statement],
      conclusion: Statement)
    extends Inference
  {
    override def premises = premiseStatements.zipWithIndex.map { case (premiseStatement, index) =>
      Premise(premiseStatement, index)(isElidable = false)
    }
    override def name: String = s"Definition of $nameOfDefinition"
    override def rearrangementType = RearrangementType.NotRearrangement
    override def entryKeyOption = None
  }

  case class Transformed(
      inner: Inference,
      transformation: Transformation,
      premises: Seq[Premise],
      conclusion: Statement)
    extends Inference
  {
    override def entryKeyOption = inner.entryKeyOption
    override def name = inner.name
    override def rearrangementType = inner.rearrangementType

    private def isScoped(statement: Statement) = statement match {
      case DefinedStatement(_, transformation.statementDefinition) => true
      case _ => false
    }

    def arePremisesScoped: Seq[Boolean] = {
      premises.map(p => isScoped(p.statement))
    }
    def isConclusionScoped: Boolean = isScoped(conclusion)
  }

  sealed trait RearrangementType {
    def serialized: Option[String]
  }
  object RearrangementType {
    object NotRearrangement extends RearrangementType {
      override def serialized = None
    }
    object Simplification extends RearrangementType {
      override def serialized = Some("simplification")
    }
    object Expansion extends RearrangementType {
      override def serialized = Some("expansion")
    }

    def parser: Parser[RearrangementType] = {
      Parser.singleWord.map {
        case "simplification" => Some(Simplification)
        case "expansion" => Some(Expansion)
        case _ => None
      }.getOrElse(NotRearrangement)
    }
  }

  case class Substitutions(
      statements: Seq[Option[Statement]],
      terms: Seq[Option[Term]],
      predicates: Seq[Option[Statement]],
      functions: Seq[Option[Term]])
  {
    def serialized: String = Seq(statements, terms, predicates, functions)
      .map(x => "(" + x.map(_.map(_.serialized).getOrElse("%")).mkString(", ") + ")")
      .mkString(" ")
  }
  object Substitutions {
    def parser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
      for {
        statements <- Statement.parser.withNone("%").listInParens(Some(","))
        terms <- Term.parser.withNone("%").listInParens(Some(","))
        predicates <- Statement.parser(parsingContext.addParameters()).withNone("%").listInParens(Some(","))
        functions <- Term.parser(parsingContext.addParameters()).withNone("%").listInParens(Some(","))
      } yield Substitutions(statements, terms, predicates, functions)
    }
  }

  def unapply(inference: Inference): Option[(String, Seq[Premise], Statement)] = {
    Some(inference.name, inference.premises, inference.conclusion)
  }

  def calculateHash(premises: Seq[Premise], conclusion: Statement): String = {
    val serialized = (premises.map(_.serializedForHash) :+ conclusion.serializedForHash).mkString("\n")
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(serialized.getBytes("UTF-8"))
    String.format("%064x", new java.math.BigInteger(1, sha.digest()))
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Inference] = {
    for {
      inferenceId <- Parser.singleWord
    } yield {
      parsingContext.inferences.find(_.id == inferenceId)
        .getOrElse(throw new Exception(s"Could not find inference with id $inferenceId"))
    }
  }
}
