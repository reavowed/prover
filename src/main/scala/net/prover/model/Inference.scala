package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.Inference._
import net.prover.model.entries.{ChapterEntry, ChapterEntryParser}
import net.prover.model.expressions._

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  @JsonSerialize
  def id: String
  @JsonSerialize
  def key: ChapterEntry.Key
  @JsonSerialize
  def name: String
  @JsonSerialize
  def premises: Seq[Statement]
  @JsonSerialize
  def conclusion: Statement
  def rearrangementType: RearrangementType

  def summary: Summary = Summary(this)

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

  def substitutePremisesAndValidateConclusion(expectedConclusion: Statement, substitutions: Substitutions, externalDepth: Int): Seq[Statement] = {
    validateConclusion(expectedConclusion, substitutions, externalDepth)
    premises.map(substituteStatement(_, substitutions, externalDepth))
  }

  def substituteStatement(statement: Statement, substitutions: Substitutions, externalDepth: Int): Statement = {
    statement.applySubstitutions(substitutions, 0, externalDepth).getOrElse(throw new Exception(s"Could not substitute $statement"))
  }

  def validateConclusion(expectedConclusion: Statement, substitutions: Substitutions, externalDepth: Int): Unit = {
    val substitutedConclusion = conclusion.applySubstitutions(substitutions, 0, externalDepth)
      .getOrElse(throw new Exception(s"Could not substitute conclusion $conclusion"))
    (expectedConclusion == substitutedConclusion)
      .ifFalse(throw new Exception(s"Substituted conclusion $substitutedConclusion was not expected conclusion $expectedConclusion"))
  }
}

object Inference {
  trait WithCalculatedId extends Inference {
    def id: String = calculateHash()
  }
  trait Entry extends Inference.WithCalculatedId with ChapterEntry.Standalone {
    override def title: String = name
  }
  trait EntryParser extends ChapterEntryParser {
    def premisesParser(implicit context: ParsingContext): Parser[Seq[Statement]] = {
      Parser.optional("premise", Statement.parser).whileDefined
    }
    def conclusionParser(implicit context: ParsingContext): Parser[Statement] = {
      for {
        _ <- Parser.requiredWord("conclusion")
        conclusion <- Statement.parser
      } yield conclusion
    }
  }

  case class Summary(key: ChapterEntry.Key, name: String, id: String, premises: Seq[Statement], conclusion: Statement, rearrangementType: RearrangementType) extends Inference
  object Summary {
    def apply(inference: Inference): Summary = {
      inference.asOptionalInstanceOf[Summary].getOrElse(Summary(inference.key, inference.name, inference.id, inference.premises, inference.conclusion, inference.rearrangementType))
    }
  }

  case class Definition(
      nameOfDefinition: String,
      key: ChapterEntry.Key,
      premises: Seq[Statement],
      conclusion: Statement)
    extends Inference.WithCalculatedId
  {
    override def name: String = s"Definition of $nameOfDefinition"
    override def rearrangementType = RearrangementType.NotRearrangement
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

  def unapply(inference: Inference): Option[(String, Seq[Statement], Statement)] = {
    Some(inference.name, inference.premises, inference.conclusion)
  }

  def calculateHash(premises: Seq[Statement], conclusion: Statement): String = {
    val serialized = (premises.map("premise " + _.serializedForHash) :+ conclusion.serializedForHash).mkString("\n")
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(serialized.getBytes("UTF-8"))
    String.format("%064x", new java.math.BigInteger(1, sha.digest()))
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Inference.Summary] = {
    for {
      inferenceId <- Parser.singleWord
    } yield {
      parsingContext.inferences.find(_.id == inferenceId)
        .getOrElse(throw new Exception(s"Could not find inference with id $inferenceId"))
        .summary
    }
  }
}
