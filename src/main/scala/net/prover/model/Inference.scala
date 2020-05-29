package net.prover.model

import java.security.MessageDigest

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonIgnoreProperties}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.Inference._
import net.prover.model.definitions.{Definitions, ExpressionDefinition}
import net.prover.model.entries.{ChapterEntry, ChapterEntryParser}
import net.prover.model.expressions._
import net.prover.model.proof.{StepContext, SubstitutionContext}

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement"))
trait Inference {
  @JsonSerialize
  def id: String
  @JsonSerialize
  def name: String
  @JsonSerialize
  def premises: Seq[Statement]
  @JsonSerialize
  def conclusion: Statement

  @JsonIgnore
  def variableDefinitions: VariableDefinitions

  def summary: Summary = Summary(this)

  def substitutionsParser(implicit parsingContext: ExpressionParsingContext): Parser[Substitutions] = {
    for {
      statements <- variableDefinitions.statements.map(d => Statement.parser(parsingContext.addInitialParameters(d.arity))).inParens(Some(","))
      terms <- variableDefinitions.terms.map(d => Term.parser(parsingContext.addInitialParameters(d.arity))).inParens(Some(","))
    } yield {
      if (statements.length != variableDefinitions.statements.length) {
        throw new Exception(s"Invalid number of statements in substitutions - expected ${variableDefinitions.statements.length}, got ${statements.length}")
      }
      if (terms.length != variableDefinitions.terms.length) {
        throw new Exception(s"Invalid number of terms in substitutions - expected ${variableDefinitions.terms.length}, got ${terms.length}")
      }
      Substitutions(statements, terms)
    }
  }

  def validatePremisesAndConclusion(expectedPremises: Seq[Statement], expectedConclusion: Statement, substitutions: Substitutions)(implicit stepContext: StepContext): Option[Unit] = {
    for {
      substitutedConclusion <- substituteConclusion(substitutions)
      if substitutedConclusion == expectedConclusion
      substitutedPremises <- substitutePremises(substitutions)
      if substitutedPremises == expectedPremises
    } yield ()
  }

  def substitutePremisesAndValidateConclusion(expectedConclusion: Statement, substitutions: Substitutions)(implicit stepContext: StepContext): Option[Seq[Statement]] = {
    for {
      substitutedConclusion <- substituteConclusion(substitutions)
      if substitutedConclusion == expectedConclusion
      substitutedPremises <- substitutePremises(substitutions)
    } yield substitutedPremises
  }

  def substitutePremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[Seq[Statement]] = {
    premises.map(substituteStatement(_, substitutions)).traverseOption
  }

  def substituteConclusion(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[Statement] = {
    conclusion.applySubstitutions(substitutions)
  }

  def substituteStatement(statement: Statement, substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[Statement] = {
    statement.applySubstitutions(substitutions)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Inference]
  override def equals(other: Any): Boolean = other match {
    case that: Inference =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = name
}

object Inference {
  trait WithCalculatedId extends Inference {
    val id: String = Inference.calculateHash(premises, conclusion)
  }
  trait FromEntry extends WithCalculatedId {
    def isComplete(definitions: Definitions): Boolean
  }
  trait Entry extends Inference.FromEntry with ChapterEntry.Standalone with ChapterEntry.HasChangeableName {
    override def title: String = name
    def withName(newName: String): Entry
  }
  trait EntryParser extends ChapterEntryParser {
    def premisesParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = {
      Parser.optional("premise", Statement.parser).whileDefined
    }
    def conclusionParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
      Parser.required("conclusion", Statement.parser)
    }
  }

  case class Summary(
    name: String,
    id: String,
    variableDefinitions: VariableDefinitions,
    premises: Seq[Statement],
    conclusion: Statement
  ) extends Inference {
    def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Summary = {
      val newPremises = premises.map(_.replaceDefinitions(expressionDefinitionReplacements))
      val newConclusion = conclusion.replaceDefinitions(expressionDefinitionReplacements)
      Summary(
        name,
        Inference.calculateHash(newPremises, newConclusion),
        variableDefinitions,
        newPremises,
        newConclusion)
    }
  }
  object Summary {
    def apply(inference: Inference): Summary = {
      inference.asOptionalInstanceOf[Summary]
        .getOrElse(Summary(inference.name, inference.id, inference.variableDefinitions, inference.premises, inference.conclusion))
    }
  }

  trait Definition extends Inference.FromEntry {
    def nameOfDefinition: String
    override def name: String = s"Definition of ${nameOfDefinition.capitalizeWords}"
    override def isComplete(definitions: Definitions): Boolean = true
  }

  case class StatementDefinition(
      nameOfDefinition: String,
      variableDefinitions: VariableDefinitions,
      premise: Statement,
      conclusion: Statement)
    extends Inference.Definition
  {
    override def premises: Seq[Statement] = Seq(premise)
  }

  case class TermDefinition(
      nameOfDefinition: String,
      variableDefinitions: VariableDefinitions,
      premises: Seq[Statement],
      conclusion: Statement)
    extends Inference.Definition

  def unapply(inference: Inference): Option[(String, Seq[Statement], Statement)] = {
    Some(inference.name, inference.premises, inference.conclusion)
  }

  def calculateHash(premises: Seq[Statement], conclusion: Statement): String = {
    val serialized = (premises.map("premise " + _.serializedForHash) :+ conclusion.serializedForHash).mkString("\n")
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(serialized.getBytes("UTF-8"))
    String.format("%064x", new java.math.BigInteger(1, sha.digest()))
  }

  def parser(implicit entryContext: EntryContext): Parser[Inference.Summary] = {
    for {
      inferenceId <- Parser.singleWord
    } yield {
      entryContext.inferencesById
        .getOrElse(inferenceId, throw new Exception(s"Could not find inference with id $inferenceId"))
        .summary
    }
  }
}
