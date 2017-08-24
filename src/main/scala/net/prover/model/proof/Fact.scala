package net.prover.model.proof

import net.prover.model.{Parser, ParsingContext, Substitutions}
import net.prover.model.components.{Statement, Variable}

sealed trait Fact {
  def variables: Seq[Variable]
  def serialized: String
  def applySubstitutions(substitutions: Substitutions): Option[Fact]
  def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions]
}

object Fact {
  case class Direct(statement: Statement) extends Fact {
    override def variables = statement.variables
    override def serialized = statement.serialized
    override def applySubstitutions(substitutions: Substitutions): Option[Direct] = {
      for {
        substitutedStatement <- statement.applySubstitutions(substitutions)
      } yield Direct(substitutedStatement)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Direct(otherStatement) =>
        statement.calculateSubstitutions(otherStatement, substitutionsSoFar)
      case _ =>
        Nil
    }
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      for {
        statement <- Statement.parser
      } yield Direct(statement)
    }
  }
  case class Deduced(antecedent: Statement, consequent: Statement) extends Fact {
    override def variables = (antecedent.variables ++ consequent.variables).distinct
    override def serialized = s"proves ${antecedent.serialized} ${consequent.serialized}"
    override def applySubstitutions(substitutions: Substitutions): Option[Deduced] = {
      for {
        substitutedAntecedent <- antecedent.applySubstitutions(substitutions)
        substitutedConsequent <- consequent.applySubstitutions(substitutions)
      } yield Deduced(substitutedAntecedent, substitutedConsequent)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Deduced(otherAntecedent, otherConsequent) =>
        for {
          s1 <- antecedent.calculateSubstitutions(otherAntecedent, substitutionsSoFar)
          s2 <- consequent.calculateSubstitutions(otherConsequent, s1)
        } yield s2
      case _ =>
        Nil
    }
  }
  object Deduced {
    def parser(implicit parsingContext: ParsingContext): Parser[Deduced] = {
      for {
        antecedent <- Statement.parser
        consequent <- Statement.parser
      } yield Deduced(antecedent, consequent)
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Fact] = {
    Parser.optionalWord("proves")
      .mapFlatMap(_ => Deduced.parser)
      .orElse(Direct.parser)
  }
}
