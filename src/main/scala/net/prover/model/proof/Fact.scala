package net.prover.model.proof

import net.prover.model.components._
import net.prover.model.{Parser, ParsingContext, Substitutions}

sealed trait Fact {
  def variablesRequiringSubstitution: Seq[Variable]
  def serialized: String
  def applySubstitutions(substitutions: Substitutions): Option[Fact]
  def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions]
}

object Fact {
  case class Direct(statement: Statement) extends Fact {
    override def variablesRequiringSubstitution = statement.variablesRequiringSubstitution
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
    override def variablesRequiringSubstitution = (antecedent.variablesRequiringSubstitution ++ consequent.variablesRequiringSubstitution).distinct
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

  case class Bound(statement: Statement)(val variableName: String) extends Fact {
    override def variablesRequiringSubstitution = statement.variablesRequiringSubstitution
    override def serialized: String = s"binding $variableName ${statement.serialized}"
    override def applySubstitutions(substitutions: Substitutions): Option[Bound] = {
      for {
        updatedStatement <- statement.applySubstitutions(substitutions)
      } yield Bound(updatedStatement)(variableName)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Bound(otherStatement) =>
        statement.calculateSubstitutions(otherStatement, substitutionsSoFar)
      case _ =>
        Nil
    }
  }
  object Bound {
    def parser(implicit parsingContext: ParsingContext): Parser[Bound] = {
      for {
        variableName <- Parser.singleWord
        statementWithBinding <- Statement.parser(parsingContext.addBoundVariable(variableName))
      } yield {
        Bound(statementWithBinding)(variableName)
      }
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Fact] = {
    Parser.selectOptionalWord {
      case "proves" => Deduced.parser
      case "binding" => Bound.parser
    }.orElse(Direct.parser)
  }
}
