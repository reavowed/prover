package net.prover.model.proof

import net.prover.model.components._
import net.prover.model.{Parser, ParsingContext, Substitutions}

sealed trait Fact {
  def requiredSubstitutions: Substitutions.Required
  def serialized: String
  def applySubstitutions(substitutions: Substitutions): Option[Fact]
  def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions]
}

object Fact {
  case class Direct(statement: Statement) extends Fact {
    override def requiredSubstitutions = statement.requiredSubstitutions
    override def serialized = statement.serialized
    override def applySubstitutions(substitutions: Substitutions): Option[Direct] = {
      for {
        substitutedStatement <- statement.applySubstitutions(substitutions)
      } yield Direct(substitutedStatement)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Direct(otherStatement) =>
        statement.calculateSubstitutions(otherStatement, substitutionsSoFar, 0)
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
    override def requiredSubstitutions = antecedent.requiredSubstitutions ++ consequent.requiredSubstitutions
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
          s1 <- antecedent.calculateSubstitutions(otherAntecedent, substitutionsSoFar, 0)
          s2 <- consequent.calculateSubstitutions(otherConsequent, s1, 0)
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

  case class Bound(statement: Statement)(val boundVariableName: String) extends Fact {
    override def requiredSubstitutions = statement.requiredSubstitutions
    override def serialized: String = s"binding $boundVariableName ${statement.serialized}"
    override def applySubstitutions(substitutions: Substitutions): Option[Bound] = {
      for {
        updatedStatement <- statement.applySubstitutions(substitutions)
      } yield Bound(updatedStatement)(boundVariableName)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Bound(otherStatement) =>
        statement.calculateSubstitutions(otherStatement, substitutionsSoFar, 0)
      case _ =>
        Nil
    }
  }
  object Bound {
    def parser(implicit parsingContext: ParsingContext): Parser[Bound] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addBoundVariable(variableName)
        statementWithBinding <- Statement.parser(updatedContext)
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
