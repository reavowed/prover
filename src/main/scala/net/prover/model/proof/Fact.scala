package net.prover.model.proof

import net.prover.model.expressions._
import net.prover.model.{Parser, ParsingContext, Substitutions}

sealed trait Fact {
  def requiredSubstitutions: Substitutions.Required
  def serialized: String
  def applySubstitutions(substitutions: Substitutions): Option[Fact]
  def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions]
}

object Fact {
  case class Direct(assertion: Assertable) extends Fact {
    override def requiredSubstitutions = assertion.requiredSubstitutions
    override def serialized = assertion.serialized
    override def applySubstitutions(substitutions: Substitutions): Option[Direct] = {
      for {
        substitutedAssertion <- assertion.applySubstitutions(substitutions)
      } yield Direct(substitutedAssertion)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Direct(otherAssertion) =>
        assertion.calculateSubstitutions(otherAssertion, substitutionsSoFar, 0)
      case _ =>
        Nil
    }
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      for {
        assertion <- Assertable.parser
      } yield Direct(assertion)
    }
  }
  case class Deduced(antecedent: Assertable, consequent: Assertable) extends Fact {
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
        antecedent <- Assertable.parser
        consequent <- Assertable.parser
      } yield Deduced(antecedent, consequent)
    }
  }

  case class ScopedVariable(assertion: Assertable)(val variableName: String) extends Fact {
    override def requiredSubstitutions = assertion.requiredSubstitutions
    override def serialized: String = s"binding $variableName ${assertion.serialized}"
    override def applySubstitutions(substitutions: Substitutions): Option[ScopedVariable] = {
      for {
        updatedAssertion <- assertion.applySubstitutions(substitutions)
      } yield ScopedVariable(updatedAssertion)(variableName)
    }
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case ScopedVariable(otherPredicate) =>
        assertion.calculateSubstitutions(otherPredicate, substitutionsSoFar, 0)
      case _ =>
        Nil
    }
  }
  object ScopedVariable {
    def parser(implicit parsingContext: ParsingContext): Parser[ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addBoundVariable(variableName)
        predicate <- Assertable.parser(updatedContext)
      } yield {
        ScopedVariable(predicate)(variableName)
      }
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Fact] = {
    Parser.selectOptionalWordParser {
      case "proves" => Deduced.parser
      case "binding" => ScopedVariable.parser
    }.orElse(Direct.parser)
  }
}
