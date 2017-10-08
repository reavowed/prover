package net.prover.model.proof

import net.prover.model.expressions._
import net.prover.model.{Parser, ParsingContext, Substitutions}

sealed trait Fact {
  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Fact]
  def increaseDepth(additionalDepth: Int): Fact
  def serialized: String
}

object Fact {
  case class Direct(assertion: Assertable) extends Fact {
    override def requiredSubstitutions = assertion.requiredSubstitutions
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Direct(otherAssertion) =>
        assertion.calculateSubstitutions(otherAssertion, substitutionsSoFar)
      case _ =>
        Nil
    }
    override def applySubstitutions(substitutions: Substitutions): Option[Direct] = {
      for {
        substitutedAssertion <- assertion.applySubstitutions(substitutions)
      } yield Direct(substitutedAssertion)
    }
    override def increaseDepth(additionalDepth: Int) = {
      Direct(assertion.increaseDepth(additionalDepth))
    }
    override def serialized = assertion.serialized
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
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case Deduced(otherAntecedent, otherConsequent) =>
        for {
          s1 <- antecedent.calculateSubstitutions(otherAntecedent, substitutionsSoFar)
          s2 <- consequent.calculateSubstitutions(otherConsequent, s1)
        } yield s2
      case _ =>
        Nil
    }
    override def applySubstitutions(substitutions: Substitutions): Option[Deduced] = {
      for {
        substitutedAntecedent <- antecedent.applySubstitutions(substitutions)
        substitutedConsequent <- consequent.applySubstitutions(substitutions)
      } yield Deduced(substitutedAntecedent, substitutedConsequent)
    }
    override def increaseDepth(additionalDepth: Int) = {
      Deduced(antecedent.increaseDepth(additionalDepth), consequent.increaseDepth(additionalDepth))
    }
    override def serialized = s"proves ${antecedent.serialized} ${consequent.serialized}"
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
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case ScopedVariable(otherPredicate) =>
        assertion.calculateSubstitutions(otherPredicate, substitutionsSoFar)
      case _ =>
        Nil
    }
    override def applySubstitutions(substitutions: Substitutions): Option[ScopedVariable] = {
      for {
        updatedAssertion <- assertion.applySubstitutions(substitutions)
      } yield ScopedVariable(updatedAssertion)(variableName)
    }
    override def increaseDepth(additionalDepth: Int) = {
      ScopedVariable(assertion.increaseDepth(additionalDepth))(variableName)
    }
    override def serialized: String = s"binding $variableName ${assertion.serialized}"
  }
  object ScopedVariable {
    def parser(implicit parsingContext: ParsingContext): Parser[ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addParameterList(Seq(variableName))
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
