package net.prover.model.proof

import net.prover.model.expressions._
import net.prover.model.{Parser, ParsingContext, Substitutions}

import scalaz.Endo
import scalaz.Scalaz._

sealed trait Fact {
  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Fact]
  def increaseDepth(additionalDepth: Int): Fact
  def childDetails: Option[(Fact, Int, Fact => Fact)]
  def iteratedChildDetails(level: Int): Option[(Fact, Int, Fact => Fact)] = {
    def helper(acc: Option[(Fact, Int, Fact => Fact)]): Option[(Fact, Int, Fact => Fact)] = {
      for {
        (fact, accLevel, updater) <- acc
        (childFact, childLevel, childUpdater) <- fact.childDetails
      } yield (childFact, accLevel + childLevel, childUpdater andThen updater)
    }
    Endo(helper).multiply(level).apply(Some((this, 0, identity[Fact])))
  }
  def serialized: String
}

object Fact {
  case class Direct(assertion: Statement) extends Fact {
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
    override def childDetails = None
    override def serialized = assertion.serialized
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      for {
        assertion <- Statement.parser
      } yield Direct(assertion)
    }
  }
  case class Deduced(antecedent: Statement, consequent: Fact) extends Fact {
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
    override def childDetails = Some((consequent, 0, Deduced(antecedent, _)))
    override def serialized = s"proves ${antecedent.serialized} ${consequent.serialized}"
  }
  object Deduced {
    def parser(implicit parsingContext: ParsingContext): Parser[Deduced] = {
      for {
        antecedent <- Statement.parser
        consequent <- Fact.parser
      } yield Deduced(antecedent, consequent)
    }
  }

  case class ScopedVariable(scopedFact: Fact)(val variableName: String) extends Fact {
    override def requiredSubstitutions = scopedFact.requiredSubstitutions
    def calculateSubstitutions(otherFact: Fact, substitutionsSoFar: Substitutions): Seq[Substitutions] = otherFact match {
      case ScopedVariable(otherFact) =>
        scopedFact.calculateSubstitutions(otherFact, substitutionsSoFar)
      case _ =>
        Nil
    }
    override def applySubstitutions(substitutions: Substitutions): Option[ScopedVariable] = {
      for {
        updatedFact <- scopedFact.applySubstitutions(substitutions)
      } yield ScopedVariable(updatedFact)(variableName)
    }
    override def increaseDepth(additionalDepth: Int) = {
      ScopedVariable(scopedFact.increaseDepth(additionalDepth))(variableName)
    }
    override def childDetails = Some((scopedFact, 1, ScopedVariable(_)(variableName)))
    override def serialized: String = s"binding $variableName ${scopedFact.serialized}"
  }
  object ScopedVariable {
    def parser(implicit parsingContext: ParsingContext): Parser[ScopedVariable] = {
      for {
        variableName <- Parser.singleWord
        updatedContext = parsingContext.addParameterList(Seq(variableName))
        scopedFact <- Fact.parser(updatedContext)
      } yield {
        ScopedVariable(scopedFact)(variableName)
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
