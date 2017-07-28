package net.prover.model

import net.prover.model.components.{Statement, Variable}

sealed trait Premise {
  def statements: Seq[Statement]
  def applySubstitutions(substitutions: Substitutions): Option[Premise]
  def matches(other: Premise): Boolean
  def variables: Seq[Variable]
  def html: String
  def serialized: String
}

object Premise {

  case class DirectPremise(statement: Statement, isElidable: Boolean) extends Premise {
    override def statements = Seq(statement)
    override def applySubstitutions(substitutions: Substitutions) = {
      for {
        substitutedStatement <- statement.applySubstitutions(substitutions)
      } yield DirectPremise(substitutedStatement)
    }
    override def matches(other: Premise): Boolean = other match {
      case DirectPremise(`statement`) =>
        true
      case _ =>
        false
    }
    override def variables = statement.variables
    override def html = statement.html
    override def serialized = s"premise ${statement.serialized}"
  }
  object DirectPremise {
    def apply(statement: Statement): DirectPremise = DirectPremise(statement, isElidable = false)
    def unapply(obj: Object): Option[Statement] = obj match {
      case directPremise: DirectPremise => Some(directPremise.statement)
      case _ => None
    }

    def parser(implicit context: ParsingContext): Parser[DirectPremise] = {
      for {
        statement <- Statement.parser
        isElidable <- Parser.optionalWord("elidable").isDefined
      } yield {
        DirectPremise(statement, isElidable)
      }
    }
  }

  case class DeducedPremise(antecedent: Statement, consequent: Statement) extends Premise {
    override def statements = Seq(antecedent, consequent)
    override def applySubstitutions(substitutions: Substitutions) = {
      for {
        substitutedAntecedent <- antecedent.applySubstitutions(substitutions)
        substitutedConsequent <- consequent.applySubstitutions(substitutions)
      } yield DeducedPremise(substitutedAntecedent, substitutedConsequent)
    }
    override def matches(other: Premise): Boolean = other match {
      case DeducedPremise(`antecedent`, `consequent`) =>
        true
      case _ =>
        false
    }

    override def variables = (antecedent.variables ++ consequent.variables).distinct
    override def html = antecedent.html + " ⊢ " + consequent.html
    override def serialized = Seq("premise", "proves", antecedent.serialized, consequent.serialized).mkString(" ")
  }

  object DeducedPremise {
    def parser(implicit context: ParsingContext): Parser[DeducedPremise] = {
      for {
        antecedent <- Statement.parser
        consequent <- Statement.parser
      } yield {
        DeducedPremise(antecedent, consequent)
      }
    }
  }

  def parser(implicit context: ParsingContext): Parser[Option[Premise]] = {
    Parser.optionalWord("premise")
      .mapFlatMap { _ =>
        Parser.optionalWord("proves")
          .mapFlatMap(_ => DeducedPremise.parser)
          .orElse(DirectPremise.parser)
      }
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Premise]] = {
    parser.whileDefined
  }
}
