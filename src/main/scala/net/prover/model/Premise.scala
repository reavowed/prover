package net.prover.model

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Reference, ProvenFact}

case class Premise(statement: Statement, index: Int)(val isElidable: Boolean) {
  def reference = Reference.Direct(s"p$index")
  def referencedFact = ProvenFact(statement, reference)
  def requiredSubstitutions: Substitutions.Required = statement.requiredSubstitutions
  def increaseDepth(additionalDepth: Int, insertionPoint: Int): Premise = {
    Premise(statement.increaseDepth(additionalDepth, insertionPoint), index)(isElidable)
  }
  def serialized: String = s"premise ${statement.serialized}"
}

object Premise {
    def parser(index: Int)(implicit context: ParsingContext): Parser[Option[Premise]] = {
      Parser.optional("premise", for {
          fact <- Statement.parser
          isElidable <- Parser.optionalWord("elidable").isDefined
      } yield Premise(fact, index)(isElidable))
    }
    def listParser(implicit context: ParsingContext): Parser[Seq[Premise]] = {
      Parser.iterateWhileDefined((Seq.empty[Premise], 0)) { case (acc, index) =>
        parser(index).mapMap { p => (acc :+ p, index + 1) }
      }.map(_._1)
    }
}
