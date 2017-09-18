package net.prover.model

import net.prover.model.expressions.Variable
import net.prover.model.proof.{Fact, Reference, ReferencedFact}

case class Premise(fact: Fact, index: Int)(val isElidable: Boolean) {
  def reference = Reference.Direct(s"p$index")
  def referencedFact = ReferencedFact(fact, reference)
  def requiredSubstitutions: Substitutions.Required = fact.requiredSubstitutions
  def serialized: String = s"premise ${fact.serialized}"
}

object Premise {
    def parser(index: Int)(implicit context: ParsingContext): Parser[Option[Premise]] = {
      Parser.optional("premise", for {
          fact <- Fact.parser
          isElidable <- Parser.optionalWord("elidable").isDefined
      } yield Premise(fact, index)(isElidable))
    }
    def listParser(implicit context: ParsingContext): Parser[Seq[Premise]] = {
      Parser.iterateWhileDefined((Seq.empty[Premise], 0)) { case (acc, index) =>
        parser(index).mapMap { p => (acc :+ p, index + 1) }
      }.map(_._1)
    }
}
