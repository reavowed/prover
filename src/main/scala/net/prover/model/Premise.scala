package net.prover.model

import net.prover.model.expressions.Statement
import net.prover.model.proof.{PreviousLineReference, ProvenStatement, Reference}

case class Premise(statement: Statement, index: Int)(val isElidable: Boolean) {
  def reference = Reference.Direct(s"p$index")
  def provenStatement = ProvenStatement(statement, PreviousLineReference(reference.value, Nil))
  def requiredSubstitutions: Substitutions.Required = statement.requiredSubstitutions
  def insertExternalParameters(numberOfParametersToInsert: Int): Premise = {
    withStatement(statement.insertExternalParameters(numberOfParametersToInsert))
  }
  def withStatement(newStatement: Statement): Premise = copy(statement = newStatement)(isElidable)
  def serialized: String = s"premise ${statement.serialized}" + (if (isElidable) " elidable" else "")
  def serializedForHash: String = s"premise ${statement.serializedForHash}"
}

object Premise {
    def parser(index: Int)(implicit context: ParsingContext): Parser[Option[Premise]] = {
      Parser.optional("premise", for {
          statement <- Statement.parser
          isElidable <- Parser.optionalWord("elidable").isDefined
      } yield Premise(statement, index)(isElidable))
    }
    def listParser(implicit context: ParsingContext): Parser[Seq[Premise]] = {
      Parser.whileDefined[Premise] { (_, index) => parser(index) }
    }
}
