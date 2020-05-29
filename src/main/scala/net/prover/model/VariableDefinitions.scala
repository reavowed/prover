package net.prover.model

import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.expressions.Statement

case class VariableDefinitions(statements: Seq[VariableDefinition], terms: Seq[VariableDefinition]) {
  def addSimpleTermVariables(variableNames: Seq[String]): VariableDefinitions = {
    copy(terms = terms ++ variableNames.map(VariableDefinition(_, 0, Nil)))
  }

  def serializedLines: Seq[String] = {
    def serialize(variableDefinitions: Seq[VariableDefinition], prefix: String): Option[String] = {
      if (variableDefinitions.nonEmpty) {
        Some(prefix + variableDefinitions.map(_.serialized).mkString(", ").inParens)
      } else {
        None
      }
    }
    serialize(statements, "statementVariables").toSeq ++
      serialize(terms, "termVariables").toSeq
  }

  def isEmpty: Boolean = statements.isEmpty && terms.isEmpty
  def hasNoApplications: Boolean = (statements ++ terms).forall(_.arity == 0)
}

object VariableDefinitions {
  val empty: VariableDefinitions = VariableDefinitions(Nil, Nil)

  def fromComponentTypes(componentTypes: Seq[ComponentType]): VariableDefinitions = {
    VariableDefinitions(
      componentTypes.ofType[StatementComponent].map(c => VariableDefinition(c.name, c.arguments.length, Nil)),
      componentTypes.ofType[TermComponent].map(c => VariableDefinition(c.name, c.arguments.length, Nil)))
  }

  def parser: Parser[VariableDefinitions] = {
    for {
      statementVariableDefinitions <- Parser.optional("statementVariables", VariableDefinition.listParser, Nil)
      termVariableDefinitions <- Parser.optional("termVariables", VariableDefinition.listParser, Nil)
    } yield VariableDefinitions(statementVariableDefinitions, termVariableDefinitions)
  }
}
