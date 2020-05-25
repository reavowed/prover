package net.prover.model

import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.expressions.Statement

case class VariableDefinitions(statementVariableDefinitions: Seq[VariableDefinition], termVariableDefinitions: Seq[VariableDefinition]) {
  def addSimpleTermVariables(variableNames: Seq[String]): VariableDefinitions = {
    copy(termVariableDefinitions = termVariableDefinitions ++ variableNames.map(VariableDefinition(_, 0, Nil)))
  }

  def serializedLines: Seq[String] = {
    def serialize(variableDefinitions: Seq[VariableDefinition], prefix: String): Option[String] = {
      if (variableDefinitions.nonEmpty) {
        Some(prefix + variableDefinitions.map(_.serialized).mkString(", ").inParens)
      } else {
        None
      }
    }
    serialize(statementVariableDefinitions, "statementVariables").toSeq ++
      serialize(termVariableDefinitions, "termVariables").toSeq
  }
}

object VariableDefinitions {
  val empty: VariableDefinitions = VariableDefinitions(Nil, Nil)

  def fromComponentTypes(componentTypes: Seq[ComponentType]): VariableDefinitions = {
    VariableDefinitions(
      componentTypes.ofType[StatementComponent].map(c => VariableDefinition(c.name, c.arguments.length, Nil)),
      componentTypes.ofType[TermComponent].map(c => VariableDefinition(c.name, c.arguments.length, Nil)))
  }
  def fromStatements(statements: Seq[Statement]): VariableDefinitions = {
    val requiredSubstitutions = statements.map(_.requiredSubstitutions).foldTogether
    VariableDefinitions(
      requiredSubstitutions.statements.map { case (name, arity) => VariableDefinition(name, arity, Nil) },
      requiredSubstitutions.terms.map { case (name, arity) => VariableDefinition(name, arity, Nil) })
  }

  def parser: Parser[VariableDefinitions] = {
    for {
      statementVariableDefinitions <- Parser.optional("statementVariables", VariableDefinition.listParser, Nil)
      termVariableDefinitions <- Parser.optional("termVariables", VariableDefinition.listParser, Nil)
    } yield VariableDefinitions(statementVariableDefinitions, termVariableDefinitions)
  }
}
