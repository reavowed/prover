package net.prover.model.definitions

import net.prover.model.Substitutions
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext

class StatementWrapper(baseStatement: Statement, variableName: String) {
  def apply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Statement = baseStatement.applySubstitutions(Substitutions(statements = Map(variableName -> statement))).get
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[Statement] = baseStatement.calculateSubstitutions(statement).flatMap(_.statements.get(variableName))
}
