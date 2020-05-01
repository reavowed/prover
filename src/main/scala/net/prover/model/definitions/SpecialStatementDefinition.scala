package net.prover.model.definitions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.entries.ChapterEntry
import net.prover.model.expressions.{DefinedStatement, Statement}

@JsonSerialize(using = classOf[SpecialStatementDefinitionSymbolSerializer])
sealed trait SpecialStatementDefinition {
  val statementDefinition: StatementDefinition
  val referencedEntry: ChapterEntry = statementDefinition.associatedChapterEntry
}

object SpecialStatementDefinition {
  sealed trait BinaryConnective extends SpecialStatementDefinition {
    def apply(firstComponent: Statement, secondComponent: Statement): Statement = statementDefinition(firstComponent, secondComponent)
    def unapply(statement: Statement): Option[(Statement, Statement)] = statement match {
      case DefinedStatement(Seq(antecedent: Statement, consequent: Statement), `statementDefinition`) =>
        Some((antecedent, consequent))
      case _ =>
        None
    }
  }
  sealed trait Quantifier extends SpecialStatementDefinition {
    def apply(variableName: String, predicate: Statement): Statement = statementDefinition.bind(variableName)(predicate)
    def unapply(statement: Statement): Option[(String, Statement)] = statement match {
      case definedStatement @ DefinedStatement(Seq(predicate: Statement), `statementDefinition`) =>
        definedStatement.boundVariableNames match {
          case Seq(singleVariableName) =>
            Some((singleVariableName, predicate))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }
}

case class ConjunctionDefinition(statementDefinition: StatementDefinition) extends SpecialStatementDefinition.BinaryConnective
case class DeductionDefinition(statementDefinition: StatementDefinition) extends SpecialStatementDefinition.BinaryConnective
case class GeneralizationDefinition(statementDefinition: StatementDefinition) extends SpecialStatementDefinition.Quantifier
case class UniqueExistenceDefinition(statementDefinition: StatementDefinition) extends SpecialStatementDefinition.Quantifier

class SpecialStatementDefinitionSymbolSerializer extends JsonSerializer[SpecialStatementDefinition] {
  override def serialize(value: SpecialStatementDefinition, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(value.statementDefinition.symbol)
  }
}
