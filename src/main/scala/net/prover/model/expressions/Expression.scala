package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.proof.SubstitutionContext
import net.prover.model.{ExpressionParsingContext, Parser, UsedVariables}

@JsonSerialize(using = classOf[ExpressionSerializer])
trait Expression extends TypedExpression[Expression]

trait TypedExpression[+ExpressionType <: Expression] {
  def definitionUsages: DefinitionUsages
  def referencedDefinitions: Set[CompoundExpressionDefinition] = definitionUsages.map.keySet

  def usedVariables: UsedVariables
  def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, ExpressionType, Int, Seq[Int])]
  def getTerms()(implicit substitutionContext: SubstitutionContext): Seq[(Term, ExpressionType, Int, Seq[Int])] = getTerms(0, substitutionContext.externalDepth)
  def getPredicateForTerm(term: Term, depth: Int): ExpressionType
  def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): ExpressionType

  def renameBoundVariable(newName: String, index: Int, path: Seq[Int]): Option[ExpressionType] = None
  def findComponentPath(other: Expression): Option[Seq[Int]] = {
    if (this == other) {
      Some(Nil)
    } else {
      None
    }
  }
  def safeToString: String = toString
  def serialized: String
  def serializedForHash: String
}

object Expression {
  def parser(implicit parsingContext: ExpressionParsingContext): Parser[Expression] = {
    Statement.parser.tryOrElse(Term.parser)
  }

  implicit class ExpressionSeqOps(expressions: Seq[Expression]) {
    def usedVariables: UsedVariables = {
      expressions.map(_.usedVariables).foldTogether
    }
  }
}

sealed trait ExpressionType[T <: Expression]
object ExpressionType {
  implicit object StatementType extends ExpressionType[Statement]
  implicit object TermType extends ExpressionType[Term]
}

private class ExpressionSerializer extends JsonSerializer[Expression] {
  override def serialize(value: Expression, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(value.serialized)
  }
}
