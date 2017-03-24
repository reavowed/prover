package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil

trait Statement extends JsonSerializable.Base with Component {
  override val componentType = Statement
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def allBoundVariables: Seq[TermVariable]
  def containsTerms: Boolean
  def applySubstitutions(substitutions: Substitutions): Statement
}

case class StatementVariable(text: String) extends Statement {
  override def variables: Variables = Variables(Seq(this), Nil)
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Option[Substitutions] = other match {
    case otherStatement: Statement =>
      substitutions.statements.get(this) match {
        case Some(`otherStatement`) =>
          Some(substitutions)
        case Some(_) =>
          None
        case None =>
          Some(substitutions + (this, otherStatement))
      }
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    substitutions.statements.getOrElse(this, throw new Exception(s"No replacement for statement variable $this"))
  }

  override def allBoundVariables = Nil

  override def containsTerms = false

  override def html: String = text

  override def serialized: String = text
}

case class DefinedStatement(
    subcomponents: Seq[Component],
    boundVariables: Seq[TermVariable],
    definition: StatementDefinition)
 extends Statement
{
  override def variables: Variables = subcomponents.map(_.variables).foldLeft(Variables.empty)(_ ++ _)
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Option[Substitutions] = other match {
    case DefinedStatement(otherSubcomponents, _, `definition`) =>
      subcomponents.zip(otherSubcomponents)
        .foldLeft(Option(substitutions)) { case (substitutionsSoFarOption, (component, otherComponent)) =>
          substitutionsSoFarOption.flatMap(component.calculateSubstitutions(otherComponent, _))
        }
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    copy(
      subcomponents = subcomponents.map(_.applySubstitutions(substitutions)),
      boundVariables = boundVariables.map(_.applySubstitutions(substitutions)).map(Term.asVariable))
  }
  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")
  override def containsTerms: Boolean = subcomponents.exists {
    case s: Statement =>
      s.containsTerms
    case _: Term =>
      true
  }

  override def allBoundVariables = boundVariables ++ subcomponents.ofType[Statement].flatMap(_.allBoundVariables)
}

object Statement extends ComponentType {

  def parser(implicit context: Context): Parser[Statement] = {
    object ParsableStatement {
      def unapply(s: String): Option[StatementDefinition] = {
        context.statementDefinitions.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[StatementVariable] = {
        context.variables.statementVariables.find(_.text == s)
      }
    }

    def parserForStatementType(statementType: String): Parser[Statement] = statementType match {
      case ParsableStatement(statementDefinition) =>
        statementDefinition.statementParser
      case SpecifiedVariable(v) =>
        Parser.constant(v)
      case _ =>
        throw new Exception(s"Unrecognised statement type $statementType")
    }

    Parser.singleWord.flatMap(parserForStatementType)
  }

  def listParser(implicit context: Context): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: Context): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }
}
