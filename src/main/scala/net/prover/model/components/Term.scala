package net.prover.model.components

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}
import net.prover.model.entries.TermDefinition
import net.prover.model.{Context, DistinctVariables, Parser, Substitutions}

trait Term extends JsonSerializable.Base with Component {
  override val componentType = Term
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def applySubstitutions(substitutions: Substitutions): Option[Term]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Term]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Term, DistinctVariables)]
  def replacePlaceholder(other: Component): Option[Term]
}

object Term extends ComponentType {
  def asVariable(component: Component): TermVariable = {
    optionAsVariable(component).getOrElse(throw new Exception(s"Expected term variable, got $component"))
  }

  def optionAsVariable(component: Component): Option[TermVariable] = {
    component match {
      case v: TermVariable =>
        Some(v)
      case _ =>
        None
    }
  }

  def findVariable(name: String)(implicit context: Context): Option[TermVariable] = {
    def findDirectly(name: String): Option[TermVariable] = {
      context.termVariableNames.find(_ == name).map(TermVariable)
    }
    def findPrime(name: String): Option[TermVariable] = {
      context.termVariableNames.find(_ + "'" == name).map(_ => TermVariable(name))
    }
    def findWithSuffix(name: String): Option[TermVariable] = {
      val index = name.indexOf('_')
      if (index >= 0) {
        val prefix = name.substring(0, index)
        context.termVariableNames.find(_ == prefix).map(_ => TermVariable(name))
      } else {
        None
      }
    }
    findDirectly(name) orElse findPrime(name) orElse findWithSuffix(name)
  }

  def parser(implicit context: Context): Parser[Term] = {
    object TermDefinitionMatcher {
      def unapply(s: String): Option[TermDefinition] = {
        context.termDefinitions.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[TermVariable] = {
        findVariable(s)
      }
    }
    def parserForTermType(termType: String): Parser[Term] = {
      termType match {
        case TermDefinitionMatcher(termDefinition) =>
          termDefinition.termParser
        case SpecifiedVariable(variable) =>
          Parser.constant(variable)
        case "sub" =>
          for {
            termToReplaceWith <- parser
            termToBeReplaced <- variableParser
            term <- parser
          } yield {
            term.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, DistinctVariables.empty)
              .getOrElse(throw new Exception("Invalid substitution"))
          }
        case "_" =>
          Parser.constant(PlaceholderTerm)
        case _ =>
          throw new Exception(s"Unrecognised term type '$termType'")
      }
    }
    Parser.singleWord.flatMap(parserForTermType)
  }

  def listParser(implicit context: Context): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: Context): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: Context): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }
}
