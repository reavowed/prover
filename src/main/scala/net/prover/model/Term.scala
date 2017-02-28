package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

trait Term extends JsonSerializable.Base with Component {
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

case class TermVariable(text: String) extends Term {
  override def variables: Variables = Variables(Nil, Seq(this))
  override def freeVariables: Seq[TermVariable] = Seq(this)
  override def calculateSubstitutions(other: Component): Option[Substitutions] = {
    other match {
      case otherTerm: Term =>
        Some(Substitutions(Map.empty, Map(this -> otherTerm)))
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    substitutions.terms.getOrElse(this, {
      throw new Exception(s"No replacement for term variable $this")
    })
  }
  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Term = {
    if (this == termToBeReplaced)
      termToReplaceWith
    else
      this
  }

  override def attemptSimplification(other: Component): Option[DistinctVariables] = {
    if (other == this)
      Some(DistinctVariables.empty)
    else
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Term = {
    this
  }

  override def html: String = text
}

case class DefinedTerm(
    subcomponents: Seq[Component],
    termSpecification: TermSpecification)
  extends Term
{
  override def variables: Variables = subcomponents.map(_.variables).foldLeft(Variables.empty)(_ ++ _)
  override def freeVariables: Seq[TermVariable] = subcomponents.map(_.freeVariables).foldLeft(Seq.empty[TermVariable])(_ ++ _)
  override def calculateSubstitutions(other: Component): Option[Substitutions] = other match {
    case DefinedTerm(otherSubcomponents, `termSpecification`) =>
      val substitutionAttempts = subcomponents.zip(otherSubcomponents).map { case (component, otherComponent) =>
        component.calculateSubstitutions(otherComponent)
      }
      Substitutions.mergeAttempts(substitutionAttempts)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    copy(subcomponents = subcomponents.map(_.applySubstitutions(substitutions)))
  }
  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Term = {
    copy(subcomponents = subcomponents.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced)))
  }
  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case DefinedTerm(otherSubcomponents, `termSpecification`) =>
      subcomponents.zip(otherSubcomponents).map { case (component, otherComponent) =>
        component.attemptSimplification(otherComponent)
      }.traverseOption.map(_.foldLeft(DistinctVariables.empty)(_ ++ _))
    case _ =>
      None
  }
  override def makeSimplifications(distinctVariables: DistinctVariables): Term = {
    copy(subcomponents = subcomponents.map(_.makeSimplifications(distinctVariables)))
  }
  override def html: String = {
    termSpecification.format.html(subcomponents)
  }
  override def safeHtml: String = {
    termSpecification.format.safeHtml(subcomponents)
  }

  override def equals(obj: Any): Boolean = obj match {
    case DefinedTerm(`subcomponents`, otherTermDefinition) if otherTermDefinition.symbol == termSpecification.symbol =>
      true
    case _ =>
      false
  }

  override def hashCode(): Int = {
    subcomponents.hashCode * 41 + termSpecification.symbol.hashCode
  }
}

object Term extends ComponentType {
  def asVariable(term: Term): TermVariable = {
    term match {
      case v: TermVariable =>
        v
      case x =>
        throw new Exception(s"Expected term variable, got $x")
    }
  }

  def parser(context: Context): Parser[Term] = {
    object TermSpecificationMatcher {
      def unapply(s: String): Option[TermSpecification] = {
        context.termSpecifications.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[TermVariable] = {
        context.variables.termVariables.find(_.text == s)
      }
    }
    val primeVariableRegex = "(\\d)'".r
    def parserForTermType(termType: String): Parser[Term] = {
      termType match {
        case TermSpecificationMatcher(termSpecification) =>
          termSpecification.termParser(context)
        case SpecifiedVariable(variable) =>
          Parser.constant(variable)
        case IntParser(i) =>
          Parser.constant(TermVariable((123 - i).toChar.toString))
        case primeVariableRegex(IntParser(i)) =>
          Parser.constant(TermVariable((123 - i).toChar.toString + "'"))
        case _ =>
          throw new Exception(s"Unrecognised term type '$termType'")
      }
    }
    Parser.singleWord.flatMap(parserForTermType)
  }

  def listParser(context: Context): Parser[Seq[Term]] = {
    parser(context).listInParens(Some(","))
  }

  def variableParser(context: Context): Parser[TermVariable] = parser(context).map(asVariable)

  def variableListParser(context: Context): Parser[Seq[TermVariable]] = {
    variableParser(context).listInParens(None)
  }
}
