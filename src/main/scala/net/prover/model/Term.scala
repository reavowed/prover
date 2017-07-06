package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil

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

case class TermVariable(text: String) extends Term with Variable {
  override def allVariables: Set[Variable] = Set(this)
  override def presentVariables: Set[Variable] = Set(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set(this))
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  override def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = Set(this)
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = {
    other match {
      case otherTerm: Term =>
        substitutions.tryAdd(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Term])
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Term] = {
    if (termToBeReplaced == this)
      Some(termToReplaceWith)
    else if (termToBeReplaced == termToReplaceWith)
      Some(this)
    else if (distinctVariables.areDistinct(this, termToBeReplaced))
      Some(this)
    else
      Some(SubstitutedTermVariable(this, termToReplaceWith, termToBeReplaced))
  }
  override def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    if (target == this) {
      if (termToBeReplaced == termToReplaceWith)
        Some(DistinctVariables.empty)
      else
        Some(DistinctVariables(termToBeReplaced -> this))
    } else if (makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables).contains(target)) {
      Some(DistinctVariables.empty)
    } else {
      None
    }
  }
  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else if (this == other && this != termVariable) {
      Some((this, DistinctVariables(termVariable -> this)))
    } else {
      None
    }
  }
  override def findSubstitution(
    target: Component,
    termVariableToBeReplaced: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == termVariableToBeReplaced) {
      (Seq((target.asInstanceOf[Term], DistinctVariables.empty)), None)
    } else if (this == target) {
      (Seq((termVariableToBeReplaced, DistinctVariables.empty)), Some(DistinctVariables(termVariableToBeReplaced -> this)))
    }  else {
      target match {
        case SubstitutedTermVariable(variable, termToReplaceWith, `termVariableToBeReplaced`) if variable == this =>
          (Seq((termToReplaceWith, DistinctVariables.empty)), None)
        case _ =>
          (Nil, None)
      }
    }
  }
  override def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (firstTermVariable == firstTerm) {
      this.findSubstitution(target, secondTermVariable)
    } else if (this == firstTermVariable) {
      firstTerm.findSubstitution(target, secondTermVariable)
    } else {
      val (termsStraight, noTermStraight) = this.findSubstitution(target, secondTermVariable)
      val (termsSubbed, noTermSubbed) = SubstitutedTermVariable(this, firstTerm, firstTermVariable).findSubstitution(target, secondTermVariable)
      val jointTerms = termsStraight.map(_.mapRight(_ ++ DistinctVariables(firstTermVariable -> this))) ++ termsSubbed
      (jointTerms, noTermStraight.map(_ ++ DistinctVariables(firstTermVariable -> this)) orElse noTermSubbed)
    }
  }
  override def condenseWithSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    def tryWith(thisTarget: Term, additionalDistinctVariables: DistinctVariables): Option[(PartialSubstitutions, PartialSubstitutions)] = {
      other.calculateSubstitutions(thisTarget, otherSubstitutions)
        .map(thisSubstitutions.withDistinctVariables(additionalDistinctVariables) -> _)
        .headOption
    }
    super.condenseWithSubstitution(termToReplaceWith, termToBeReplaced, other, thisSubstitutions, otherSubstitutions) orElse
      (if (termToBeReplaced == termToReplaceWith) {
        tryWith(this, DistinctVariables.empty)
      } else if (this == termToBeReplaced) {
        tryWith(termToReplaceWith, DistinctVariables.empty)
      } else {
        tryWith(this, DistinctVariables(termToBeReplaced -> this)) orElse
          tryWith(SubstitutedTermVariable(this, termToReplaceWith, termToBeReplaced), DistinctVariables.empty)
      })
  }
  override def replacePlaceholder(other: Component) = Some(this)
  override def html: String = Html.format(text)
  override def serialized: String = text
}

case class SubstitutedTermVariable(
  variable: TermVariable,
  termToReplaceWith: Term,
  termToBeReplaced: TermVariable)
  extends Term with SubstitutedVariable[Term, TermVariable]
{
  override def update(
    updatedVariable: TermVariable,
    updatedTermToReplaceWith: Term,
    updatedTermToBeReplaced: TermVariable
  ): SubstitutedTermVariable = {
    SubstitutedTermVariable(updatedVariable, updatedTermToReplaceWith, updatedTermToBeReplaced)
  }
}

case class DefinedTerm(
    subcomponents: Seq[Component],
    localBoundVariables: Set[TermVariable],
    definition: TermDefinition)
  extends Term with DefinedComponent[Term]
{
  override def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])] = other match {
    case DefinedTerm(otherSubcomponents, otherBoundVariables, `definition`) =>
      Some((otherSubcomponents, otherBoundVariables))
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): Term = {
    copy(subcomponents = newSubcomponents, localBoundVariables = newBoundVariables)
  }

  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else {
      super.resolveSingleSubstitution(other, termVariable, thisTerm, otherTerm)
    }
  }

  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")
}

object PlaceholderTerm extends Term with Placeholder[Term] {
  def replacePlaceholder(other: Component) = {
    Some(other.asInstanceOf[Term])
  }
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
