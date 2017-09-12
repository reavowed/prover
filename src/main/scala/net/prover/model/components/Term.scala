package net.prover.model.components

import net.prover.model.entries.TermDefinition
import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Term extends Component {
  override val componentType = Term
  def applySubstitutions(substitutions: Substitutions): Option[Term]
  def replacePlaceholder(other: Component): Term
  def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int): Seq[(Function, Substitutions)] = {
    argument.calculateSubstitutions(this, substitutions, boundVariableCount).map(Function.Identity -> _)
  }
  def makeApplicative(argument: Term): Option[Term] = None
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

  def parser(implicit context: ParsingContext): Parser[Term] = {
    object TermDefinitionMatcher {
      def unapply(s: String): Option[TermDefinition] = {
        context.termDefinitions.find(_.symbol == s)
      }
    }

    Parser.selectWordParser("term") {
      case "_" =>
        Parser.constant(PlaceholderTerm)
      case context.RecognisedBoundVariable(variable) =>
        Parser.constant(variable)
      case TermDefinitionMatcher(termDefinition) =>
        termDefinition.termParser
      case context.RecognisedTermVariable(variable) =>
        Parser.constant(variable)
    }
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: ParsingContext): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: ParsingContext): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }

  def applicativeParser(implicit context: ParsingContext) = Function.parser
}
