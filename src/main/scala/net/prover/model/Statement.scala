package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil

trait Statement extends JsonSerializable.Base with Component[Statement] {
  def safeHtml: String = html
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

case class StatementVariable(i: Int) extends Statement {
  override def variables: Variables = Variables(Seq(this), Nil)
  override def freeVariables: Seq[TermVariable] = Nil
  override def calculateSubstitutions(otherStatement: Statement): Option[Substitutions] = {
    Some(Substitutions(Map(this -> otherStatement), Map.empty))
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    substitutions.statements.getOrElse(this, throw new Exception(s"No replacement for statement variable $this"))
  }
  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    if (termToReplaceWith == termToBeReplaced)
      this
    else termToReplaceWith match {
      case variable: TermVariable =>
        StatementVariableWithReplacement(this, variable, termToBeReplaced)
      case _ =>
        throw new Exception("Cannot substitute a non-variable term into a statement variable")
    }
  }

  override def attemptSimplification(other: Statement): Option[DistinctVariables] = other match {
    case StatementVariableWithReplacement(x, _, termToBeReplaced) if x == this =>
      Some(DistinctVariables(Map(termToBeReplaced -> Variables(Seq(this), Nil))))
    case x if x == this =>
      Some(DistinctVariables.empty)
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = this

  override def html: String = (944 + i).toChar.toString
}

case class StatementVariableWithReplacement(
    statementVariable: StatementVariable,
    termToReplaceWith: TermVariable,
    termToBeReplaced: TermVariable)
  extends Statement {
  override def variables: Variables = Variables(Seq(statementVariable), Seq(termToReplaceWith, termToBeReplaced))
  override def freeVariables: Seq[TermVariable] = Seq(termToReplaceWith)
  override def calculateSubstitutions(otherStatement: Statement): Option[Substitutions] = {
    otherStatement match {
      case StatementVariableWithReplacement(otherStatementVariable, otherTermToReplaceWith, otherTermToBeReplaced) =>
        Some(Substitutions(
          Map(statementVariable -> otherStatementVariable),
          Map(termToReplaceWith -> otherTermToReplaceWith, termToBeReplaced -> otherTermToBeReplaced)))
      case _ =>
        Some(Substitutions(Map.empty, Map.empty))
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    statementVariable.applySubstitutions(substitutions)
      .substituteFreeVariable(
        termToReplaceWith.applySubstitutions(substitutions),
        Term.asVariable(termToBeReplaced.applySubstitutions(substitutions)))
  }

  def substituteFreeVariable(
    newTermToReplaceWith: Term,
    newTermToBeReplaced: TermVariable
  ): Statement = {
    if (newTermToBeReplaced == termToBeReplaced || newTermToReplaceWith == newTermToBeReplaced) {
      this
    } else {
      throw new Exception("Multiple term substitutions not currently supported")
    }
  }

  override def attemptSimplification(other: Statement): Option[DistinctVariables] = {
    if (other == this)
      Some(DistinctVariables.empty)
    else if (other == statementVariable)
      Some(DistinctVariables(Map(termToBeReplaced -> Variables(Seq(statementVariable), Nil))))
    else
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    if (distinctVariables.map.contains(termToBeReplaced) &&
      distinctVariables.map(termToBeReplaced).statementVariables.contains(statementVariable)
    )
      statementVariable
    else
      this
  }

  override def html: String = s"$statementVariable[$termToReplaceWith/$termToBeReplaced]"
}

case class ConnectiveStatement(substatements: Seq[Statement], connective: Connective) extends Statement {
  override def variables: Variables = substatements.map(_.variables).reduce(_ ++ _)
  override def freeVariables: Seq[TermVariable] = substatements.map(_.freeVariables).reduce(_ ++ _)
  override def calculateSubstitutions(otherStatement: Statement): Option[Substitutions] = {
    otherStatement match {
      case ConnectiveStatement(otherSubstatements, `connective`) =>
        val substitutionAttempts = substatements.zip(otherSubstatements).map { case (substatement, otherSubstatement) =>
          substatement.calculateSubstitutions(otherSubstatement)
        }
        Substitutions.mergeAttempts(substitutionAttempts)
      case _ =>
        None
    }
  }

  override def applySubstitutions(substitutions: Substitutions): Statement = {
    copy(substatements = substatements.map(_.applySubstitutions(substitutions)))
  }

  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    copy(substatements = substatements.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced)))
  }

  override def attemptSimplification(other: Statement): Option[DistinctVariables] = other match {
    case ConnectiveStatement(otherSubstatements, `connective`) =>
      substatements.zip(otherSubstatements).map { case (substatement, otherSubstatement) =>
        substatement.attemptSimplification(otherSubstatement)
      }.traverseOption.map(_.reduce(_ ++ _))
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    copy(substatements.map(_.makeSimplifications(distinctVariables)))
  }

  def html: String = substatements match {
    case Seq(substatement) =>
      connective.symbol + substatement.safeHtml
    case _ =>
      substatements.map(_.safeHtml).mkString(" " + connective.symbol + " ")
  }

  override def safeHtml: String = if (substatements.length == 1) html else "(" + html + ")"
}

case class QuantifierStatement(boundVariable: TermVariable, substatement: Statement, quantifier: Quantifier) extends Statement {
  override def variables: Variables = boundVariable +: substatement.variables
  override def freeVariables: Seq[TermVariable] = substatement.freeVariables.filter(_ != boundVariable)

  override def calculateSubstitutions(otherStatement: Statement): Option[Substitutions] = {
    otherStatement match {
      case QuantifierStatement(otherBoundVariable, otherSubstatement, `quantifier`) =>
        substatement.calculateSubstitutions(otherSubstatement).flatMap { substatementMatch =>
          Substitutions.merge(Seq(
            substatementMatch,
            Substitutions(Map.empty, Map(boundVariable -> otherBoundVariable))))
        }
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    copy(
      boundVariable = Term.asVariable(boundVariable.applySubstitutions(substitutions)),
      substatement = substatement.applySubstitutions(substitutions))
  }

  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    if (termToBeReplaced == boundVariable)
      this
    else if (termToReplaceWith.freeVariables.contains(boundVariable))
      throw new Exception("Cannot replace free variable with bound variable in quantified statement")
    else
      copy(substatement = substatement.substituteFreeVariable(
        termToReplaceWith,
        termToBeReplaced))
  }

  override def attemptSimplification(other: Statement): Option[DistinctVariables] = other match {
    case QuantifierStatement(`boundVariable`, otherSubstatement, `quantifier`) =>
      substatement.attemptSimplification(otherSubstatement)
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    copy(substatement = substatement.makeSimplifications(distinctVariables))
  }

  override def html: String = s"(${quantifier.symbol}${boundVariable.html})${substatement.safeHtml}"
}

case class PredicateStatement(terms: Seq[Term], predicate: Predicate) extends Statement {
  override def variables: Variables = terms.map(_.variables).reduce(_ ++ _)
  override def freeVariables: Seq[TermVariable] = terms.map(_.freeVariables).reduce(_ ++ _)
  override def calculateSubstitutions(otherStatement: Statement): Option[Substitutions] = {
    otherStatement match {
      case PredicateStatement(otherTerms, `predicate`) =>
        val substitutionAttempts = terms.zip(otherTerms).map { case (term, otherTerm) =>
          term.calculateSubstitutions(otherTerm)
        }
        Substitutions.mergeAttempts(substitutionAttempts)
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    copy(terms = terms.map(_.applySubstitutions(substitutions)))
  }
  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    copy(terms = terms.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced)))
  }
  override def attemptSimplification(other: Statement): Option[DistinctVariables] = other match {
    case PredicateStatement(otherTerms, `predicate`) =>
      terms.zip(otherTerms).map { case (term, otherTerm) =>
        term.attemptSimplification(otherTerm)
      }.traverseOption.map(_.reduce(_ ++ _))
    case _ =>
      None
  }
  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    copy(terms = terms.map(_.makeSimplifications(distinctVariables)))
  }
  def html: String = terms.map(_.html).mkString(" " + predicate.symbol + " ")
  override def safeHtml: String = if (terms.length > 1) "(" + html + ")" else html
}

object Statement extends ComponentType[Statement] {
  def parseStatementVariable(line: PartialLine, context: Context): (StatementVariable, PartialLine) = {
    parse(line, context) match {
      case (v: StatementVariable, remainingLine) =>
        (v, remainingLine)
      case (x, _) =>
        throw ParseException.withMessage(s"Expected statement variable, got $x", line.fullLine)
    }
  }

  def parse(line: PartialLine, context: Context): (Statement, PartialLine) = {
    object ParsableStatement {
      def unapply(s: String): Option[StatementDefinition] = {
        context.statementDefinitions.find(_.symbol == s)
      }
    }
    val (statementType, remainingLine) = line.splitFirstWord
    statementType match {
      case ParsableStatement(statementDefinition) =>
        statementDefinition.parseStatement(remainingLine, context)
      case IntParser(i) =>
        (StatementVariable(i), remainingLine)
      case "sub" =>
        val (termToReplaceWith, lineAfterFirstTerm) = Term.parse(remainingLine, context).mapLeft(Term.asVariable)
        val (termToBeReplaced, lineAfterSecondTerm) = Term.parse(lineAfterFirstTerm, context).mapLeft(Term.asVariable)
        val (statementVariable, lineAfterStatement) = parseStatementVariable(lineAfterSecondTerm, context)
        (StatementVariableWithReplacement(statementVariable, termToReplaceWith, termToBeReplaced), lineAfterStatement)
      case _ =>
        throw ParseException.withMessage(s"Unrecognised statement type $statementType", line.fullLine)
    }
  }

  def parseList(
    line: PartialLine,
    context: Context,
    statementsSoFar: Seq[Statement] = Nil
  ): (Seq[Statement], PartialLine) = {
    val (statement, lineAfterStatement) = parse(line, context)
    lineAfterStatement match {
      case WordAndRemainingText("&", remainingText) =>
        parseList(remainingText, context, statementsSoFar :+ statement)
      case _ =>
        (statementsSoFar :+ statement, lineAfterStatement)
    }
  }

  def parseOptional(line: PartialLine, context: Context): (Option[Statement], PartialLine) = {
    if (line.nonEmpty && line.remainingText.head != ')') {
      Statement.parse(line, context).mapLeft(Some(_))
    } else {
      (None, line)
    }
  }
}
