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
        StatementVariableWithReplacement(this, Seq((variable, termToBeReplaced)))
      case _ =>
        throw new Exception("Cannot substitute a non-variable term into a statement variable")
    }
  }

  override def attemptSimplification(other: Statement): Option[DistinctVariables] = other match {
    case x: StatementVariableWithReplacement =>
      Some(DistinctVariables(x.variablesBeingReplaced.map(_ -> Variables(Seq(this), Nil)).toMap))
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
    replacements: Seq[(Term, TermVariable)])
  extends Statement {
  def replacingTerms = replacements.map(_._1)
  def variablesBeingReplaced = replacements.map(_._2)
  override def variables: Variables = replacements.foldLeft(Variables(Seq(statementVariable), Nil)) { case (variables, (term, termVariable)) =>
    variables ++ term.variables :+ termVariable
  }
  override def freeVariables: Seq[TermVariable] = replacements.foldRight(Seq.empty[TermVariable]) { case ((term, termVariable), freeVariables) =>
    freeVariables.filter(_ != termVariable).union(term.freeVariables)
  }
  override def calculateSubstitutions(otherStatement: Statement): Option[Substitutions] = {
    otherStatement match {
      case other @ StatementVariableWithReplacement(otherStatementVariable, otherReplacements) if otherReplacements.length == replacements.length =>
        val base = Substitutions(Map(statementVariable -> otherStatementVariable), variablesBeingReplaced.zip(other.variablesBeingReplaced).toMap)
        val substitutionAttempts = Some(base) +: replacingTerms.zip(other.replacingTerms).map { case (x, y) =>
            x.calculateSubstitutions(y)
        }
        Substitutions.mergeAttempts(substitutionAttempts)
      case _ =>
        Some(Substitutions(Map.empty, Map.empty))
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    replacements.foldLeft(statementVariable.applySubstitutions(substitutions)) { case (statement, (a, b)) =>
      statement.substituteFreeVariable(
        a.applySubstitutions(substitutions),
        Term.asVariable(b.applySubstitutions(substitutions)))
    }
  }

  def substituteFreeVariable(
    newTermToReplaceWith: Term,
    newTermToBeReplaced: TermVariable
  ): Statement = {
    if (newTermToReplaceWith == newTermToBeReplaced || variablesBeingReplaced.contains(newTermToBeReplaced)) {
      this
    } else {
      copy(replacements = (newTermToReplaceWith -> newTermToBeReplaced) +: replacements)
    }
  }

  override def attemptSimplification(other: Statement): Option[DistinctVariables] = other match {
    case x if x == this =>
      Some(DistinctVariables.empty)
    case `statementVariable` =>
      Some(DistinctVariables(variablesBeingReplaced.map(_ -> Variables(Seq(statementVariable), Nil)).toMap))
    case StatementVariableWithReplacement(`statementVariable`, otherReplacements) =>
      otherReplacements.omittedElements(replacements).map { omittedReplacements =>
        DistinctVariables(
          omittedReplacements.map(_._2 -> Variables(Seq(statementVariable), Nil)).toMap)
      }
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    val simplifiedReplacements = replacements.filter { case (_, termVariable) =>
      !distinctVariables.map.contains(termVariable) ||
        !distinctVariables.map(termVariable).statementVariables.contains(statementVariable)
    }
    if (simplifiedReplacements.isEmpty)
      statementVariable
    else
      copy(replacements = simplifiedReplacements)
  }

  override def html: String = {
    replacements.map { case (term, termVariable) =>
      s"[$term/$termVariable]"
    }.mkString("") + statementVariable.html
  }
}

object StatementVariableWithReplacement {
  def apply(
    statementVariable: StatementVariable,
    term: Term,
    termVariable: TermVariable
  ): StatementVariableWithReplacement = {
    StatementVariableWithReplacement(statementVariable, Seq(term -> termVariable))
  }
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
