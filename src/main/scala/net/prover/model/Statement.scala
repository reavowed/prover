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
  override def attemptMatch(otherStatement: Statement): Option[MatchWithSubstitutions] = {
    Some(MatchWithSubstitutions(Map(this -> otherStatement), Map.empty, Nil))
  }
  override def applyMatch(m: Match): Statement = {
    m.statements.getOrElse(this, throw new Exception(s"No replacement for statement variable $this"))
  }
  def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Statement = {
    if (termToReplaceWith == termToBeReplaced)
      this
    else
      StatementVariableWithReplacement(this, termToReplaceWith, termToBeReplaced)
  }
  override def html: String = (944 + i).toChar.toString
}

case class StatementVariableWithReplacement(
    statementVariable: StatementVariable,
    termToReplaceWith: TermVariable,
    termToBeReplaced: TermVariable)
  extends Statement {
  override def variables: Variables = Variables(Seq(statementVariable), Seq(termToReplaceWith, termToBeReplaced))
  override def freeVariables: Seq[TermVariable] = Seq(termToReplaceWith)
  override def attemptMatch(otherStatement: Statement): Option[MatchWithSubstitutions] = {
    otherStatement match {
      case StatementVariableWithReplacement(otherStatementVariable, otherTermToReplaceWith, otherTermToBeReplaced) =>
        Some(MatchWithSubstitutions(
          Map(statementVariable -> otherStatementVariable),
          Map(termToReplaceWith -> otherTermToReplaceWith, termToBeReplaced -> otherTermToBeReplaced),
          Nil))
      case _ =>
        Some(MatchWithSubstitutions(Map.empty, Map.empty, Seq((this, otherStatement))))
    }
  }
  override def applyMatch(m: Match): Statement = {
    statementVariable.applyMatch(m)
      .substituteTermVariables(
        Term.asVariable(termToReplaceWith.applyMatch(m)),
        Term.asVariable(termToBeReplaced.applyMatch(m)))
  }

  def substituteTermVariables(newTermToReplaceWith: TermVariable, newTermToBeReplaced: TermVariable): Statement = {
    if (newTermToBeReplaced == termToBeReplaced || newTermToReplaceWith == newTermToBeReplaced) {
      this
    } else {
      throw new Exception("Multiple term substitutions not currently supported")
    }
  }

  override def html: String = s"$statementVariable[$termToReplaceWith/$termToBeReplaced]"
}

case class ConnectiveStatement(substatements: Seq[Statement], connective: Connective) extends Statement {
  override def variables: Variables = substatements.map(_.variables).reduce(_ ++ _)
  override def freeVariables: Seq[TermVariable] = substatements.map(_.freeVariables).reduce(_ ++ _)
  override def attemptMatch(otherStatement: Statement): Option[MatchWithSubstitutions] = {
    otherStatement match {
      case ConnectiveStatement(otherSubstatements, `connective`) =>
        val matchAttempts = substatements.zip(otherSubstatements).map { case (substatement, otherSubstatement) =>
          substatement.attemptMatch(otherSubstatement)
        }
        MatchWithSubstitutions.mergeAttempts(matchAttempts)
      case _ =>
        None
    }
  }

  override def applyMatch(m: Match): Statement = {
    copy(substatements = substatements.map(_.applyMatch(m)))
  }

  def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Statement = {
    copy(substatements = substatements.map(_.substituteTermVariables(termToReplaceWith, termToBeReplaced)))
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

  override def attemptMatch(otherStatement: Statement): Option[MatchWithSubstitutions] = {
    otherStatement match {
      case QuantifierStatement(otherBoundVariable, otherSubstatement, `quantifier`) =>
        substatement.attemptMatch(otherSubstatement).flatMap { substatementMatch =>
          MatchWithSubstitutions.merge(Seq(substatementMatch, MatchWithSubstitutions(Map.empty, Map(boundVariable -> otherBoundVariable), Nil)))
        }
      case _ =>
        None
    }
  }
  override def applyMatch(m: Match): Statement = {
    copy(
      boundVariable = Term.asVariable(boundVariable.applyMatch(m)),
      substatement = substatement.applyMatch(m))
  }

  override def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Statement = {
    if (termToBeReplaced == boundVariable)
      this
    else if (termToReplaceWith == boundVariable)
      throw new Exception("Cannot replace free variable with bound variable in quantified statement")
    else
      copy(substatement = substatement.substituteTermVariables(termToReplaceWith, termToBeReplaced))
  }

  override def html: String = s"(${quantifier.symbol}${boundVariable.html})${substatement.safeHtml}"
}

case class PredicateStatement(terms: Seq[Term], predicate: Predicate) extends Statement {
  override def variables: Variables = terms.map(_.variables).reduce(_ ++ _)
  override def freeVariables: Seq[TermVariable] = terms.map(_.freeVariables).reduce(_ ++ _)
  override def attemptMatch(otherStatement: Statement): Option[MatchWithSubstitutions] = {
    otherStatement match {
      case PredicateStatement(otherTerms, `predicate`) =>
        val matchAttempts = terms.zip(otherTerms).map { case (term, otherTerm) =>
          term.attemptMatch(otherTerm)
        }
        MatchWithSubstitutions.mergeAttempts(matchAttempts)
      case _ =>
        None
    }
  }
  override def applyMatch(m: Match): Statement = {
    copy(terms = terms.map(_.applyMatch(m)))
  }
  def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Statement = {
    copy(terms = terms.map(_.substituteTermVariables(termToReplaceWith, termToBeReplaced)))
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
    object ConnectiveName {
      def unapply(s: String): Option[Connective] = {
        context.connectives.find(_.symbol == s)
      }
    }
    object QuantifierName {
      def unapply(s: String): Option[Quantifier] = {
        context.quantifiers.find(_.symbol == s)
      }
    }
    object PredicateName {
      def unapply(s: String): Option[Predicate] = {
        context.predicates.find(_.symbol == s)
      }
    }
    val (statementType, remainingLine) = line.splitFirstWord
    statementType match {
      case ConnectiveName(connective) =>
        connective.parseStatement(remainingLine, context)
      case QuantifierName(quantifier) =>
        quantifier.parseStatement(remainingLine, context)
      case PredicateName(predicate) =>
        predicate.parseStatement(remainingLine, context)
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
    if (line.nonEmpty) {
      Statement.parse(line, context).mapLeft(Some(_))
    } else {
      (None, line)
    }
  }
}
