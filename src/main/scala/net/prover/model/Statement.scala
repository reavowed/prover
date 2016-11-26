package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

trait Statement extends JsonSerializable.Base {
  def attemptMatch(otherStatement: Statement): Option[Map[Int, Statement]]
  def replace(map: Map[Int, Statement]): Statement
  def html: String
  def safeHtml: String = html
  override def toString: String = html

  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

case class Atom(i: Int) extends Statement {
  override def attemptMatch(otherStatement: Statement): Option[Map[Int, Statement]] = {
    Some(Map(i -> otherStatement))
  }
  override def replace(map: Map[Int, Statement]): Statement = {
    map.getOrElse(i, throw new Exception(s"No replacement for atom $i"))
  }
  override def html = (944 + i).toChar.toString
}

case class ConnectiveStatement(substatements: Seq[Statement], connective: Connective) extends Statement {
  override def attemptMatch(otherStatement: Statement): Option[Map[Int, Statement]] = {
    otherStatement match {
      case ConnectiveStatement(otherSubstatements, `connective`) =>
        Statement.mergeMatchAttempts(substatements.zip(otherSubstatements).map { case (substatement, otherSubstatement) =>
          substatement.attemptMatch(otherSubstatement)
        })
      case _ =>
        None
    }
  }

  override def replace(map: Map[Int, Statement]): Statement = {
    ConnectiveStatement(substatements.map(_.replace(map)), connective)
  }

  def html = substatements match {
    case Seq(substatement) =>
      connective.symbol + substatement.safeHtml
    case _ =>
      substatements.map(_.safeHtml).mkString(" " + connective.symbol + " ")
  }

  override def safeHtml = if (substatements.length == 1) html else "(" + html + ")"
}

object Statement {

  def mergeMatchAttempts(matches: Seq[Option[Map[Int, Statement]]]): Option[Map[Int, Statement]] = {
    matches.traverseOption.flatMap(mergeMatches)
  }

  def mergeMatches(matches: Seq[Map[Int, Statement]]): Option[Map[Int, Statement]] = {
    val keys = matches.map(_.keySet).fold(Set.empty)(_ ++ _).toSeq
    keys.map(i => matches.flatMap(_.get(i)).distinct match {
      case Seq(statement) => Some(i -> statement)
      case _ => None
    }).traverseOption.map(_.toMap)
  }

  def parse(s: String, connectiveDefinitions: Seq[Connective]): (Statement, String) = {
    s match {
      case WordAndRemainingText(IntParser(i), restOfLine) =>
        (Atom(i), restOfLine)
      case WordAndRemainingText(connectiveName, substatements) =>
        val connective = connectiveDefinitions
          .find(_.name == connectiveName)
          .getOrElse(throw new Exception(s"Unrecognised statement connective '$connectiveName'"))
        connective.parseStatement(substatements, connectiveDefinitions)
      case _ =>
        throw new Exception("Could not read statement\n" + s)
    }
  }

  def parseList(
    text: String,
    statementDefinitions: Seq[Connective],
    statementsSoFar: Seq[Statement] = Nil
  ): (Seq[Statement], String) = {
    val (statement, textAfterStatement) = parse(text, statementDefinitions)
    textAfterStatement.splitByWhitespace(2) match {
      case Seq("&", remainingText) =>
        parseList(remainingText, statementDefinitions, statementsSoFar :+ statement)
      case _ =>
        (statementsSoFar :+ statement, textAfterStatement)
    }
  }
}
