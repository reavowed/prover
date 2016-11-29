package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

trait Statement extends JsonSerializable.Base {
  def atoms: Seq[Int]
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
  override def atoms = Seq(i)
  override def attemptMatch(otherStatement: Statement): Option[Map[Int, Statement]] = {
    Some(Map(i -> otherStatement))
  }
  override def replace(map: Map[Int, Statement]): Statement = {
    map.getOrElse(i, throw new Exception(s"No replacement for atom $i"))
  }
  override def html = (944 + i).toChar.toString
}

case class ConnectiveStatement(substatements: Seq[Statement], connective: Connective) extends Statement {
  override def atoms = substatements.flatMap(_.atoms).distinct
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

  def parse(line: PartialLine, context: Context): (Statement, PartialLine) = {
    object ConnectiveName {
      def unapply(s: String): Option[Connective] = {
        context.connectives.find(_.name == s)
      }
    }
    val (statementType, remainingLine) = line.splitFirstWord
    statementType match {
      case IntParser(i) =>
        (Atom(i), remainingLine)
      case ConnectiveName(connective) =>
        connective.parseStatement(remainingLine, context)
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
}
