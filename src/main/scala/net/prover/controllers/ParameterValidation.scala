package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.Qualifier

import scala.util.{Failure, Success, Try}

trait ParameterValidation {

  def getOptionalString(source: String): Option[String] = {
    Option(source.trim()).filter(_.nonEmpty)
  }
  def getOptionalSingleWord(source: String, name: String): Try[Option[String]] = {
    getOptionalString(source) match {
      case Some(word) if word.splitByWhitespace().length == 1 =>
        Success(Some(word))
      case Some(_) =>
        Failure(BadRequestException(s"$name must be a single word"))
      case None =>
        Success(None)
    }
  }

  def getWords(source: String): Seq[String] = {
    source.splitByWhitespace().flatMap(getOptionalString)
  }
  def getMandatoryString(source: String, name: String): Try[String] = {
    getOptionalString(source).orBadRequest(s"$name must be given")
  }
  def getFormat(source: String, symbol: String, boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Try[Format.Basic] = {
    getOptionalString(source)
      .map(f => Format.parserForExpressionDefinition(symbol, boundVariableNames, componentTypes).parseFromString(f, "format"))
      .getOrElse(Format.default(boundVariableNames, componentTypes))
      .recoverWithBadRequest
  }
  def getQualifier(termNamesText: String, serializedFormat: String): Try[Qualifier] = {
    val termNames = getWords(termNamesText)
    for {
      _ <- termNames.nonEmpty.orBadRequest("At least one term name must be provided")
      format <- Format.parser(termNames).parseFromString(serializedFormat, "format").recoverWithBadRequest
    } yield Qualifier(termNames, format)
  }
  def getOptionalQualifier(termNamesText: String, serializedFormat: String): Try[Option[Qualifier]] = {
    (getWords(termNamesText), getOptionalString(serializedFormat)) match {
      case (otherTermNames, Some(serializedFormat)) if otherTermNames.nonEmpty =>
        for {
          format <- Format.parser(otherTermNames).parseFromString(serializedFormat, "format").recoverWithBadRequest
        } yield Some(Qualifier(otherTermNames, format))
      case (Nil, None) =>
        Success(None)
      case _ =>
        Failure(BadRequestException("Both format and term names must be provided for qualifier"))
    }
  }
}
