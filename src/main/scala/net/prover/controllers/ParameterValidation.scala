package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model._

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
  def getFormat(source: String, symbol: String, componentNames: Seq[String]): Try[Format] = {
    getOptionalString(source)
      .map(f => Format.parser(componentNames).parseFromString(f, "format"))
      .getOrElse(Format.default(componentNames))
      .recoverWithBadRequest
  }
}
