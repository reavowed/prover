package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{Qualifier, TermListAdapter}
import net.prover.model.entries.{PropertyDefinitionOnType, TypeDefinition, TypeQualifierDefinition}
import net.prover.model.expressions.{Statement, Term}

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
  def getOptionalParentQualifier(parentType: TypeDefinition, qualifierSymbolText: String)(implicit entryContext: EntryContext): Try[Option[TypeQualifierDefinition]] = {
    getOptionalString(qualifierSymbolText)
      .map(qualifierSymbol => entryContext.qualifiersByType(parentType.symbol).find(_.symbol == qualifierSymbol).orBadRequest(s"Unknown qualifier '$qualifierSymbol' on type '${parentType.symbol}'"))
      .swap
  }
  def getParentObjects(parentType: TypeDefinition, objectSymbolsText: String)(implicit entryContext: EntryContext): Try[Option[PropertyDefinitionOnType.RequiredParentObjects]] = {
    getWords(objectSymbolsText) match {
      case Nil =>
        Success(None)
      case objectSymbols =>
        for {
          objectDefinitions <- objectSymbols.map(s => entryContext.relatedObjectsByType(parentType.symbol).find(_.symbol == s)
            .orBadRequest(s"Unknown object '$s' on type '${parentType.symbol}'"))
            .traverseTry
          uniquenessDefinition <- entryContext.uniquenessDefinitionOption.orBadRequest("Cannot add related objects to property definition without uniqueness")
          generalizationDefinition <- entryContext.generalizationDefinitionOption.orBadRequest("Cannot add related objects to property definition without generalization")
          deductionDefinition <- entryContext.deductionDefinitionOption.orBadRequest("Cannot add related objects to property definition without deduction")
        } yield Some(PropertyDefinitionOnType.RequiredParentObjects(objectDefinitions, uniquenessDefinition, generalizationDefinition, deductionDefinition))
    }
  }
  def getOptionalAdapter(termNamesText: String, possibleSerializedTemplates: String, qualifierTermNames: Seq[String])(implicit entryContext: EntryContext): Try[Option[TermListAdapter]] = {
    getOptionalString(possibleSerializedTemplates) match {
      case Some(serializedTemplates) =>
        val termNames = getWords(termNamesText)
        implicit val epc = ExpressionParsingContext.outsideProof(entryContext, Nil).addInitialParameters(termNames)
        for {
          templates <- qualifierTermNames.indices.map(_ => Term.parser).traverse.parseFromString(serializedTemplates, "templates").recoverWithBadRequest
        } yield Some(TermListAdapter(termNames, templates))
      case None =>
        Success(None)
    }
  }

  def getPremises(serializedPremises: Seq[String])(implicit expressionParsingContext: ExpressionParsingContext): Try[Seq[Statement]] = {
    serializedPremises.flatMap(getOptionalString).mapWithIndex((str, index) => getStatement(str, s"premise ${index + 1}")).traverseTry
  }
  def getStatement(serializedStatement: String, description: String)(implicit expressionParsingContext: ExpressionParsingContext): Try[Statement] = {
    Statement.parser.parseFromString(serializedStatement, description).recoverWithBadRequest
  }
}
