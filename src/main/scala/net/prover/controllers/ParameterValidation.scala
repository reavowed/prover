package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{Qualifier, TermListAdapter}
import net.prover.model.entries.{PropertyDefinitionOnType, RequiredParentObjects, TypeDefinition, TypeQualifierDefinition}
import net.prover.model.expressions.{Statement, Term}

import scala.util.{Failure, Success, Try}

trait ParameterValidation {

  def getOptionalString(source: String): Option[String] = {
    Option(source).map(_.trim()).filter(_.nonEmpty)
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
  def getTypeDefinition(symbol: String)(implicit availableEntries: AvailableEntries): Try[TypeDefinition] = {
    availableEntries.typeDefinitions.get(symbol).orBadRequest(s"Unknown type '$symbol'")
  }
  def getSimpleVariableDefinition(text: String, description: String): Try[SimpleVariableDefinition] = {
    SimpleVariableDefinition.parser.parseFromString(text, description).recoverWithBadRequest
  }
  def getSimpleVariableDefinitions(text: String, description: String): Try[Seq[SimpleVariableDefinition]] = {
    SimpleVariableDefinition.parser.toEndOfFile.parseFromString(text, description).recoverWithBadRequest
  }
  def getQualifier(variableDefinitionsText: String, serializedFormat: String): Try[Qualifier] = {
    for {
      variableDefinitions <- getSimpleVariableDefinitions(variableDefinitionsText, "qualifier variable definitions")
      _ <- variableDefinitions.nonEmpty.orBadRequest("At least one qualifier variable must be provided")
      format <- Format.parserForTypeDefinition(variableDefinitions).parseFromString(serializedFormat, "qualifier format").recoverWithBadRequest
    } yield Qualifier(variableDefinitions, format)
  }
  def getOptionalQualifier(variableDefinitionsText: String, serializedFormat: String): Try[Option[Qualifier]] = {
    (getOptionalString(variableDefinitionsText), getOptionalString(serializedFormat)) match {
      case (Some(variableDefinitionsText), Some(serializedFormat)) =>
        getQualifier(variableDefinitionsText, serializedFormat).map(Some(_))
      case (None, None) =>
        Success(None)
      case _ =>
        Failure(BadRequestException("Both format and term names must be provided for qualifier"))
    }
  }
  def getOptionalParentQualifier(parentType: TypeDefinition, qualifierSymbolText: String)(implicit availableEntries: AvailableEntries): Try[Option[TypeQualifierDefinition]] = {
    getOptionalString(qualifierSymbolText)
      .map(qualifierSymbol => availableEntries.qualifiersByType(parentType.symbol).find(_.symbol == qualifierSymbol).orBadRequest(s"Unknown qualifier '$qualifierSymbol' on type '${parentType.symbol}'"))
      .swap
  }
  def getParentObjects(parentType: TypeDefinition, objectSymbolsText: String)(implicit availableEntries: AvailableEntries): Try[Option[RequiredParentObjects]] = {
    getWords(objectSymbolsText) match {
      case Nil =>
        Success(None)
      case objectSymbols =>
        for {
          objectDefinitions <- objectSymbols.map(s => availableEntries.relatedObjectsByType(parentType.symbol).find(_.symbol == s)
            .orBadRequest(s"Unknown object '$s' on type '${parentType.symbol}'"))
            .traverseTry
          uniquenessDefinition <- availableEntries.uniquenessDefinitionOption.orBadRequest("Cannot add related objects to property definition without uniqueness")
          generalizationDefinition <- availableEntries.generalizationDefinitionOption.orBadRequest("Cannot add related objects to property definition without generalization")
          deductionDefinition <- availableEntries.deductionDefinitionOption.orBadRequest("Cannot add related objects to property definition without deduction")
        } yield Some(RequiredParentObjects(objectDefinitions, uniquenessDefinition, generalizationDefinition, deductionDefinition))
    }
  }
  def getOptionalAdapter(variableDefinitionsText: String, possibleSerializedTemplates: String, qualifierVariableDefinitions: Seq[SimpleVariableDefinition])(implicit availableEntries: AvailableEntries): Try[Option[TermListAdapter]] = {
    getOptionalString(possibleSerializedTemplates) match {
      case Some(serializedTemplates) =>
        for {
          variableDefinitions <- getSimpleVariableDefinitions(variableDefinitionsText, "adapter variable definitions")
          epc = ExpressionParsingContext.forTypeDefinition(variableDefinitions)
          templates <- qualifierVariableDefinitions.indices.map(_ => Term.parser(epc)).traverse.parseFromString(serializedTemplates, "templates").recoverWithBadRequest
        } yield Some(TermListAdapter(variableDefinitions, templates))
      case None =>
        Success(None)
    }
  }

  private def parse[T](parser: Parser[T], str: String, description: String): Try[T] = {
    parser.parseFromString(str, description).recoverWithBadRequest
  }
  private def parseAll[T](parser: Parser[T], strs: Seq[String], description: String): Try[Seq[T]] = {
    strs.mapWithIndex((str, index) =>  parse(parser, str, s"$description ${index + 1}")).traverseTry
  }

  def getVariableDefinitions(serializedVariableDefinitions: String): Try[VariableDefinitions] = {
    VariableDefinitions.parser.parseFromString(serializedVariableDefinitions, "variables").recoverWithBadRequest
  }

  def getPremises(serializedPremises: Seq[String])(implicit expressionParsingContext: ExpressionParsingContext): Try[Seq[Statement]] = {
    parseAll(Statement.parser, serializedPremises.flatMap(getOptionalString), "premise")
  }
  def getStatement(serializedStatement: String, description: String)(implicit expressionParsingContext: ExpressionParsingContext): Try[Statement] = {
    parse(Statement.parser, serializedStatement, description)
  }
}
