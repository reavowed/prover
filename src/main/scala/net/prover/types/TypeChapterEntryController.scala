package net.prover.types

import net.prover._
import net.prover.model.ExpressionParsingContext
import net.prover.model.expressions.Statement
import net.prover.structure.{BookService, ChapterControllerBase, EntryContext}
import net.prover.types
import net.prover.types.datatransfer._
import net.prover.types.model.entries.{StandalonePropertyDefinition, TypeDefinition}
import net.prover.types.model.{ParentTypeConditions, entries}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.{PathVariable, PostMapping, RequestBody}

class TypeChapterEntryController @Autowired() (val bookService: BookService) extends ChapterControllerBase {

  @PostMapping(value = Array("/typeDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeDefinition: NewTypeDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTypeDefinition.name)
      for {
        symbol <- getMandatoryString(newTypeDefinition.symbol, "Symbol")
        mainVariableDefinition <- getSimpleVariableDefinition(newTypeDefinition.mainVariableDefinition, "Main variable definition")
        qualifier <- getOptionalQualifier(newTypeDefinition.qualifierVariableDefinitions, newTypeDefinition.qualifierFormat)
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(mainVariableDefinition +: qualifier.variableDefinitions)
        definition <- Statement.parser(expressionParsingContext).parseFromString(newTypeDefinition.definition, "definition").recoverWithBadRequest
        newTypeDefinition = TypeDefinition(
          symbol,
          mainVariableDefinition,
          qualifier,
          name,
          definition)
      } yield newTypeDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/typeQualifierDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeQualifierDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeQualifierDefinition: NewTypeQualifierDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTypeQualifierDefinition.name)
      for {
        symbol <- getMandatoryString(newTypeQualifierDefinition.symbol, "Symbol")
        parentType <- getTypeDefinition(newTypeQualifierDefinition.parentType)
        qualifier <- getQualifier(newTypeQualifierDefinition.qualifierTermNames, newTypeQualifierDefinition.qualifierFormat)
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(parentType.mainVariableDefinition +: qualifier.variableDefinitions)
        definition <- Statement.parser(expressionParsingContext).parseFromString(newTypeQualifierDefinition.definition, "definition").recoverWithBadRequest
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        newTypeQualifierDefinition = entries.TypeQualifierDefinition(
          symbol,
          parentType,
          qualifier,
          name,
          definition,
          conjunctionDefinition)
      } yield newTypeQualifierDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/propertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createPropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefinition: NewPropertyDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newPropertyDefinition.name)
      for {
        symbol <- getMandatoryString(newPropertyDefinition.symbol, "Symbol")
        parentType <- getTypeDefinition(newPropertyDefinition.parentType)
        requiredParentQualifier <- getOptionalParentQualifier(parentType, newPropertyDefinition.requiredParentQualifier)
        requiredParentObjects <- getParentObjects(parentType, newPropertyDefinition.requiredParentObjects)
        adapter <- getOptionalAdapter(newPropertyDefinition.ownTermNames, newPropertyDefinition.parentTerms, (requiredParentQualifier.map(_.qualifier) orElse parentType.defaultQualifier).variableDefinitions)
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        parentTypeConditions = ParentTypeConditions(parentType, requiredParentQualifier, requiredParentObjects, adapter, conjunctionDefinition)
        expressionParsingContext = requiredParentObjects.addParametersToParsingContext(ExpressionParsingContext.forTypeDefinition(parentTypeConditions.allVariableDefinitions))
        definingStatement <- Statement.parser(expressionParsingContext).parseFromString(newPropertyDefinition.definingStatement, "definition").recoverWithBadRequest
        newPropertyDefinition = entries.PropertyDefinitionOnType(
          symbol,
          parentTypeConditions,
          name,
          definingStatement)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/relatedObjectDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createRelatedObjectDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newRelatedObjectDefinition: NewRelatedObjectDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newRelatedObjectDefinition.name)
      for {
        symbol <- getMandatoryString(newRelatedObjectDefinition.symbol, "Symbol")
        mainVariableDefinition <- getSimpleVariableDefinition(newRelatedObjectDefinition.mainVariableDefinition, "Main variable definition")
        parentType <- getTypeDefinition(newRelatedObjectDefinition.parentType)
        requiredParentQualifier <- getOptionalParentQualifier(parentType, newRelatedObjectDefinition.requiredParentQualifier)
        requiredParentObjects <- getParentObjects(parentType, newRelatedObjectDefinition.requiredParentObjects)
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        parentTypeConditions = types.model.ParentTypeConditions(parentType, requiredParentQualifier, requiredParentObjects, None, conjunctionDefinition)
        expressionParsingContext = requiredParentObjects.addParametersToParsingContext(ExpressionParsingContext.forTypeDefinition(mainVariableDefinition +: parentTypeConditions.allVariableDefinitions))
        definingStatement <- Statement.parser(expressionParsingContext).parseFromString(newRelatedObjectDefinition.definingStatement, "definition").recoverWithBadRequest
        newPropertyDefinition = entries.RelatedObjectDefinition(
          symbol,
          mainVariableDefinition,
          parentTypeConditions,
          name,
          definingStatement)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/typeRelationDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeRelationDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeRelationDefinition: NewTypeRelationDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTypeRelationDefinition.name)
      for {
        symbol <- getMandatoryString(newTypeRelationDefinition.symbol, "Symbol")
        firstType <- getTypeDefinition(newTypeRelationDefinition.firstType)
        firstVariableDefinition <- getSimpleVariableDefinition(newTypeRelationDefinition.firstVariableDefinition, "First variable definition")
        secondType <- getTypeDefinition(newTypeRelationDefinition.secondType)
        secondVariableDefinition <- getSimpleVariableDefinition(newTypeRelationDefinition.secondVariableDefinition, "Second variable definition")
        linkingPhrase <- getMandatoryString(newTypeRelationDefinition.linkingPhrase, "Linking phrase")
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(Seq(firstVariableDefinition, secondVariableDefinition))
        definition <- Statement.parser(expressionParsingContext).parseFromString(newTypeRelationDefinition.definingStatement, "definition").recoverWithBadRequest
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        newPropertyDefinition = entries.TypeRelationDefinition(
          symbol,
          firstType,
          secondType,
          firstVariableDefinition,
          secondVariableDefinition,
          linkingPhrase,
          name,
          definition,
          conjunctionDefinition)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/standalonePropertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createStandalonePropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefinition: NewStandalonePropertyDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newPropertyDefinition.name)
      for {
        symbol <- getMandatoryString(newPropertyDefinition.symbol, "Symbol")
        mainVariableDefinition <- getSimpleVariableDefinition(newPropertyDefinition.mainVariableDefinition, "Main variable definition")
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(Seq(mainVariableDefinition))
        definition <- Statement.parser(expressionParsingContext).parseFromString(newPropertyDefinition.definingStatement, "definition").recoverWithBadRequest
        newPropertyDefinition = StandalonePropertyDefinition(
          symbol,
          mainVariableDefinition,
          name,
          definition)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }
}
