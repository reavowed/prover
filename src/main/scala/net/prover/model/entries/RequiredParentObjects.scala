package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.{ConjunctionDefinition, DeductionDefinition, GeneralizationDefinition, UniqueExistenceDefinition}
import net.prover.model.expressions.{FunctionParameter, Statement, TermVariable}

case class RequiredParentObjects(
  objectDefinitions: Seq[RelatedObjectDefinition],
  uniqueExistenceDefinition: UniqueExistenceDefinition,
  generalizationDefinition: GeneralizationDefinition,
  deductionDefinition: DeductionDefinition)
{
  def referencedEntries: Set[ChapterEntry] = objectDefinitions.flatMap(_.referencedEntries).toSet ++ Set(uniqueExistenceDefinition, generalizationDefinition, deductionDefinition).map(_.referencedEntry)
  def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], entryContext: EntryContext): RequiredParentObjects = {
    RequiredParentObjects(
      objectDefinitions.map(d => entryReplacements(d).asInstanceOf[RelatedObjectDefinition]),
      entryContext.uniquenessDefinitionOption.get,
      entryContext.generalizationDefinitionOption.get,
      entryContext.deductionDefinitionOption.get)
  }
  def conditionConstructor(conjunctionDefinition: ConjunctionDefinition, offset: Int)(statement: Statement): Statement = {
    objectDefinitions.foldRight(statement) { (objectDefinition, currentStatement) =>
      val objectCondition = objectDefinition.statementDefinition(FunctionParameter(0, 0) +: objectDefinition.parentVariableDefinitions.indices.map(i => TermVariable(i + offset)): _*)
      conjunctionDefinition(
        uniqueExistenceDefinition(objectDefinition.mainVariableDefinition.name, objectCondition),
        generalizationDefinition(objectDefinition.mainVariableDefinition.name, deductionDefinition(objectCondition, currentStatement)))
    }
  }
  def serialized: String = Seq("parentObjects", objectDefinitions.map(_.symbol).mkString(" ").inParens).mkString(" ")
}

object RequiredParentObjects {
  implicit class OptionOps(option: Option[RequiredParentObjects]) {
    def objectDefinitions: Seq[RelatedObjectDefinition] = option.map(_.objectDefinitions).getOrElse(Nil)
    def addParametersToParsingContext(expressionParsingContext: ExpressionParsingContext): ExpressionParsingContext = {
      option.map(_.objectDefinitions.foldRight(expressionParsingContext) { (d, c) => c.addInitialParameter(d.mainVariableDefinition.name) })
        .getOrElse(expressionParsingContext)
    }
  }
  def parser(parentType: TypeDefinition)(implicit context: EntryContext): Parser[Option[RequiredParentObjects]] = {
    Parser.optional(
      "parentObjects",
      for {
        objectDefinitions <- Parser.wordsInParens.map(_.map(s => context.relatedObjectsByType.getOrElse(parentType.symbol, Nil).find(_.symbol == s).getOrElse(throw new Exception(s"Unrecognised object '$s' on type '${parentType.symbol}'"))))
        uniquenessDefinition = context.uniquenessDefinitionOption.getOrElse(throw new Exception("Cannot add related objects to property definition without uniqueness"))
        generalizationDefinition = context.generalizationDefinitionOption.getOrElse(throw new Exception("Cannot add related objects to property definition without generalization"))
        deductionDefinition = context.deductionDefinitionOption.getOrElse(throw new Exception("Cannot add related objects to property definition without deduction"))
      } yield RequiredParentObjects(objectDefinitions, uniquenessDefinition, generalizationDefinition, deductionDefinition))
  }
}
