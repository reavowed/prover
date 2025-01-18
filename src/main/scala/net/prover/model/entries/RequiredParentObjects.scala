package net.prover.model.entries

import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.proving.structure.definitions.{ConjunctionDefinition, DeductionDefinition, GeneralizationDefinition, UniqueExistenceDefinition}

case class RequiredParentObjects(
  objectDefinitions: Seq[RelatedObjectDefinition],
  uniqueExistenceDefinition: UniqueExistenceDefinition,
  generalizationDefinition: GeneralizationDefinition,
  deductionDefinition: DeductionDefinition)
{
  def referencedEntries: Set[ChapterEntry] = objectDefinitions.flatMap(_.referencedEntries).toSet ++ Set(uniqueExistenceDefinition, generalizationDefinition, deductionDefinition).map(_.referencedEntry)
  def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], availableEntries: AvailableEntries): RequiredParentObjects = {
    RequiredParentObjects(
      objectDefinitions.map(d => entryReplacements(d).asInstanceOf[RelatedObjectDefinition]),
      availableEntries.uniquenessDefinitionOption.get,
      availableEntries.generalizationDefinitionOption.get,
      availableEntries.deductionDefinitionOption.get)
  }
  def conditionConstructor(conjunctionDefinition: ConjunctionDefinition, offset: Int)(statement: Statement): Statement = {
    val existenceStatements = objectDefinitions.map { objectDefinition =>
      uniqueExistenceDefinition(objectDefinition.mainVariableDefinition.name, objectDefinition.condition(offset))
    }
    def helper(statement: Statement, extractedStatements: Seq[Statement]): (Statement, Seq[Statement]) = {
      def byExtractingLeft = for {
        (l, r) <- conjunctionDefinition.unapply(statement)
        extractedL <- l.removeExternalParameters(objectDefinitions.length)
      } yield helper(r, extractedStatements :+ extractedL)
      byExtractingLeft getOrElse (statement, extractedStatements)
    }
    val (innerStatement, extractedStatements) = helper(statement, Nil)
    val generalizedInnerStatement = objectDefinitions.foldRight(innerStatement) { (objectDefinition, currentStatement) =>
      generalizationDefinition(objectDefinition.mainVariableDefinition.name, deductionDefinition(objectDefinition.condition(offset), currentStatement))
    }
    conjunctionDefinition.all(extractedStatements ++ existenceStatements :+ generalizedInnerStatement :_*)
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
  def parser(parentType: TypeDefinition)(implicit context: AvailableEntries): Parser[Option[RequiredParentObjects]] = {
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
