package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions._
import net.prover.model.entries.PropertyDefinitionOnType.RequiredParentObjects
import net.prover.model.expressions.{FunctionParameter, Statement, TermVariable}
import net.prover.model.proof.SubstitutionContext

case class PropertyDefinitionOnType(
    symbol: String,
    parentType: TypeDefinition,
    requiredParentQualifier: Option[TypeQualifierDefinition],
    requiredParentObjects: Option[RequiredParentObjects],
    termListAdapter: Option[TermListAdapter],
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: ConjunctionDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  override def title: String = s"Definition: ${name.capitalizeWords} ${parentType.name.capitalizeWords}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.referencedEntry + parentType

  override def withSymbol(newSymbol: String): PropertyDefinitionOnType = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): PropertyDefinitionOnType = copy(explicitName = newName)

  def baseFormat = Format.Explicit(s"%1 is %0", s"${parentType.mainVariableDefinition.name} is $name", 2, true, true)
  def fullFormat = parentType.defaultQualifier.prependFormat(baseFormat)

  val (parentTypeConditionConstructor, qualifierVariableDefinitions) = PropertyDefinitionOnType.getParentConditionAndQualifierVariableDefinitions(parentType, termListAdapter, requiredParentQualifier, requiredParentObjects, conjunctionDefinition)
  val allVariableDefinitions: Seq[SimpleVariableDefinition] = parentType.mainVariableDefinition +: qualifierVariableDefinitions
  val allVariableNames: Seq[String] = allVariableDefinitions.map(_.name)
  def allComponents: Seq[TermComponent] = allVariableNames.map(ComponentType.TermComponent(_, Nil))

  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    allComponents,
    Some(name),
    fullFormat,
    Some(parentTypeConditionConstructor(definingStatement)),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("property", symbol, "on", parentType.symbol).mkString(" ") +:
    (requiredParentQualifier.map(q => Seq("parentQualifier", q.symbol).mkString(" ")).toSeq ++
      requiredParentObjects.map(_.serialized).toSeq ++
      termListAdapter.map(a => Seq("termListAdapter", a.serialized).mkString(" ")).toSeq ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): PropertyDefinitionOnType = {
    PropertyDefinitionOnType(
      symbol,
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      requiredParentQualifier.map(q => entryReplacements(q).asInstanceOf[TypeQualifierDefinition]),
      requiredParentObjects.map(_.replaceDefinitions(entryReplacements, entryContext)),
      termListAdapter.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryContext.conjunctionDefinitionOption.get)
  }
}

object PropertyDefinitionOnType extends ChapterEntryParser {

  case class RequiredParentObjects(
    objectDefinitions: Seq[RelatedObjectDefinition],
    uniqueExistenceDefinition: UniqueExistenceDefinition,
    generalizationDefinition: GeneralizationDefinition,
    deductionDefinition: DeductionDefinition)
  {
    def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], entryContext: EntryContext): RequiredParentObjects = {
      RequiredParentObjects(
        objectDefinitions.map(d => entryReplacements(d).asInstanceOf[RelatedObjectDefinition]),
        entryContext.uniquenessDefinitionOption.get,
        entryContext.generalizationDefinitionOption.get,
        entryContext.deductionDefinitionOption.get)
    }
    def conditionConstructor(conjunctionDefinition: ConjunctionDefinition)(statement: Statement): Statement = {
      objectDefinitions.foldRight(statement) { (objectDefinition, currentStatement) =>
        val objectCondition = objectDefinition.statementDefinition(FunctionParameter(0, 0) +: objectDefinition.parentVariableDefinitions.indices.map(TermVariable(_)): _*)
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

  def getParentConditionAndQualifierVariableDefinitions(
    parentType: TypeDefinition,
    termListAdapter: Option[TermListAdapter],
    requiredParentQualifier: Option[TypeQualifierDefinition],
    requiredParentObjects: Option[RequiredParentObjects],
    conjunctionDefinition: ConjunctionDefinition
  ): (Statement => Statement, Seq[SimpleVariableDefinition]) = {
    val mainTerm = TermVariable(0, Nil)
    val (qualifierTerms, qualifierVariableDefinitions) = termListAdapter match {
      case Some(adapter) =>
        val adapterTerms = adapter.variableDefinitions.indices.map(i => TermVariable(i + 1, Nil))
        (adapter.templates.map(_.specify(adapterTerms)(SubstitutionContext.outsideProof).get), adapter.variableDefinitions)
      case None =>
        val variableDefinitions = requiredParentQualifier match {
          case Some(qualifier) =>
            qualifier.qualifier.variableDefinitions
          case None =>
            parentType.defaultQualifier.variableDefinitions
        }
        (variableDefinitions.indices.map(i => TermVariable(i + 1, Nil)), variableDefinitions)
    }
    val qualifierCondition = requiredParentQualifier match {
      case Some(qualifier) =>
        val parentStatement = parentType.statementDefinition(mainTerm)
        val qualifierStatement = qualifier.statementDefinition(mainTerm +: qualifierTerms: _*)
        conjunctionDefinition(parentStatement, qualifierStatement)
      case None =>
        parentType.statementDefinition(mainTerm +: qualifierTerms: _*)
    }
    val qualifierConstructor = requiredParentObjects.map(rpo => rpo.conditionConstructor(conjunctionDefinition)(_)).getOrElse(identity[Statement](_))
    ((statement: Statement) => conjunctionDefinition(qualifierCondition, qualifierConstructor(statement)), qualifierVariableDefinitions)
  }

  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentType <- Parser.required("on", context.typeDefinitionParser)
      requiredParentQualifier <- parentType.parentQualifierParser
      requiredParentObjects <- RequiredParentObjects.parser(parentType)
      termListAdapter <- Parser.optional("termListAdapter", TermListAdapter.parser)
      explicitName <- Parser.optional("name", Parser.allInParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create property definition without conjunction"))
      (_, qualifierVariableDefinitions) = getParentConditionAndQualifierVariableDefinitions(parentType, termListAdapter, requiredParentQualifier, requiredParentObjects, conjunctionDefinition)
      expressionParsingContext = requiredParentObjects.addParametersToParsingContext(ExpressionParsingContext.forTypeDefinition(parentType.mainVariableDefinition +: qualifierVariableDefinitions))
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
    } yield PropertyDefinitionOnType(symbol, parentType, requiredParentQualifier, requiredParentObjects, termListAdapter, explicitName, definingStatement, conjunctionDefinition)
  }
}
