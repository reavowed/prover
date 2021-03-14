package net.prover.types.model

import net.prover.model.definitions.{CompoundExpressionDefinition, ConjunctionDefinition, TermListAdapter}
import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.model.proof.SubstitutionContext
import net.prover.model.{Parser, SimpleVariableDefinition}
import net.prover.old.ExpressionSpecifier
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.ChapterEntry
import net.prover.types.model.entries.{TypeDefinition, TypeQualifierDefinition}

case class ParentTypeConditions(
    parentType: TypeDefinition,
    requiredParentQualifier: Option[TypeQualifierDefinition],
    requiredParentObjects: Option[RequiredParentObjects],
    termListAdapter: Option[TermListAdapter],
    conjunctionDefinition: ConjunctionDefinition)
{
  val (parentConditionConstructor, qualifierVariableDefinitions) = {
    def mainTerm(offset: Int) = TermVariable(offset, Nil)
    val (qualifierTerms, qualifierVariableDefinitions) = termListAdapter match {
      case Some(adapter) =>
        (
          (offset: Int) =>
            adapter.templates.map(ExpressionSpecifier.specify(_, adapter.variableDefinitions.indices.map(i => TermVariable(offset + i + 1, Nil)))(SubstitutionContext.outsideProof).get),
          adapter.variableDefinitions)
      case None =>
        val variableDefinitions = requiredParentQualifier match {
          case Some(qualifier) =>
            qualifier.qualifier.variableDefinitions
          case None =>
            parentType.defaultQualifier.variableDefinitions
        }
        ((offset: Int) => variableDefinitions.indices.map(i => TermVariable(offset + i + 1, Nil)), variableDefinitions)
    }
    def qualifierCondition(offset: Int) = requiredParentQualifier match {
      case Some(qualifier) =>
        val parentStatement = parentType.statementDefinition(mainTerm(offset))
        val qualifierStatement = qualifier.statementDefinition(mainTerm(offset) +: qualifierTerms(offset): _*)
        conjunctionDefinition(parentStatement, qualifierStatement)
      case None =>
        parentType.statementDefinition(mainTerm(offset) +: qualifierTerms(offset): _*)
    }
    def qualifierConstructor(offset: Int) = requiredParentObjects.map(rpo => rpo.conditionConstructor(conjunctionDefinition, offset)(_)).getOrElse(identity[Statement](_))
    ((statement: Statement, offset: Int) => conjunctionDefinition(qualifierCondition(offset), qualifierConstructor(offset)(statement)), qualifierVariableDefinitions)
  }

  val allVariableDefinitions: Seq[SimpleVariableDefinition] = parentType.mainVariableDefinition +: qualifierVariableDefinitions

  def referencedEntries: Set[ChapterEntry] = Set(parentType, conjunctionDefinition.referencedEntry) ++
    requiredParentQualifier.toSet ++
    requiredParentObjects.toSet[RequiredParentObjects].flatMap(_.referencedEntries) ++
    termListAdapter.toSet[TermListAdapter].flatMap(_.referencedEntries)

  def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): ParentTypeConditions = {
    ParentTypeConditions(
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      requiredParentQualifier.map(q => entryReplacements(q).asInstanceOf[TypeQualifierDefinition]),
      requiredParentObjects.map(_.replaceDefinitions(entryReplacements, entryContext)),
      termListAdapter.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      entryContext.conjunctionDefinitionOption.get)
  }

  def serializedFollowingLines: Seq[String] = requiredParentQualifier.map(q => Seq("parentQualifier", q.symbol).mkString(" ")).toSeq ++
    requiredParentObjects.map(_.serialized).toSeq ++
    termListAdapter.map(a => Seq("termListAdapter", a.serialized).mkString(" ")).toSeq
}

object ParentTypeConditions {
  def parser(implicit context: EntryContext): Parser[ParentTypeConditions] = {
    for {
      parentType <- context.typeDefinitionParser
      requiredParentQualifier <- parentType.parentQualifierParser
      requiredParentObjects <- RequiredParentObjects.parser(parentType)
      termListAdapter <- Parser.optional("termListAdapter", TermListAdapter.parser)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create parent type condition without conjunction"))
    } yield ParentTypeConditions(parentType, requiredParentQualifier, requiredParentObjects, termListAdapter, conjunctionDefinition)
  }
}
