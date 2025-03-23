package net.prover.model.utils

import net.prover.model._
import net.prover.model.definitions.TermDefinition
import net.prover.model.entries.{PropertyDefinitionOnType, TypeDefinition, TypeQualifierDefinition}
import net.prover.model.expressions._

import scala.reflect.ClassTag

object ExpressionUtils {
  trait TypeLikeStatement {
    def typeDefinition: TypeDefinition
    def mainTerm: Term
    def baseStatement: Statement
    def qualifierTerms: Seq[Term]
    def matchesBase(other: TypeLikeStatement): Boolean = {
      typeDefinition == other.typeDefinition && mainTerm == other.mainTerm
    }
    def matchesWithQualifierTerms(other: TypeLikeStatement): Boolean = {
      matchesBase(other) && qualifierTerms == other.qualifierTerms
    }
  }
  case class TypeStatement(typeDefinition: TypeDefinition, mainTerm: Term, qualifierTerms: Seq[Term], explicitQualifier: Option[TypeQualifierDefinition], properties: Seq[PropertyDefinitionOnType], baseStatement: Statement) extends TypeLikeStatement
  case class TypeQualifierStatement(typeQualifierDefinition: TypeQualifierDefinition, mainTerm: Term, qualifierTerms: Seq[Term], baseStatement: Statement) extends TypeLikeStatement {
    override def typeDefinition: TypeDefinition = typeQualifierDefinition.parentType
  }
  case class TypePropertyStatement(propertyDefinitionOnType: PropertyDefinitionOnType, mainTerm: Term, qualifierTerms: Seq[Term], baseStatement: Statement) extends TypeLikeStatement {
    override def typeDefinition: TypeDefinition = propertyDefinitionOnType.parentType
  }

  private def getBaseTypeStatement(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypeStatement] = {
    for {
      definedStatement <- statement.asOptionalInstanceOf[DefinedStatement]
      typeDefinition <- availableEntries.typeDefinitions.values.find(_.statementDefinition == definedStatement.definition)
      case mainTerm +: qualifierTerms <- definedStatement.components.map(_.asOptionalInstanceOf[Term]).traverseOption
    } yield TypeStatement(typeDefinition, mainTerm, qualifierTerms, None, Nil, statement)
  }
  def getTypeQualifierStatement(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypeQualifierStatement] = {
    for {
      definedStatement <- statement.asOptionalInstanceOf[DefinedStatement]
      typeQualifierDefinition <- availableEntries.qualifiersByType.values.flatten.find(_.statementDefinition == definedStatement.definition)
      case mainTerm +: qualifierTerms <- definedStatement.components.map(_.asOptionalInstanceOf[Term]).traverseOption
    } yield TypeQualifierStatement(typeQualifierDefinition, mainTerm, qualifierTerms, statement)
  }
  def getTypePropertyStatement(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypePropertyStatement] = {
    for {
      definedStatement <- statement.asOptionalInstanceOf[DefinedStatement]
      propertyDefinitionOnType <- availableEntries.propertyDefinitionsByType.values.flatten.find(_.statementDefinition == definedStatement.definition)
      case mainTerm +: qualifierTerms <- definedStatement.components.map(_.asOptionalInstanceOf[Term]).traverseOption
    } yield TypePropertyStatement(propertyDefinitionOnType, mainTerm, qualifierTerms, statement)
  }
  private def getTypeStatementWithOptionalQualifier(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypeStatement] = {
    getBaseTypeStatement(statement) orElse (for {
      conjunctionDefinition <- availableEntries.conjunctionDefinitionOption
      (first, second) <- conjunctionDefinition.unapply(statement)
      baseTypeStatement <- getTypeStatement(first)
      typeQualifierStatement <- getTypeQualifierStatement(second)
      if baseTypeStatement.matchesBase(typeQualifierStatement) && baseTypeStatement.qualifierTerms.isEmpty && baseTypeStatement.explicitQualifier.isEmpty
    } yield TypeStatement(baseTypeStatement.typeDefinition, baseTypeStatement.mainTerm, typeQualifierStatement.qualifierTerms, Some(typeQualifierStatement.typeQualifierDefinition), baseTypeStatement.properties, statement))
  }
  private def getTypeStatementWithProperties(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypeStatement] = {
    getTypeStatementWithOptionalQualifier(statement) orElse (for {
      conjunctionDefinition <- availableEntries.conjunctionDefinitionOption
      (first, second) <- conjunctionDefinition.unapply(statement)
      baseTypeStatement <- getTypeStatementWithProperties(first)
      typePropertyStatement <- getTypePropertyStatement(second)
      if baseTypeStatement.matchesWithQualifierTerms(typePropertyStatement)
      if typePropertyStatement.propertyDefinitionOnType.parentTypeConditions.requiredParentQualifier.forall(baseTypeStatement.explicitQualifier.contains)
      if !baseTypeStatement.properties.contains(typePropertyStatement.propertyDefinitionOnType)
    } yield TypeStatement(baseTypeStatement.typeDefinition, baseTypeStatement.mainTerm, typePropertyStatement.qualifierTerms, baseTypeStatement.explicitQualifier, baseTypeStatement.properties :+ typePropertyStatement.propertyDefinitionOnType, statement))
  }

  def getTypeStatement(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypeStatement] = {
    getTypeStatementWithProperties(statement)
  }
  def getTypeLikeStatement(statement: Statement)(implicit availableEntries: AvailableEntries): Option[TypeLikeStatement] = {
    getTypeStatementWithOptionalQualifier(statement) orElse getTypeQualifierStatement(statement) orElse getTypePropertyStatement(statement)
  }
  def isTypeLikeStatement(statement: Statement)(implicit availableEntries: AvailableEntries): Boolean = {
    getTypeLikeStatement(statement).nonEmpty
  }

  private def getFromUnaryExpression[TExpression <: Expression : ClassTag, TDefinedExpression <: DefinedExpression[TExpression] : ClassTag, TResult](statement: TExpression, f: TExpression => Option[TResult]): Option[TResult] = {
    statement.asOptionalInstanceOf[TDefinedExpression].filter(_.boundVariableNames.isEmpty).flatMap(_.components.single).flatMap(_.asOptionalInstanceOf[TExpression]).flatMap(f)
  }
  private def getFromUnaryStatement[T](statement: Statement, f: Statement => Option[T]): Option[T] = {
    getFromUnaryExpression[Statement, DefinedStatement, T](statement, f)
  }
  private def getFromUnaryTerm[T](term: Term, f: Term => Option[T]): Option[T] = {
    getFromUnaryExpression[Term, DefinedTerm, T](term, f)
  }

  private def getFromBinaryExpression[TExpression <: Expression : ClassTag, TDefinedExpression <: DefinedExpression[TExpression] : ClassTag, TResult1, TResult2](
    statement: TExpression,
    fLeft: TExpression => Option[TResult1],
    fRight: TExpression => Option[TResult2]
  ): Option[(TResult1, TResult2)] = {
    for {
      definedExpression <- statement.asOptionalInstanceOf[TDefinedExpression]
      if definedExpression.boundVariableNames.isEmpty
      (first, second) <- definedExpression.components match {
        case Seq(first: TExpression, second: TExpression) => Some((first, second))
        case _ => None
      }
      result1 <- fLeft(first)
      result2 <- fRight(second)
    } yield (result1, result2)
  }

  private def getFromBinaryStatement[T, S](statement: Statement, f: Statement => Option[T], g: Statement => Option[S]): Option[(T, S)] = {
    getFromBinaryExpression[Statement, DefinedStatement, T, S](statement, f, g)
  }
  private def getFromBinaryTerm[T, S](term: Term, f: Term => Option[T], g: Term => Option[S]): Option[(T, S)] = {
    getFromBinaryExpression[Term, DefinedTerm, T, S](term, f, g)
  }


  def getSimpleStatementVariable(statement: Statement): Option[Int] = {
    statement.asOptionalInstanceOf[StatementVariable].filter(_.arguments.isEmpty).map(_.index)
  }
  def isSimpleStatementVariable(statement: Statement): Boolean = {
    getSimpleStatementVariable(statement).isDefined
  }
  def getWrappedSimpleStatementVariable(statement: Statement): Option[Int] = {
    getSimpleStatementVariable(statement) orElse getFromUnaryStatement(statement, getWrappedSimpleStatementVariable)
  }
  def getWrappedBinaryStatementVariables(statement: Statement): Option[(Int, Int)] = {
    getFromUnaryStatement(statement, getWrappedBinaryStatementVariables) orElse getFromBinaryStatement(statement, getWrappedSimpleStatementVariable, getWrappedSimpleStatementVariable)
  }


  def getSimpleTermVariable(term: Term): Option[Int] = {
    term.asOptionalInstanceOf[TermVariable].filter(_.arguments.isEmpty).map(_.index)
  }
  def isSimpleTermVariable(term: Term): Boolean = {
    getSimpleTermVariable(term).isDefined
  }
  def getTermConstantDefinition(term: Term): Option[TermDefinition] = {
    term.asOptionalInstanceOf[DefinedTerm].filter(_.components.isEmpty).map(_.definition)
  }
  def isTermConstant(term: Term): Boolean = {
    getTermConstantDefinition(term).isDefined
  }
  def getCombinationOfSimpleTermVariables(term: Term): Option[Seq[Int]] = {
    recurseOnTerms(term, getSimpleTermVariable)
  }
  def isCombinationOfTermConstants(t: Term): Boolean = {
    recurseOnTerms(t, getTermConstantDefinition).isDefined
  }
  def isSimpleTermVariableOrCombinationOfTermConstants(t: Term): Boolean = {
    isSimpleTermVariable(t) || isCombinationOfTermConstants(t)
  }
  def isWrappedSimpleTerm(t: Term): Boolean = {
    t.asOptionalInstanceOf[DefinedTerm].map(_.components).exists {
      case Seq(t1: Term) if isSimpleTermVariable(t1) || isTermConstant(t1) =>
        true
      case _ =>
        false
    }
  }
  def getWrappedSimpleTermVariable(term: Term): Option[Int] = {
    getSimpleTermVariable(term) orElse
      term.asOptionalInstanceOf[DefinedTerm].filter(_.boundVariableNames.isEmpty).flatMap(_.components.single).flatMap(_.asOptionalInstanceOf[Term]).flatMap(getWrappedSimpleTermVariable)
  }
  def getSingleSimpleTermVariable(term: Term): Option[Int] = {
    getSimpleTermVariable(term) orElse
      getFromUnaryTerm(term, getSingleSimpleTermVariable) orElse
      getFromBinaryTerm(term, getTermConstantDefinition, getSingleSimpleTermVariable).map(_._2)
  }

  private def recurseOnTerms[T](term: Term, f: Term => Option[T]): Option[Seq[T]] = {
    f(term).map(Seq(_)) orElse (for {
      definedTerm <- term.asOptionalInstanceOf[DefinedTerm]
      if definedTerm.boundVariableNames.isEmpty
      termComponents <- definedTerm.components.map(_.asOptionalInstanceOf[Term]).traverseOption
      innerValues <- termComponents.map(recurseOnTerms(_, f)).traverseOption.map(_.flatten)
    } yield innerValues)
  }

}
