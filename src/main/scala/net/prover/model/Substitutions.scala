package net.prover.model

import monocle.Lens
import monocle.macros.GenLens
import net.prover.model.entries.ExpressionDefinition
import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext
import net.prover.util.PossibleSingleMatch
import net.prover.util.PossibleSingleMatch._

case class Substitutions(
    statements: Map[String, Statement] = Map.empty,
    terms: Map[String, Term] = Map.empty,
    predicates: Map[(String, Int), Statement] = Map.empty,
    functions: Map[(String, Int), Term] = Map.empty)
{
  def isEmpty: Boolean = statements.isEmpty && terms.isEmpty && functions.isEmpty && predicates.isEmpty

  def insertExternalParameters(numberOfParametersToInsert: Int): Substitutions = {
    Substitutions(
      statements.mapValues(_.insertExternalParameters(numberOfParametersToInsert)),
      terms.mapValues(_.insertExternalParameters(numberOfParametersToInsert)),
      predicates.mapValues(_.insertExternalParameters(numberOfParametersToInsert)),
      functions.mapValues(_.insertExternalParameters(numberOfParametersToInsert)))
  }
  def removeExternalParameters(numberOfParametersToRemove: Int): Option[Substitutions] = {
    for {
      newStatements <- statements.mapValues(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      newTerms <- terms.mapValues(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      newPredicates <- predicates.mapValues(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      newFunctions <- functions.mapValues(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
    } yield Substitutions(newStatements, newTerms, newPredicates, newFunctions)
  }
  def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Substitutions = {
    Substitutions(
      statements.mapValues(_.replaceDefinition(oldDefinition, newDefinition)),
      terms.mapValues(_.replaceDefinition(oldDefinition, newDefinition)),
      predicates.mapValues(_.replaceDefinition(oldDefinition, newDefinition)),
      functions.mapValues(_.replaceDefinition(oldDefinition, newDefinition)))
  }
}

object Substitutions {
  val empty = Substitutions(Map.empty, Map.empty, Map.empty, Map.empty)

  case class Required(statements: Seq[String], terms: Seq[String], predicates: Seq[(String, Int)], functions: Seq[(String, Int)]) {
    def isEmpty: Boolean = statements.isEmpty && terms.isEmpty && functions.isEmpty && predicates.isEmpty

    def ++(other: Required): Required = {
      Required(
        (statements ++ other.statements).distinct,
        (terms ++ other.terms).distinct,
        (predicates ++ other.predicates).distinct,
        (functions ++ other.functions).distinct)
    }
    def isEquivalentTo(other: Required): Boolean = {
      statements.toSet == other.statements.toSet &&
        terms.toSet == other.terms.toSet &&
        predicates.toSet == other.predicates.toSet &&
        functions.toSet == other.functions.toSet
    }
    def contains(other: Required): Boolean = {
      other.statements.toSet.subsetOf(statements.toSet) &&
        other.terms.toSet.subsetOf(terms.toSet) &&
        other.predicates.toSet.subsetOf(predicates.toSet) &&
        other.functions.toSet.subsetOf(functions.toSet)
    }
    def isSingleStatementVariable: Boolean = {
      statements.length == 1 && terms.isEmpty && predicates.isEmpty && functions.isEmpty
    }
  }

  object Required {
    val empty = Required(Seq.empty, Seq.empty, Seq.empty, Seq.empty)
    implicit class RequiredSeqOps(seq: Seq[Required]) {
      def foldTogether: Required = {
        seq.fold(empty)(_ ++ _)
      }
    }
  }

  case class Possible(
    statements: Map[String, Statement] = Map.empty,
    terms: Map[String, Term] = Map.empty,
    predicates: Map[(String, Int), Statement] = Map.empty,
    functions: Map[(String, Int), Term] = Map.empty,
    predicateApplications: Map[(String, Int), Seq[(Seq[Term], Statement, Int)]] = Map.empty,
    functionApplications: Map[(String, Int), Seq[(Seq[Term], Term, Int)]] = Map.empty)
  {
    def update[TKey, TExpression](
      key: TKey,
      expression: TExpression,
      lens: Lens[Substitutions.Possible, Map[TKey, TExpression]]
    ): Option[Substitutions.Possible] = {
      lens.get(this)
        .tryAdd(key, expression)
        .map(lens.set(_)(this))
    }
    def updateAdd[TKey, TExpression](
      key: TKey,
      expression: (Seq[Term], TExpression, Int),
      lens: Lens[Substitutions.Possible, Map[TKey, Seq[(Seq[Term], TExpression, Int)]]]
    ): Substitutions.Possible = {
      val map = lens.get(this)
      val newValue = map.getOrElse(key, Nil) :+ expression
      val newMap = map.updated(key, newValue)
      lens.set(newMap)(this)
    }
    def updateAdd[TKey, TExpression](
      key: TKey,
      expression: Seq[(Seq[Term], TExpression, Int)],
      lens: Lens[Substitutions.Possible, Map[TKey, Seq[(Seq[Term], TExpression, Int)]]]
    ): Substitutions.Possible = {
      val map = lens.get(this)
      val newValue = map.getOrElse(key, Nil) ++ expression
      val newMap = map.updated(key, newValue)
      lens.set(newMap)(this)
    }

    def clearApplicationsWherePossible(externalDepth: Int): Option[Substitutions.Possible] = {
      Possible.clearApplicationsWherePossible(this, externalDepth)
    }

    def stripApplications(): Substitutions = Substitutions(statements, terms, predicates, functions)

    def confirmTotality: Option[Substitutions] = {
      if (predicateApplications.isEmpty && functionApplications.isEmpty)
        Some(stripApplications())
      else
        None
    }
  }
  object Possible {

    val empty = Possible(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
    val predicatesLens = GenLens[Substitutions.Possible](_.predicates)
    val functionsLens = GenLens[Substitutions.Possible](_.functions)
    val predicateApplicationsLens = GenLens[Substitutions.Possible](_.predicateApplications)
    val functionApplicationsLens = GenLens[Substitutions.Possible](_.functionApplications)

    private def clearAtStatement(
      arity: Int,
      currentApplications: Seq[(Seq[Term], Statement, Int)],
      externalDepth: Int,
      currentInternalDepth: Int,
      currentSubstitutions: Possible
    ): Iterator[(Statement, Possible)] = {
      def statementVariable = for {
        currentApplicationsAsStatementVariable <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[StatementVariable])).traverseOption.iterator
        name <- currentApplicationsAsStatementVariable.map { case (_, v, _) => v.name }.distinct.single.iterator
      } yield (StatementVariable(name), currentSubstitutions)
      def definedStatement = for {
        currentApplicationsAsDefinedStatement <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[DefinedStatement])).traverseOption.iterator
        boundVariableNames = currentApplicationsAsDefinedStatement.head._2.scopedBoundVariableNames
        statementDefinition <- currentApplicationsAsDefinedStatement.map { case (_, t, _) => t.definition }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(statementDefinition.componentTypes.length, arity, currentApplicationsAsDefinedStatement.map(_.map2(_.components)), externalDepth, statementDefinition.increaseDepth(currentInternalDepth), currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      } yield (DefinedStatement(expressions, statementDefinition)(boundVariableNames), resultSubstitutions)
      def predicateApplication = for {
        currentApplicationsAsPredicateApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[PredicateApplication])).traverseOption.iterator
        length = currentApplicationsAsPredicateApplication.head._2.arity
        variableName <- currentApplicationsAsPredicateApplication.map { case (_, t, _) => t.variableName }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(length, arity, currentApplicationsAsPredicateApplication.map(_.map2(_.arguments)), externalDepth, currentInternalDepth, currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
        terms = expressions.map(_.asInstanceOf[Term])
      } yield (PredicateApplication(variableName, terms), resultSubstitutions)

      statementVariable ++ definedStatement ++ predicateApplication
    }

    private def clearAtTerm(
      arity: Int,
      currentApplications: Seq[(Seq[Term], Term, Int)],
      externalDepth: Int,
      currentInternalDepth: Int,
      currentSubstitutions: Possible
    ): Iterator[(Term, Possible)] = {
      def termVariable = for {
        currentApplicationsAsTermVariable <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[TermVariable])).traverseOption.iterator
        name <- currentApplicationsAsTermVariable.map { case (_, v, _) => v.name }.distinct.single
      } yield (TermVariable(name), currentSubstitutions)
      def unshiftedParameter = for {
        currentApplicationsAsFunctionParameter <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[FunctionParameter])).traverseOption.iterator
        parameter <- currentApplicationsAsFunctionParameter.map { case (_, p, _) => p }.distinct.single
        if parameter.level < currentInternalDepth
      } yield (parameter, currentSubstitutions)
      def shiftedParameter = for {
        currentApplicationsAsFunctionParameter <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[FunctionParameter])).traverseOption.iterator
        (index, level) <- currentApplicationsAsFunctionParameter.map { case (_, p, d) => (p.index, p.level - d) }.distinct.single
        if level >= currentInternalDepth
      } yield (FunctionParameter(index, level), currentSubstitutions)
      def argument = (0 until arity).mapCollect { i =>
        currentApplications.foldLeft(Option(currentSubstitutions)) { case (substitutionsSoFarOption, (arguments, application, internalDepth)) =>
          substitutionsSoFarOption.flatMap(s => arguments(i).insertExternalParameters(currentInternalDepth).calculateSubstitutions(application, s, currentInternalDepth + internalDepth, externalDepth))
        }.map(FunctionParameter(i, currentInternalDepth + externalDepth) -> _)
      }
      def definedTerm = for {
        currentApplicationsAsDefinedTerm <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[DefinedTerm])).traverseOption.iterator
        boundVariableNames = currentApplicationsAsDefinedTerm.head._2.scopedBoundVariableNames
        termDefinition <- currentApplicationsAsDefinedTerm.map { case (_, t, _) => t.definition }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(termDefinition.componentTypes.length, arity, currentApplicationsAsDefinedTerm.map(_.map2(_.components)), externalDepth, termDefinition.increaseDepth(currentInternalDepth), currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      } yield (DefinedTerm(expressions, termDefinition)(boundVariableNames), resultSubstitutions)
      def functionApplication = for {
        currentApplicationsAsFunctionApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[FunctionApplication])).traverseOption.iterator
        length = currentApplicationsAsFunctionApplication.head._2.arity
        variableName <- currentApplicationsAsFunctionApplication.map { case (_, t, _) => t.variableName }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(length, arity, currentApplicationsAsFunctionApplication.map(_.map2(_.arguments)), externalDepth, currentInternalDepth, currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
        terms = expressions.map(_.asInstanceOf[Term])
      } yield (FunctionApplication(variableName, terms), resultSubstitutions)

      termVariable ++ shiftedParameter ++ unshiftedParameter ++ argument ++ definedTerm ++ functionApplication
    }

    private def clearAtExpression(
      arity: Int,
      currentApplications: Seq[(Seq[Term], Expression, Int)],
      externalDepth: Int,
      currentInternalDepth: Int,
      currentSubstitutions: Possible
    ): Iterator[(Expression, Possible)] = {
      currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Statement])).traverseOption.map(clearAtStatement(arity, _, externalDepth, currentInternalDepth, currentSubstitutions)) orElse
        currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Term])).traverseOption.map(clearAtTerm(arity, _, externalDepth, currentInternalDepth, currentSubstitutions)) getOrElse
        Iterator.empty
    }

    private def clearAtExpressions(
      length: Int,
      arity: Int,
      currentApplications: Seq[(Seq[Term], Seq[Expression], Int)],
      externalDepth: Int,
      currentInternalDepth: Int,
      currentSubstitutions: Possible
    ): Iterator[(Seq[Expression], Possible)] = {
      (0 until length).foldLeft(Iterator((Seq.empty[Expression], currentSubstitutions))) { case (resultSoFar, i) =>
        resultSoFar.flatMap { case (expressionsSoFar, substitutionsSoFar) =>
          clearAtExpression(arity, currentApplications.map(_.map2(_(i))), externalDepth, currentInternalDepth, substitutionsSoFar)
            .map(_.mapLeft(expressionsSoFar :+ _))
        }
      }
    }

    @scala.annotation.tailrec
    private def clearApplicationsWherePossible(possible: Possible, externalDepth: Int): Option[Possible] = {
      @scala.annotation.tailrec
      def helper[T <: Expression](
        currentSubstitutions: Possible,
        applicationsByNameAndArity: Seq[((String, Int), Seq[(Seq[Term], T, Int)])],
        clear: (Int, Seq[(Seq[Term], T, Int)], Possible) => PossibleSingleMatch[(T, Possible)],
        lens: Lens[Substitutions.Possible, Map[(String, Int), T]],
        applicationLens: Lens[Substitutions.Possible, Map[(String, Int), Seq[(Seq[Term], T, Int)]]],
        retryNeeded: Boolean
      ): Option[(Possible, Boolean)] = {
        applicationsByNameAndArity match {
          case ((name, arity), applications) +: tail =>
            clear(arity, applications, currentSubstitutions) match {
              case NoMatches =>
                None
              case SingleMatch((value, resultingSubstitutions)) =>
                resultingSubstitutions.update((name, arity), value, lens) match {
                  case Some(result) =>
                    helper(result, tail, clear, lens, applicationLens, true)
                  case None =>
                    None
                }
              case MultipleMatches =>
                helper(currentSubstitutions.updateAdd((name, arity), applications, applicationLens), tail, clear, lens, applicationLens, retryNeeded)
            }
          case Nil =>
            Some((currentSubstitutions, retryNeeded))
        }
      }
      helper[Statement](possible.copy(predicateApplications = Map.empty), possible.predicateApplications.toSeq, clearAtStatement(_, _, externalDepth, 0, _).singleDistinctMatch, predicatesLens, predicateApplicationsLens, false) match {
        case Some((result, true)) =>
          clearApplicationsWherePossible(result, externalDepth)
        case Some((result, false)) =>
          helper[Term](result.copy(functionApplications = Map.empty), result.functionApplications.toSeq, clearAtTerm(_, _, externalDepth, 0, _).singleDistinctMatch, functionsLens, functionApplicationsLens, false) match {
            case Some((result, true)) =>
              clearApplicationsWherePossible(result, externalDepth)
            case Some((result, false)) =>
              Some(result)
            case None =>
              None
          }
        case None =>
          None
      }
    }

    implicit def substitutionsToPossibleSubstitutions(substitutions: Substitutions): Substitutions.Possible = {
      Substitutions.Possible(substitutions.statements, substitutions.terms, substitutions.predicates, substitutions.functions, Map.empty, Map.empty)
    }
  }
}
