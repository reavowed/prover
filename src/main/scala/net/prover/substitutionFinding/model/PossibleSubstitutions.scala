package net.prover.substitutionFinding.model

import monocle.Lens
import monocle.macros.GenLens
import net.prover._
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.expressions._
import net.prover.model.{Substitutions, VariableDefinitions}
import net.prover.old.OldParameterInserter
import net.prover.substitutionFinding.transformers.{PossibleSubstitutionCalculationParameters, PossibleSubstitutionCalculator}
import net.prover.util.PossibleSingleMatch
import net.prover.util.PossibleSingleMatch._

case class PossibleSubstitutions(
  statements: Map[Int, Statement] = Map.empty,
  terms: Map[Int, Term] = Map.empty,
  statementApplications: Map[Int, Seq[(Seq[Term], Statement, Int)]] = Map.empty,
  termApplications: Map[Int, Seq[(Seq[Term], Term, Int)]] = Map.empty)
{
  def update[TExpression](
    index: Int,
    expression: TExpression,
    lens: Lens[PossibleSubstitutions, Map[Int, TExpression]]
  ): Option[PossibleSubstitutions] = {
    lens.get(this)
      .tryAdd(index, expression)
      .map(lens.set(_)(this))
  }
  def updateAdd[TKey, TExpression](
    index: Int,
    newApplication: (Seq[Term], TExpression, Int),
    lens: Lens[PossibleSubstitutions, Map[Int, Seq[(Seq[Term], TExpression, Int)]]]
  ): Option[PossibleSubstitutions] = {
    updateAdd(index, Seq(newApplication), lens)
  }
  def updateAdd[TKey, TExpression](
    index: Int,
    newApplications: Seq[(Seq[Term], TExpression, Int)],
    lens: Lens[PossibleSubstitutions, Map[Int, Seq[(Seq[Term], TExpression, Int)]]]
  ): Option[PossibleSubstitutions] = {
    val currentMap = lens.get(this)
    val currentApplications = currentMap.getOrElse(index, Nil)
    val newMap = currentMap.updated(index, currentApplications ++ newApplications)
    Some(lens.set(newMap)(this))
  }

  def clearApplicationsWherePossible(externalDepth: Int): Option[PossibleSubstitutions] = {
    PossibleSubstitutions.clearApplicationsWherePossible(this, externalDepth)
  }

  def confirmTotality(variableDefinitions: VariableDefinitions): Option[Substitutions] = {
    for {
      statements <- variableDefinitions.statements.indices.map(i => statements.get(i)).traverseOption
      terms <- variableDefinitions.terms.indices.map(i => terms.get(i)).traverseOption
    } yield Substitutions(statements, terms)
  }
}

object PossibleSubstitutions {

  def empty: PossibleSubstitutions = PossibleSubstitutions(Map.empty, Map.empty, Map.empty, Map.empty)
  val statementsLens = GenLens[PossibleSubstitutions](_.statements)
  val termsLens = GenLens[PossibleSubstitutions](_.terms)
  val statementApplicationsLens = GenLens[PossibleSubstitutions](_.statementApplications)
  val termApplicationsLens = GenLens[PossibleSubstitutions](_.termApplications)

  private def clearAtStatement(
    currentApplications: Seq[(Seq[Term], Statement, Int)],
    externalDepth: Int,
    currentInternalDepth: Int,
    currentSubstitutions: PossibleSubstitutions
  ): Iterator[(Statement, PossibleSubstitutions)] = {
    def statementVariable = for {
      currentApplicationsAsPredicateApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[StatementVariable])).traverseOption.iterator
      length = currentApplicationsAsPredicateApplication.head._2.arity
      index <- currentApplicationsAsPredicateApplication.map { case (_, t, _) => t.index }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(length, currentApplicationsAsPredicateApplication.map(_.map2(_.arguments)), externalDepth, currentInternalDepth, currentSubstitutions)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      terms = expressions.map(_.asInstanceOf[Term])
    } yield (StatementVariable(index, terms), resultSubstitutions)
    def definedStatement = for {
      currentApplicationsAsDefinedStatement <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[DefinedStatement])).traverseOption.iterator
      boundVariableNames = currentApplicationsAsDefinedStatement.head._2.boundVariableNames
      statementDefinition <- currentApplicationsAsDefinedStatement.map { case (_, t, _) => t.definition }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(statementDefinition.componentTypes.length, currentApplicationsAsDefinedStatement.map(_.map2(_.components)), externalDepth, statementDefinition.increaseDepth(currentInternalDepth), currentSubstitutions)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
    } yield (DefinedStatement(expressions, statementDefinition)(boundVariableNames), resultSubstitutions)

    statementVariable ++ definedStatement
  }

  private def clearAtTerm(
    currentApplications: Seq[(Seq[Term], Term, Int)],
    externalDepth: Int,
    currentInternalDepth: Int,
    currentSubstitutions: PossibleSubstitutions
  ): Iterator[(Term, PossibleSubstitutions)] = {
    def termVariable = for {
      currentApplicationsAsFunctionApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[TermVariable])).traverseOption.iterator
      length = currentApplicationsAsFunctionApplication.head._2.arity
      index <- currentApplicationsAsFunctionApplication.map { case (_, t, _) => t.index }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(length, currentApplicationsAsFunctionApplication.map(_.map2(_.arguments)), externalDepth, currentInternalDepth, currentSubstitutions)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      terms = expressions.map(_.asInstanceOf[Term])
    } yield (TermVariable(index, terms), resultSubstitutions)
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
    def argument = (0 until currentApplications.map(_._1.length).distinct.single.get).mapCollect { i =>
      currentApplications.foldLeft(Option(currentSubstitutions)) { case (substitutionsSoFarOption, (arguments, application, internalDepth)) =>
        substitutionsSoFarOption.flatMap(s =>
          PossibleSubstitutionCalculator.calculateFromExpressionWithContext(
            OldParameterInserter.insertParameters(arguments(i), currentInternalDepth, 0),
            application,
            PossibleSubstitutionCalculationParameters(s, ContextWithExternalDepth(externalDepth)))(
            ContextWithInternalDepth(currentInternalDepth + internalDepth)))
      }.map(FunctionParameter(i, currentInternalDepth + externalDepth) -> _)
    }
    def definedTerm = for {
      currentApplicationsAsDefinedTerm <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[DefinedTerm])).traverseOption.iterator
      boundVariableNames = currentApplicationsAsDefinedTerm.head._2.boundVariableNames
      termDefinition <- currentApplicationsAsDefinedTerm.map { case (_, t, _) => t.definition }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(termDefinition.componentTypes.length, currentApplicationsAsDefinedTerm.map(_.map2(_.components)), externalDepth, termDefinition.increaseDepth(currentInternalDepth), currentSubstitutions)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
    } yield (DefinedTerm(expressions, termDefinition)(boundVariableNames), resultSubstitutions)

    termVariable ++ shiftedParameter ++ unshiftedParameter ++ argument ++ definedTerm
  }

  private def clearAtExpression(
    currentApplications: Seq[(Seq[Term], Expression, Int)],
    externalDepth: Int,
    currentInternalDepth: Int,
    currentSubstitutions: PossibleSubstitutions
  ): Iterator[(Expression, PossibleSubstitutions)] = {
    currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Statement])).traverseOption.map(clearAtStatement(_, externalDepth, currentInternalDepth, currentSubstitutions)) orElse
      currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Term])).traverseOption.map(clearAtTerm(_, externalDepth, currentInternalDepth, currentSubstitutions)) getOrElse
      Iterator.empty
  }

  private def clearAtExpressions(
    length: Int,
    currentApplications: Seq[(Seq[Term], Seq[Expression], Int)],
    externalDepth: Int,
    currentInternalDepth: Int,
    currentSubstitutions: PossibleSubstitutions
  ): Iterator[(Seq[Expression], PossibleSubstitutions)] = {
    (0 until length).foldLeft(Iterator((Seq.empty[Expression], currentSubstitutions))) { case (resultSoFar, i) =>
      resultSoFar.flatMap { case (expressionsSoFar, substitutionsSoFar) =>
        clearAtExpression(currentApplications.map(_.map2(_(i))), externalDepth, currentInternalDepth, substitutionsSoFar)
          .map(_.mapLeft(expressionsSoFar :+ _))
      }
    }
  }

  @scala.annotation.tailrec
  private def clearApplicationsWherePossible(possible: PossibleSubstitutions, externalDepth: Int): Option[PossibleSubstitutions] = {
    @scala.annotation.tailrec
    def helper[T <: Expression](
      currentSubstitutions: PossibleSubstitutions,
      applicationsByIndex: Seq[(Int, Seq[(Seq[Term], T, Int)])],
      clear: (Seq[(Seq[Term], T, Int)], PossibleSubstitutions) => PossibleSingleMatch[(T, PossibleSubstitutions)],
      lens: Lens[PossibleSubstitutions, Map[Int, T]],
      applicationLens: Lens[PossibleSubstitutions, Map[Int, Seq[(Seq[Term], T, Int)]]],
      retryNeeded: Boolean
    ): Option[(PossibleSubstitutions, Boolean)] = {
      applicationsByIndex match {
        case (index, applications) +: tail =>
          clear(applications, currentSubstitutions) match {
            case NoMatches =>
              None
            case SingleMatch((value, resultingSubstitutions)) =>
              resultingSubstitutions.update(index, value, lens) match {
                case Some(result) =>
                  helper(result, tail, clear, lens, applicationLens, true)
                case None =>
                  None
              }
            case MultipleMatches =>
              currentSubstitutions.updateAdd(index, applications, applicationLens) match {
                case Some(substitutions) =>
                  helper(substitutions, tail, clear, lens, applicationLens, retryNeeded)
                case None =>
                  None
              }
          }
        case Nil =>
          Some((currentSubstitutions, retryNeeded))
      }
    }
    helper[Statement](possible.copy(statementApplications = Map.empty), possible.statementApplications.toSeq, clearAtStatement( _, externalDepth, 0, _).singleDistinctMatch, statementsLens, statementApplicationsLens, false) match {
      case Some((result, true)) =>
        clearApplicationsWherePossible(result, externalDepth)
      case Some((result, false)) =>
        helper[Term](result.copy(termApplications = Map.empty), result.termApplications.toSeq, clearAtTerm(_, externalDepth, 0, _).singleDistinctMatch, termsLens, termApplicationsLens, false) match {
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
}
