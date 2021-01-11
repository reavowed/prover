package net.prover.model.substitutions

import monocle.Lens
import monocle.macros.GenLens
import net.prover._
import net.prover.core.expressions._
import net.prover.core.substitutions.Substitutions
import net.prover.core.transformers.{ContextWithInternalDepth, ParameterInserter}
import net.prover.model.VariableDefinitions
import net.prover.util.PossibleSingleMatch
import net.prover.util.PossibleSingleMatch.{MultipleMatches, NoMatches, SingleMatch}

case class PossibleSubstitutions(
  statements: Map[Int, Statement] = Map.empty,
  terms: Map[Int, Term] = Map.empty,
  statementApplications: Map[Int, Seq[(Seq[Term], Statement, Int)]] = Map.empty,
  termApplications: Map[Int, Seq[(Seq[Term], Term, Int)]] = Map.empty
) {
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

  def clearApplicationsWherePossible(implicit contextWithExternalDepth: ContextWithExternalDepth): Option[PossibleSubstitutions] = {
    PossibleSubstitutions.clearApplicationsWherePossible(this)
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
    currentSubstitutions: PossibleSubstitutions)(
    implicit internalContext: ContextWithInternalDepth,
    externalContext: ContextWithExternalDepth
  ): Iterator[(Statement, PossibleSubstitutions)] = {
    def statementVariable = for {
      currentApplicationsAsPredicateApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[StatementVariable])).traverseOption.iterator
      length = currentApplicationsAsPredicateApplication.head._2.arity
      index <- currentApplicationsAsPredicateApplication.map { case (_, t, _) => t.index }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(length, currentApplicationsAsPredicateApplication.map(_.map2(_.arguments)), currentSubstitutions)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      terms = expressions.map(_.asInstanceOf[Term])
    } yield (StatementVariable(index, terms), resultSubstitutions)

    def definedStatement = for {
      currentApplicationsAsDefinedStatement <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[CompoundStatement])).traverseOption.iterator
      boundVariableNames = currentApplicationsAsDefinedStatement.head._2.boundVariableNames
      statementDefinition <- currentApplicationsAsDefinedStatement.map { case (_, t, _) => t.definition }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(
        statementDefinition.componentTypes.length,
        currentApplicationsAsDefinedStatement.map(_.map2(_.components)),
        currentSubstitutions)(
        internalContext.increaseDepth(statementDefinition),
        implicitly)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
    } yield (CompoundStatement(statementDefinition, expressions)(boundVariableNames), resultSubstitutions)

    statementVariable ++ definedStatement
  }

  private def clearAtTerm(
    currentApplications: Seq[(Seq[Term], Term, Int)],
    currentSubstitutions: PossibleSubstitutions)(
    implicit internalContext: ContextWithInternalDepth,
    externalContext: ContextWithExternalDepth
  ): Iterator[(Term, PossibleSubstitutions)] = {
    def termVariable = for {
      currentApplicationsAsFunctionApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[TermVariable])).traverseOption.iterator
      length = currentApplicationsAsFunctionApplication.head._2.arity
      index <- currentApplicationsAsFunctionApplication.map { case (_, t, _) => t.index }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(length, currentApplicationsAsFunctionApplication.map(_.map2(_.arguments)), currentSubstitutions)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      terms = expressions.map(_.asInstanceOf[Term])
    } yield (TermVariable(index, terms), resultSubstitutions)

    def unshiftedParameter = for {
      currentApplicationsAsFunctionParameter <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Parameter])).traverseOption.iterator
      parameter <- currentApplicationsAsFunctionParameter.map { case (_, p, _) => p }.distinct.single
      if parameter.level < internalContext.internalDepth
    } yield (parameter, currentSubstitutions)

    def shiftedParameter = for {
      currentApplicationsAsFunctionParameter <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Parameter])).traverseOption.iterator
      (index, level) <- currentApplicationsAsFunctionParameter.map { case (_, p, d) => (p.index, p.level - d) }.distinct.single
      if level >= internalContext.internalDepth
    } yield (Parameter(index, level), currentSubstitutions)

    def argument = (0 until currentApplications.map(_._1.length).distinct.single.get).mapCollect { i =>
      currentApplications.foldLeft(Option(currentSubstitutions)) { case (substitutionsSoFarOption, (arguments, application, internalDepth)) =>
        substitutionsSoFarOption.flatMap(s =>
          SubstitutionCalculator.calculateFromExpressionWithContext(
            ParameterInserter.insertParameters(arguments(i), internalContext.internalDepth, 0),
            application,
            SubstitutionCalculationParameters(s, externalContext))(
            ContextWithInternalDepth(internalContext.internalDepth + internalDepth)))
      }.map(Parameter(i, internalContext.internalDepth + externalContext.externalDepth) -> _)
    }

    def compoundTerm = for {
      currentApplicationsAsDefinedTerm <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[CompoundTerm])).traverseOption.iterator
      boundVariableNames = currentApplicationsAsDefinedTerm.head._2.boundVariableNames
      termDefinition <- currentApplicationsAsDefinedTerm.map { case (_, t, _) => t.definition }.distinct.single.iterator
      expressionsAndResultSubstitutions <- clearAtExpressions(termDefinition.componentTypes.length, currentApplicationsAsDefinedTerm.map(_.map2(_.components)), currentSubstitutions)(internalContext.increaseDepth(termDefinition), implicitly)
      (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
    } yield (CompoundTerm(termDefinition, expressions)(boundVariableNames), resultSubstitutions)

    termVariable ++ shiftedParameter ++ unshiftedParameter ++ argument ++ compoundTerm
  }

  private def clearAtExpression(
    currentApplications: Seq[(Seq[Term], Expression, Int)],
    currentSubstitutions: PossibleSubstitutions)(
    implicit internalContext: ContextWithInternalDepth,
    contextWithExternalDepth: ContextWithExternalDepth
  ): Iterator[(Expression, PossibleSubstitutions)] = {
    currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Statement])).traverseOption.map(clearAtStatement(_, currentSubstitutions)) orElse
      currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[Term])).traverseOption.map(clearAtTerm(_, currentSubstitutions)) getOrElse
      Iterator.empty
  }

  private def clearAtExpressions(
    length: Int,
    currentApplications: Seq[(Seq[Term], Seq[Expression], Int)],
    currentSubstitutions: PossibleSubstitutions)(
    implicit internalContext: ContextWithInternalDepth,
    contextWithExternalDepth: ContextWithExternalDepth
  ): Iterator[(Seq[Expression], PossibleSubstitutions)] = {
    (0 until length).foldLeft(Iterator((Seq.empty[Expression], currentSubstitutions))) { case (resultSoFar, i) =>
      resultSoFar.flatMap { case (expressionsSoFar, substitutionsSoFar) =>
        clearAtExpression(currentApplications.map(_.map2(_ (i))), substitutionsSoFar)
          .map(_.mapLeft(expressionsSoFar :+ _))
      }
    }
  }

  @scala.annotation.tailrec
  private def clearApplicationsWherePossible(possible: PossibleSubstitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Option[PossibleSubstitutions] = {
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

    implicit val internalContext = ContextWithInternalDepth.zero
    helper[Statement](possible.copy(statementApplications = Map.empty), possible.statementApplications.toSeq, clearAtStatement(_, _).singleDistinctMatch, statementsLens, statementApplicationsLens, false) match {
      case Some((result, true)) =>
        clearApplicationsWherePossible(result)
      case Some((result, false)) =>
        helper[Term](result.copy(termApplications = Map.empty), result.termApplications.toSeq, clearAtTerm(_, _).singleDistinctMatch, termsLens, termApplicationsLens, false) match {
          case Some((result, true)) =>
            clearApplicationsWherePossible(result)
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
