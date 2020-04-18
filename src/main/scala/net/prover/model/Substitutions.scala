package net.prover.model

import monocle.Lens
import monocle.macros.GenLens
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions._
import net.prover.util.PossibleSingleMatch
import net.prover.util.PossibleSingleMatch._

case class Substitutions(
    statements: Map[String, (Int, Statement)] = Map.empty,
    terms: Map[String, (Int, Term)] = Map.empty)
{
  def isEmpty: Boolean = statements.isEmpty && terms.isEmpty

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Substitutions = {
    Substitutions(
      statements.mapValues(_.mapRight(_.insertExternalParameters(numberOfParametersToInsert, internalDepth))),
      terms.mapValues(_.mapRight(_.insertExternalParameters(numberOfParametersToInsert, internalDepth))))
  }
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Substitutions] = {
    for {
      newStatements <- statements.mapValues(_.optionMapRight(_.removeExternalParameters(numberOfParametersToRemove, internalDepth))).traverseOption
      newTerms <- terms.mapValues(_.optionMapRight(_.removeExternalParameters(numberOfParametersToRemove, internalDepth))).traverseOption
    } yield Substitutions(newStatements, newTerms)
  }
  def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Substitutions = {
    Substitutions(
      statements.mapValues(_.mapRight(_.replaceDefinition(oldDefinition, newDefinition))),
      terms.mapValues(_.mapRight(_.replaceDefinition(oldDefinition, newDefinition))))
  }
}

object Substitutions {
  val empty = Substitutions(Map.empty, Map.empty)

  case class Required(statements: Seq[(String, Int)], terms: Seq[(String, Int)]) {
    def isEmpty: Boolean = statements.isEmpty && terms.isEmpty

    def ++(other: Required): Required = {
      Required(
        (statements ++ other.statements).distinct,
        (terms ++ other.terms).distinct)
    }
    def isEquivalentTo(other: Required): Boolean = {
      statements.toSet == other.statements.toSet &&
        terms.toSet == other.terms.toSet
    }
    def contains(other: Required): Boolean = {
      other.statements.toSet.subsetOf(statements.toSet) &&
        other.terms.toSet.subsetOf(terms.toSet)
    }

    def hasNoApplications: Boolean = {
      statements.forall(_._2 == 0) && terms.forall(_._2 == 0)
    }

    def fill(statements: Seq[Statement], terms: Seq[Term]): Substitutions = {
      Substitutions(
        this.statements.zip(statements).map { case ((name, arity), statement) => (name, (arity, statement)) }.toMap,
        this.terms.zip(terms).map { case ((name, arity), term) => (name, (arity, term)) }.toMap)
    }

    def filterSubstitutions(substitutions: Substitutions): Substitutions = {
      Substitutions(
        statements = substitutions.statements.filterKeys(s => statements.exists(_._1 == s)),
        terms = substitutions.terms.filterKeys(t => terms.exists(_._1 == t)))
    }
  }

  object Required {
    val empty = Required(Seq.empty, Seq.empty)
    implicit class RequiredSeqOps(seq: Seq[Required]) {
      def foldTogether: Required = {
        seq.fold(empty)(_ ++ _)
      }
    }
  }

  case class Possible(
    statements: Map[String, (Int, Statement)] = Map.empty,
    terms: Map[String, (Int, Term)] = Map.empty,
    statementApplications: Map[String, (Int, Seq[(Seq[Term], Statement, Int)])] = Map.empty,
    termApplications: Map[String, (Int, Seq[(Seq[Term], Term, Int)])] = Map.empty)
  {
    def update[TExpression](
      name: String,
      arity: Int,
      expression: TExpression,
      lens: Lens[Substitutions.Possible, Map[String, (Int, TExpression)]]
    ): Option[Substitutions.Possible] = {
      lens.get(this)
        .tryAdd(name, (arity, expression))
        .map(lens.set(_)(this))
    }
    def updateAdd[TKey, TExpression](
      name: String,
      arity: Int,
      application: (Seq[Term], TExpression, Int),
      lens: Lens[Substitutions.Possible, Map[String, (Int, Seq[(Seq[Term], TExpression, Int)])]]
    ): Option[Substitutions.Possible] = {
      updateAdd(name, arity, Seq(application), lens)
    }
    def updateAdd[TKey, TExpression](
      name: String,
      arity: Int,
      applications: Seq[(Seq[Term], TExpression, Int)],
      lens: Lens[Substitutions.Possible, Map[String, (Int, Seq[(Seq[Term], TExpression, Int)])]]
    ): Option[Substitutions.Possible] = {
      val map = lens.get(this)
      val (currentArity, currentApplications) = map.getOrElse(name, (arity, Nil))
      if (currentArity == arity) {
        val newValue = (arity, currentApplications ++ applications)
        val newMap = map.updated(name, newValue)
        Some(lens.set(newMap)(this))
      } else {
        None
      }
    }

    def clearApplicationsWherePossible(externalDepth: Int): Option[Substitutions.Possible] = {
      Possible.clearApplicationsWherePossible(this, externalDepth)
    }

    def stripApplications(): Substitutions = Substitutions(statements, terms)

    def confirmTotality: Option[Substitutions] = {
      if (statementApplications.isEmpty && termApplications.isEmpty)
        Some(stripApplications())
      else
        None
    }
  }
  object Possible {

    val empty = Possible(Map.empty, Map.empty, Map.empty, Map.empty)
    val statementsLens = GenLens[Substitutions.Possible](_.statements)
    val termsLens = GenLens[Substitutions.Possible](_.terms)
    val statementApplicationsLens = GenLens[Substitutions.Possible](_.statementApplications)
    val termApplicationsLens = GenLens[Substitutions.Possible](_.termApplications)

    private def clearAtStatement(
      arity: Int,
      currentApplications: Seq[(Seq[Term], Statement, Int)],
      externalDepth: Int,
      currentInternalDepth: Int,
      currentSubstitutions: Possible
    ): Iterator[(Statement, Possible)] = {
      def statementVariable = for {
        currentApplicationsAsPredicateApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[StatementVariable])).traverseOption.iterator
        length = currentApplicationsAsPredicateApplication.head._2.arity
        variableName <- currentApplicationsAsPredicateApplication.map { case (_, t, _) => t.name }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(length, arity, currentApplicationsAsPredicateApplication.map(_.map2(_.arguments)), externalDepth, currentInternalDepth, currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
        terms = expressions.map(_.asInstanceOf[Term])
      } yield (StatementVariable(variableName, terms), resultSubstitutions)
      def definedStatement = for {
        currentApplicationsAsDefinedStatement <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[DefinedStatement])).traverseOption.iterator
        boundVariableNames = currentApplicationsAsDefinedStatement.head._2.boundVariableNames
        statementDefinition <- currentApplicationsAsDefinedStatement.map { case (_, t, _) => t.definition }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(statementDefinition.componentTypes.length, arity, currentApplicationsAsDefinedStatement.map(_.map2(_.components)), externalDepth, statementDefinition.increaseDepth(currentInternalDepth), currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      } yield (DefinedStatement(expressions, statementDefinition)(boundVariableNames), resultSubstitutions)

      statementVariable ++ definedStatement
    }

    private def clearAtTerm(
      arity: Int,
      currentApplications: Seq[(Seq[Term], Term, Int)],
      externalDepth: Int,
      currentInternalDepth: Int,
      currentSubstitutions: Possible
    ): Iterator[(Term, Possible)] = {
      def termVariable = for {
        currentApplicationsAsFunctionApplication <- currentApplications.map(_.optionMap2(_.asOptionalInstanceOf[TermVariable])).traverseOption.iterator
        length = currentApplicationsAsFunctionApplication.head._2.arity
        variableName <- currentApplicationsAsFunctionApplication.map { case (_, t, _) => t.name }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(length, arity, currentApplicationsAsFunctionApplication.map(_.map2(_.arguments)), externalDepth, currentInternalDepth, currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
        terms = expressions.map(_.asInstanceOf[Term])
      } yield (TermVariable(variableName, terms), resultSubstitutions)
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
        boundVariableNames = currentApplicationsAsDefinedTerm.head._2.boundVariableNames
        termDefinition <- currentApplicationsAsDefinedTerm.map { case (_, t, _) => t.definition }.distinct.single.iterator
        expressionsAndResultSubstitutions <- clearAtExpressions(termDefinition.componentTypes.length, arity, currentApplicationsAsDefinedTerm.map(_.map2(_.components)), externalDepth, termDefinition.increaseDepth(currentInternalDepth), currentSubstitutions)
        (expressions, resultSubstitutions) = expressionsAndResultSubstitutions
      } yield (DefinedTerm(expressions, termDefinition)(boundVariableNames), resultSubstitutions)

      termVariable ++ shiftedParameter ++ unshiftedParameter ++ argument ++ definedTerm
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
        applicationsByName: Seq[(String, (Int, Seq[(Seq[Term], T, Int)]))],
        clear: (Int, Seq[(Seq[Term], T, Int)], Possible) => PossibleSingleMatch[(T, Possible)],
        lens: Lens[Substitutions.Possible, Map[String, (Int, T)]],
        applicationLens: Lens[Substitutions.Possible, Map[String, (Int, Seq[(Seq[Term], T, Int)])]],
        retryNeeded: Boolean
      ): Option[(Possible, Boolean)] = {
        applicationsByName match {
          case (name, (arity, applications)) +: tail =>
            clear(arity, applications, currentSubstitutions) match {
              case NoMatches =>
                None
              case SingleMatch((value, resultingSubstitutions)) =>
                resultingSubstitutions.update(name, arity, value, lens) match {
                  case Some(result) =>
                    helper(result, tail, clear, lens, applicationLens, true)
                  case None =>
                    None
                }
              case MultipleMatches =>
                currentSubstitutions.updateAdd(name, arity, applications, applicationLens) match {
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
      helper[Statement](possible.copy(statementApplications = Map.empty), possible.statementApplications.toSeq, clearAtStatement(_, _, externalDepth, 0, _).singleDistinctMatch, statementsLens, statementApplicationsLens, false) match {
        case Some((result, true)) =>
          clearApplicationsWherePossible(result, externalDepth)
        case Some((result, false)) =>
          helper[Term](result.copy(termApplications = Map.empty), result.termApplications.toSeq, clearAtTerm(_, _, externalDepth, 0, _).singleDistinctMatch, termsLens, termApplicationsLens, false) match {
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
      Substitutions.Possible(substitutions.statements, substitutions.terms, Map.empty, Map.empty)
    }
  }

  trait Lenses[TExpression <: Expression] {
    def substitutionsLens: Lens[Substitutions, Map[String, (Int, TExpression)]]
    def possibleSubstitutionsLens: Lens[Substitutions.Possible, Map[String, (Int, TExpression)]]
    def possibleSubstitutionsApplicationsLens: Lens[Substitutions.Possible, Map[String, (Int, Seq[(Seq[Term], TExpression, Int)])]]
    def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[(String, Int)]]

    def fillRequiredSubstitutions(required: Required, expressions: Seq[TExpression]): Substitutions = {
      substitutionsLens.set(
        requiredSubstitutionsLens.get(required)
          .zip(expressions)
          .map { case ((name, arity), expression) => (name, (arity, expression)) }
          .toMap)(
        Substitutions.empty)
    }
    def getRequiredSubstitutions(possible: Possible, required: Required): Option[Seq[TExpression]] = {
      requiredSubstitutionsLens.get(required)
        .map { case (name, arity) => possibleSubstitutionsLens.get(possible).get(name).filter(_._1 == arity).map(_._2) }
        .traverseOption
    }
  }

  object Lenses {
    trait ForStatements extends Lenses[Statement] {
      override def substitutionsLens = GenLens[Substitutions](_.statements)
      override def possibleSubstitutionsLens = Possible.statementsLens
      override def possibleSubstitutionsApplicationsLens = Possible.statementApplicationsLens
      override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.statements)
    }
    trait ForTerms extends Lenses[Term] {
      override def substitutionsLens = GenLens[Substitutions](_.terms)
      override def possibleSubstitutionsLens = Possible.termsLens
      override def possibleSubstitutionsApplicationsLens = Possible.termApplicationsLens
      override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.terms)
    }
  }
}
