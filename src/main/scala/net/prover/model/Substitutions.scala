package net.prover.model

import monocle.Lens
import monocle.macros.GenLens
import net.prover.model.entries.ExpressionDefinition
import net.prover.model.expressions._

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
    predicateApplications: Map[(String, Int), Seq[(Seq[Term], Statement, Int, Int)]] = Map.empty,
    functionApplications: Map[(String, Int), Seq[(Seq[Term], Term, Int, Int)]] = Map.empty)
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
      expression: (Seq[Term], TExpression, Int, Int),
      lens: Lens[Substitutions.Possible, Map[TKey, Seq[(Seq[Term], TExpression, Int, Int)]]]
    ): Substitutions.Possible = {
      val map = lens.get(this)
      val newValue = map.getOrElse(key, Nil) :+ expression
      val newMap = map.updated(key, newValue)
      lens.set(newMap)(this)
    }

    def calculateApplicatives[T <: Expression](
      name: String,
      arity: Int,
      applications: Seq[(Seq[Term], T, Int, Int)],
      lens: Lens[Substitutions.Possible, Map[(String, Int), T]],
      applicationLens: Lens[Substitutions.Possible, Map[(String, Int), Seq[(Seq[Term], T, Int, Int)]]]
    ): Iterator[Possible] = {
      applications
        .foldLeft(Iterator(this)) { case (iterator, (arguments, target, internalDepth, externalDepth)) =>
          for {
            possibleSubstitutionsForThisPredicate <- iterator
            (applicative, applicativeSubstitutions) <- target.calculateApplicatives(arguments, possibleSubstitutionsForThisPredicate, 0, internalDepth, externalDepth)
            substitutionsWithApplicative <- applicativeSubstitutions.update((name, arity), applicative.asInstanceOf[T], lens)
          } yield substitutionsWithApplicative
        }
    }

    def clearApplicationsWherePossible(): Option[Substitutions.Possible] = {
      def clear[T <: Expression](
        substitutions: Substitutions.Possible,
        lens: Lens[Substitutions.Possible, Map[(String, Int), T]],
        applicationLens: Lens[Substitutions.Possible, Map[(String, Int), Seq[(Seq[Term], T, Int, Int)]]]
      ): Option[Substitutions.Possible] = {
        applicationLens.get(substitutions).toSeq.foldLeft(Option(applicationLens.set(Map.empty)(substitutions))) { case (possibleSubstitutionsSoFarOption, ((name, arity), applications)) =>
          possibleSubstitutionsSoFarOption.flatMap { possibleSubstitutionsSoFar =>
            val results = possibleSubstitutionsSoFar.calculateApplicatives(name, arity, applications, lens, applicationLens)
            results.take(2).toList match {
              case Nil =>
                None
              case Seq(single) =>
                Some(single)
              case more =>
                Some(applicationLens.set(applicationLens.get(possibleSubstitutionsSoFar) + ((name, arity) -> applications))(possibleSubstitutionsSoFar))
            }
          }
        }
      }

      for {
        afterPredicates <- clear(this, Possible.predicatesLens, Possible.predicateApplicationsLens)
        afterFunctions <- clear(afterPredicates, Possible.functionsLens, Possible.functionApplicationsLens)
      } yield afterFunctions
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

    implicit def substitutionsToPossibleSubstitutions(substitutions: Substitutions): Substitutions.Possible = {
      Substitutions.Possible(substitutions.statements, substitutions.terms, substitutions.predicates, substitutions.functions, Map.empty, Map.empty)
    }
  }
}
