package net.prover.model

import monocle.Lens
import net.prover.model.entries.ExpressionDefinition
import net.prover.model.expressions._

case class Substitutions(
    statements: Map[String, Statement] = Map.empty,
    terms: Map[String, Term] = Map.empty,
    predicates: Map[(String, Int), Statement] = Map.empty,
    functions: Map[(String, Int), Term] = Map.empty)
{
  def update[S, T <: Expression](
    name: S,
    expression: T,
    lens: Lens[Substitutions, Map[S, T]],
    additionalDepth: Int
  ): Option[Substitutions] = {
    lens.get(this)
      .tryAdd(name, expression)
      .map(lens.set(_)(this))
  }
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
}
