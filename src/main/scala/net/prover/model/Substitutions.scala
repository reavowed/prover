package net.prover.model

import monocle.Lens
import net.prover.model.expressions._

case class Substitutions(
    statements: Map[String, Statement] = Map.empty,
    terms: Map[String, Term] = Map.empty,
    predicates: Map[String, Statement] = Map.empty,
    functions: Map[String, Term] = Map.empty)
{
  def update[T <: Expression](
    name: String,
    expression: T,
    lens: Lens[Substitutions, Map[String, T]],
    additionalDepth: Int
  ): Option[Substitutions] = {
    lens.get(this)
      .tryAdd(name, expression)
      .map(lens.set(_)(this))
  }
}

object Substitutions {
  val empty = Substitutions(Map.empty, Map.empty, Map.empty, Map.empty)

  case class Required(statements: Seq[String], terms: Seq[String], predicates: Seq[String], functions: Seq[String]) {
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
