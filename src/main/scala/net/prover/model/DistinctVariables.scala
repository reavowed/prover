package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class DistinctVariables(conditions: Map[TermVariable, Set[Variable]]) extends JsonSerializable.Base {
  def +(first: TermVariable, second: Variable): DistinctVariables = {
    DistinctVariables(conditions.updated(first, get(first) + second))
  }

  def +(termVariable: TermVariable, variables: Set[Variable]): DistinctVariables = {
    if (variables.isEmpty) {
      this
    } else {
      DistinctVariables(conditions.updated(termVariable, get(termVariable) ++ variables))
    }
  }

  def ++(other: DistinctVariables): DistinctVariables = {
    val updatedConditions = (conditions.keySet ++ other.conditions.keySet).map { v =>
      v -> (get(v) ++ other.get(v))
    }.toMap
    DistinctVariables(updatedConditions)
  }

  def --(other: DistinctVariables): DistinctVariables = {
    val updatedConditions = (conditions.keySet ++ other.conditions.keySet)
      .map { v =>
        v -> (get(v) -- other.get(v))
      }
      .filter { case (_, vs) =>
        vs.nonEmpty
      }
      .toMap
    DistinctVariables(updatedConditions)
  }

  def get(variable: TermVariable): Set[Variable] = {
    conditions.getOrElse(variable, Set.empty)
  }

  def areDistinct(first: TermVariable, second: TermVariable): Boolean = {
    get(first).contains(second) || get(second).contains(first)
  }

  def areDistinct(first: TermVariable, second: Variable): Boolean = {
    get(first).contains(second)
  }

  def restrictTo(statements: Seq[Statement]): DistinctVariables = {
    val activeVariables = statements.flatMap(_.allVariables).toSet
    val restrictedConditions = conditions
      .filterKeys { activeVariables.contains }
      .mapValues { _.intersect(activeVariables) }
      .map { case (termVariable, variables) =>
        termVariable -> variables.filter { variable =>
          statements.exists { s =>
            !s.boundVariables.contains(termVariable) && (s.presentVariables.contains(termVariable) || s.presentVariables.contains(variable))
          }
        }
      }
      .filter { p => p._2.nonEmpty }
    DistinctVariables(restrictedConditions)
  }

  def applySubstitutions(substitutions: Substitutions): Option[DistinctVariables] = {
    conditions
      .map { case (termVariable, variables) =>
        for {
          updatedTermVariable <- termVariable.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
          substitutedVariables <- variables.map(_.applySubstitutions(substitutions)).traverseOption
          updatedVariables = substitutedVariables.flatMap(_.getPotentiallyIntersectingVariables(updatedTermVariable))
          if !updatedVariables.contains(updatedTermVariable)
        } yield (updatedTermVariable, updatedVariables)
      }
      .traverseOption
      .map(_.map { case (termVariable, variables) =>
        DistinctVariables(termVariable -> variables)
      })
      .map(_.foldTogether)
  }

  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartArray()
    conditions.toSeq.sortBy(_._1.text).foreach { case (termVariable, variables) =>
      variables.toSeq.sortBy(_.text).foreach { variable =>
        gen.writeStartArray()
        gen.writeString(termVariable.html)
        gen.writeString(variable.html)
        gen.writeEndArray()
      }
    }
    gen.writeEndArray()
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

object DistinctVariables {
  val empty: DistinctVariables = DistinctVariables(Map.empty[TermVariable, Set[Variable]])

  def apply(pairs: (TermVariable, Variable)*): DistinctVariables = {
    pairs.foldLeft(DistinctVariables.empty) { case (dv, (tv, v)) => dv + (tv, v) }
  }

  def apply(pair: (TermVariable, Set[Variable])): DistinctVariables = {
    DistinctVariables.empty + (pair._1, pair._2)
  }

  def byStatements(termVariables: Set[TermVariable], statements: Seq[Statement]): Option[DistinctVariables] = {
    termVariables
      .map { termVariable =>
        val newVariables = statements.flatMap(_.getPotentiallyIntersectingVariables(termVariable)).toSet
        if (newVariables.contains(termVariable)) {
          None
        } else {
          Some(DistinctVariables(termVariable -> newVariables))
        }
      }
      .traverseOption
      .map(_.foldTogether)
  }

  implicit class DistinctVariableSeqOps(seq: Traversable[DistinctVariables]) {
    def foldTogether: DistinctVariables = {
      seq.foldLeft(DistinctVariables.empty)(_ ++ _)
    }
  }
}
