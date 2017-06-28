package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class DistinctVariables(conditions: Map[TermVariable, Variables]) extends JsonSerializable.Base {
  def +(first: TermVariable, second: Variable): DistinctVariables = {
    DistinctVariables(conditions.updated(first, get(first) + second))
  }

  def +(termVariable: TermVariable, variables: Variables): DistinctVariables = {
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

  def get(variable: TermVariable): Variables = {
    conditions.getOrElse(variable, Variables.empty)
  }

  def areDistinct(first: TermVariable, second: TermVariable): Boolean = {
    get(first).contains(second) || get(second).contains(first)
  }

  def areDistinct(first: TermVariable, second: Variable): Boolean = {
    get(first).contains(second)
  }

  def restrictTo(activeVariables: Variables): DistinctVariables = {
    val restrictedConditions = conditions
      .mapValues { v => v.intersect(activeVariables) }
      .filter { p => activeVariables.contains(p._1) && p._2.nonEmpty }
    DistinctVariables(restrictedConditions)
  }

  def applySubstitutions(substitutions: Substitutions): Option[DistinctVariables] = {
    conditions
      .map { case (termVariable, variables) =>
        for {
          updatedTermVariable <- termVariable.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
          updatedVariables <- for {
            substitutedStatementVariables <- variables.statementVariables.map(_.applySubstitutions(substitutions)).traverseOption
            substitutedTermVariables <- variables.termVariables.map(_.applySubstitutions(substitutions)).traverseOption
          } yield {
            (substitutedStatementVariables ++ substitutedTermVariables).map(_.getPotentiallyIntersectingVariables(updatedTermVariable)).foldTogether
          }
          if !updatedVariables.contains(updatedTermVariable)
        } yield (updatedTermVariable, updatedVariables)
      }
      .traverseOption
      .map(_.foldLeft(DistinctVariables.empty) { case (dv, (tv, vs)) =>
          dv + (tv, vs)
      })
  }

  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartArray()
    conditions.toSeq.sortBy(_._1.text).foreach { case (termVariable, variables) =>
      variables.all.toSeq.sortBy(_.text).foreach { variable =>
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
  val empty: DistinctVariables = DistinctVariables(Map.empty[TermVariable, Variables])

  def apply(pairs: (TermVariable, Variable)*): DistinctVariables = {
    pairs.foldLeft(DistinctVariables.empty) { case (dv, (tv, v)) => dv + (tv, v) }
  }

  def apply(pair: (TermVariable, Variables)): DistinctVariables = {
    DistinctVariables.empty + (pair._1, pair._2)
  }

  def byStatements(termVariables: Set[TermVariable], statements: Seq[Statement]): Option[DistinctVariables] = {
    termVariables.foldLeft(Option(DistinctVariables.empty)) { case (distinctVariablesOption, termVariable) =>
      distinctVariablesOption.flatMap { distinctVariables =>
        val newVariables = statements.map(_.getPotentiallyIntersectingVariables(termVariable)).foldTogether
        if (newVariables.contains(termVariable)) {
          None
        } else {
          Some(distinctVariables + (termVariable, newVariables))
        }
      }
    }
  }

  implicit class DistinctVariableSeqOps(seq: Traversable[DistinctVariables]) {
    def foldTogether: DistinctVariables = {
      seq.foldLeft(DistinctVariables.empty)(_ ++ _)
    }
  }
}
