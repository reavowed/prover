package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}
import net.prover.model.DistinctVariables.DistinctPair

case class DistinctVariables(distinctPairs: Set[DistinctPair]) extends JsonSerializable.Base {
  def ++(other: DistinctVariables): DistinctVariables = {
    DistinctVariables(distinctPairs ++ other.distinctPairs)
  }

  def get(variable: Variable): Variables = {
    distinctPairs.map(_.getMatch(variable).map(_.presentVariables).getOrElse(Variables.empty)).foldTogether
  }

  def areDistinct(first: Variable, second: Variable): Boolean = {
    distinctPairs.contains(DistinctPair(first, second))
  }

  def restrictTo(activeVariables: Variables): DistinctVariables = {
    DistinctVariables(distinctPairs.filter { pair =>
      activeVariables.contains(pair.first) && activeVariables.contains(pair.second)
    })
  }

  def applySubstitutions(substitutions: Substitutions): Option[DistinctVariables] = {
    distinctPairs
      .map { case DistinctPair(first, second) =>
        substituteSinglePair(first, second, substitutions)
      }
      .traverseOption
      .map(_.foldTogether)
  }

  private def substituteSinglePair(
    first: Variable,
    second: Variable,
    substitutions: Substitutions
  ): Option[DistinctVariables] = {
    for {
      substitutedFirst <- first.applySubstitutions(substitutions)
      substitutedSecond <- second.applySubstitutions(substitutions)
      result <- (
        for {
          newFirst <- substitutedFirst.presentVariables.all
          newSecond <- substitutedSecond.getPotentiallyIntersectingVariables(newFirst).all
        } yield {
          if (newFirst == newSecond) {
            None
          } else {
            Some(DistinctPair(newFirst, newSecond))
          }
        }
      ).traverseOption.map(DistinctVariables(_))
    } yield result
  }

  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartArray()
    distinctPairs.foreach { case DistinctPair(first, second) =>
      gen.writeStartArray()
      gen.writeString(first.toString)
      gen.writeString(second.toString)
      gen.writeEndArray()
    }
    gen.writeEndArray()
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

object DistinctVariables {
  val empty: DistinctVariables = DistinctVariables(Set.empty[DistinctPair])

  def apply(pairs: (Variable, Variable)*): DistinctVariables = {
    DistinctVariables(pairs.map(pair => DistinctPair(pair._1, pair._2)).toSet)
  }

  def apply(pair: (Variable, Variables)): DistinctVariables = {
    apply(pair._2.all.map(pair._1 -> _).toSeq: _*)
  }

  case class DistinctPair(first: Variable, second: Variable) {
    override def equals(obj: Any): Boolean = obj match {
      case DistinctPair(`first`, `second`) | DistinctPair(`second`, `first`) =>
        true
      case _ =>
        false
    }

    def getMatch(variable: Variable): Option[Variable] = {
      if (variable == first)
        Some(second)
      else if (variable == second)
        Some(first)
      else
        None
    }

    def isClash: Boolean = first == second
  }

  def byStatements(termVariables: Set[TermVariable], statements: Seq[Statement]): Option[DistinctVariables] = {
    val pairOptions = for {
      termVariable <- termVariables
      otherVariable <- statements.map(_.getPotentiallyIntersectingVariables(termVariable))
        .reduceOption(_ ++ _)
        .getOrElse(Variables.empty)
        .all
    } yield {
      if (termVariable == otherVariable)
        None
      else
        Some(DistinctPair(termVariable, otherVariable))
    }
    pairOptions.traverseOption.map(DistinctVariables(_))
  }

  implicit class DistinctVariableSeqOps(seq: Traversable[DistinctVariables]) {
    def foldTogether: DistinctVariables = {
      seq.foldLeft(DistinctVariables.empty)(_ ++ _)
    }
  }
}
