package net.prover.proving.suggestions

import net.prover.controllers.models.*
import net.prover.model.*

import scala.collection.immutable.SortedSet

object SuggestInferences {

  val NumberOfSuggestionsToReturn = 10

  private case class InferenceWithMaximumPossibleComplexity(inference: Inference, maximumPossibleComplexity: Int, index: Int)
  private case class PossibleInferenceWithMaximumMatchingComplexity(possibleInference: PossibleInferenceWithTargets, matchedExpressionComplexity: Int, maximumMatchingComplexity: Int, minimumExtractionDepth: Int, index: Int)
  private object PossibleInferenceWithMaximumMatchingComplexity {
    implicit val ordering: Ordering[PossibleInferenceWithMaximumMatchingComplexity] = {
      Ordering.by(
        (i: PossibleInferenceWithMaximumMatchingComplexity) => (i.matchedExpressionComplexity, i.maximumMatchingComplexity, i.minimumExtractionDepth, i.index))(
        Ordering.Tuple4(Ordering.Int.reverse, Ordering.Int.reverse, Ordering.Int, Ordering.Int))
    }
  }

  object +: {
    def unapply[T, C[_], Coll](t: Coll with scala.collection.IterableOps[T, C, Coll]): Option[(T, Coll)] =
      if (t.isEmpty) None
      else Some(t.head -> t.tail)
  }
  object Empty {
    def unapply[T, C[_], Coll](t: Coll with scala.collection.IterableOps[T, C, Coll]): Option[Unit] =
      if (t.isEmpty) Some(())
      else None
  }

  def apply(
    searchText: String,
    getPossibleInference: Inference => Option[PossibleInferenceWithTargets],
    getConclusionComplexity: PossibleConclusion => Int)(
    implicit provingContext: ProvingContext
  ): Seq[PossibleInferenceWithTargets] = {

    val matchingInferences = provingContext.availableEntries.allInferences.filter(InferenceFilter(searchText).apply)
      .mapWithIndex((i, index) => InferenceWithMaximumPossibleComplexity(i, i.conclusion.structuralComplexity, index))
      .sortBy(_.maximumPossibleComplexity)(Ordering[Int].reverse)

    @scala.annotation.tailrec
    def recursivelyFindInferences(
      matchingInferences: Seq[InferenceWithMaximumPossibleComplexity],
      matchedInferences: Seq[PossibleInferenceWithMaximumMatchingComplexity],
      queuedInferences: SortedSet[PossibleInferenceWithMaximumMatchingComplexity]
    ): Seq[PossibleInferenceWithTargets] = {
      if (matchedInferences.size >= NumberOfSuggestionsToReturn) { // We've already found the required number of matches
        matchedInferences.map(_.possibleInference)
      } else (matchingInferences, queuedInferences) match {
        case (matchHead +: _, queueHead +: queueTail) if queueHead.maximumMatchingComplexity > matchHead.maximumPossibleComplexity =>
          recursivelyFindInferences(matchingInferences, matchedInferences :+ queueHead, queueTail)
        case (matchHead +: matchTail, _) =>
          getPossibleInference(matchHead.inference) match {
            case Some(possibleInference) =>
              val possibleInferenceWithComplexity = PossibleInferenceWithMaximumMatchingComplexity(
                possibleInference,
                possibleInference.possibleTargets.map(_.target.structuralComplexity).max,
                possibleInference.possibleTargets.flatMap(_.possibleConclusions.map(getConclusionComplexity)).max,
                possibleInference.possibleTargets.flatMap(_.possibleConclusions.map(_.extractionDefinition.depth)).min,
                matchHead.index)
              recursivelyFindInferences(matchTail, matchedInferences, queuedInferences + possibleInferenceWithComplexity)
            case None =>
              recursivelyFindInferences(matchTail, matchedInferences, queuedInferences)
          }
        case (Empty(_), _) =>
          (matchedInferences ++ queuedInferences.take(NumberOfSuggestionsToReturn - matchedInferences.length)).map(_.possibleInference)
      }
    }

    recursivelyFindInferences(
      matchingInferences,
      Nil,
      SortedSet.empty)
  }
}
