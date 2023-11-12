package net.prover.controllers

import net.prover.controllers.models.{PossibleConclusion, PossibleInference, PossibleInferenceWithTargets}
import net.prover.model._

import scala.collection.SortedSet

trait InferenceSearch {

  val NumberOfSuggestionsToReturn = 10

  case class InferenceWithMaximumPossibleComplexity(inference: Inference, maximumPossibleComplexity: Int, index: Int)
  case class PossibleInferenceWithMaximumMatchingComplexity(possibleInference: PossibleInference, matchedExpressionComplexity: Int, maximumMatchingComplexity: Int, minimumExtractionDepth: Int, index: Int)
  object PossibleInferenceWithMaximumMatchingComplexity {
    implicit val ordering: Ordering[PossibleInferenceWithMaximumMatchingComplexity] = Ordering.by(
      (i: PossibleInferenceWithMaximumMatchingComplexity) => (i.matchedExpressionComplexity, i.maximumMatchingComplexity, i.minimumExtractionDepth, i.index))(
      Ordering.Tuple4(Ordering.Int.reverse, Ordering.Int.reverse, Ordering.Int, Ordering.Int))
  }

  object +: {
    def unapply[T, C[_], Coll](t: Coll with scala.collection.IterableOps[T, C, Coll]): Option[(T, Coll)] =
      if(t.isEmpty) None
      else Some(t.head -> t.tail)
  }
  object Empty {
    def unapply[T, C[_], Coll](t: Coll with scala.collection.IterableOps[T, C, Coll]): Option[Unit] =
      if (t.isEmpty) Some(())
      else None
  }

  def getPossibleInferences(
    inferences: Seq[Inference],
    searchText: String,
    getPossibleInference: Inference => Option[PossibleInferenceWithTargets],
    getConclusionComplexity: PossibleConclusion => Int
  ): Seq[PossibleInference] = {

    val matchingInferences = filterInferences(inferences, searchText)
      .mapWithIndex((i, index) => InferenceWithMaximumPossibleComplexity(i, i.conclusion.structuralComplexity, index))
      .sortBy(_.maximumPossibleComplexity)(Ordering[Int].reverse)

    @scala.annotation.tailrec
    def recursivelyFindInferences(
      matchingInferences: Seq[InferenceWithMaximumPossibleComplexity],
      matchedInferences: Seq[PossibleInferenceWithMaximumMatchingComplexity],
      queuedInferences: SortedSet[PossibleInferenceWithMaximumMatchingComplexity]
    ): Seq[PossibleInference] = {
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
                possibleInference.possibleTargets.flatMap(_.possibleConclusions.map(_.extractionInferenceIds.length)).min,
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

  def filterInferences(inferences: Seq[Inference], searchText: String): Seq[Inference] = {
    inferences.filter(inferenceFilter(searchText))
  }
  def inferenceFilter(searchText: String): Inference => Boolean = {
    val searchWords = searchText.toLowerCase().splitByWhitespaceOrPunctuation().filter(_.nonEmpty)
    inference: Inference => matchWords(searchWords, inference.name.toLowerCase().splitByWhitespaceOrPunctuation().filter(_.nonEmpty))
  }
  private def matchWords(searchWords: Seq[String], titleWords: Seq[String]): Boolean = {
    if (searchWords.isEmpty)
      true
    else if (titleWords.isEmpty)
      false
    else if (searchWords.head.isEmpty)
      matchWords(searchWords.tail, titleWords)
    else if (titleWords.head.isEmpty)
      matchWords(searchWords, titleWords.tail)
    else if (searchWords.head(0) == titleWords.head(0)) {
      matchWords(
        searchWords.head.tail +: searchWords.tail,
        titleWords.head.tail +: titleWords.tail) ||
      matchWords(searchWords, titleWords.tail)
    } else {
      matchWords(searchWords, titleWords.tail)
    }
  }
}
