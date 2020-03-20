package net.prover.controllers

import net.prover.model._

trait InferenceSearch {
  def filterInferences(inferences: Seq[Inference], searchText: String): Seq[Inference] = {
    inferences.filter(inferenceFilter(searchText))
  }
  def inferenceFilter(searchText: String): Inference => Boolean = {
    val searchWords = searchText.toLowerCase().splitByWhitespace().filter(_.nonEmpty)
    (inference: Inference) => matchWords(searchWords, inference.name.toLowerCase().splitByWhitespace().filter(_.nonEmpty))
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
