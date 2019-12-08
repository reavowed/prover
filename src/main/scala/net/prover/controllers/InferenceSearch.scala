package net.prover.controllers

import net.prover.model._

trait InferenceSearch {
  def filterInferences(inferences: Seq[Inference], searchText: String): Seq[Inference] = {
    inferences.filter(inferenceFilter(searchText))
  }
  def inferenceFilter(searchText: String): Inference => Boolean = {
    val searchWords = searchText.toLowerCase().splitByWhitespace()
    (inference: Inference) => searchWords.forall(inference.name.toLowerCase.contains)
  }
}
