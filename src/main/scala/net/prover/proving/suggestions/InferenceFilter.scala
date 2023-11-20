package net.prover.proving.suggestions

import net.prover.model._
import net.prover.model.definitions.{NamingInference, TermRewriteInference}

case class InferenceFilter(searchText: String) {
  private val searchWords = searchText.toLowerCase().splitByWhitespaceOrPunctuation().filter(_.nonEmpty)

  def apply(inference: Inference): Boolean = {
     matchWords(searchWords, inference.name.toLowerCase().splitByWhitespaceOrPunctuation().filter(_.nonEmpty))
  }

  def apply(termRewriteInference: TermRewriteInference): Boolean = {
    apply(termRewriteInference.baseInference)
  }

  def apply(termRewriteInference: NamingInference): Boolean = {
    apply(termRewriteInference.baseInference)
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
