package net.prover.model

object Html {
  def format(text: String): String = {
    if (text.contains("_")) {
      val index = text.indexOf('_')
      s"${text.substring(0, index)}<sub>${text.substring(index + 1)}</sub>"
    } else if (text.contains("^")) {
      val index = text.indexOf('^')
      s"${text.substring(0, index)}<sup>${text.substring(index + 1)}</sup>"
    } else {
      text
    }
  }
}
