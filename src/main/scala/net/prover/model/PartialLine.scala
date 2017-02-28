package net.prover.model

case class PartialLine(remainingText: String, fullLine: BookLine) {
  def tail: PartialLine = {
    copy(remainingText = remainingText.tail.trim())
  }
  def toEndOfParens: (String, PartialLine) = {
    val (_, pre, post) = remainingText.foldLeft((1, "", "")) { case ((count, pre, post), c) =>
      if (count == 0 || (count == 1 &&  c == ')'))
        (0, pre, post + c)
      else {
        val newCount = if (c == '(')
          count + 1
        else if (c == ')')
          count - 1
        else
          count
        (newCount, pre + c, post)
      }
    }
    (pre, PartialLine(post, fullLine))
  }
  def isEmpty: Boolean = remainingText.isEmpty
  def nonEmpty: Boolean = remainingText.nonEmpty
}
