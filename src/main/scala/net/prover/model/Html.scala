package net.prover.model

object Html {
  def format(text: String): String = {
    text.replaceAll("_(\\w+)", "<sub>$1</sub>")
      .replaceAll("\\^(\\w+)", "<sup>$1</sup>")
  }
}
