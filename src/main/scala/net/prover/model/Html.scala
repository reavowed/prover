package net.prover.model

object Html {
  def format(text: String): String = {
    text.replaceAll("_([^\\s)}]+)", "<sub>$1</sub>")
      .replaceAll("\\^([^\\s)}]+)", "<sup>$1</sup>")
  }
}
