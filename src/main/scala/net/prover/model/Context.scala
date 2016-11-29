package net.prover.model

case class Context(
    connectives: Seq[Connective],
    rules: Seq[Rule],
    theorems: Seq[Theorem],
    definitions: Seq[Definition]) {

  def +(other: Context): Context = {
    Context(
      connectives ++ other.connectives,
      rules ++ other.rules,
      theorems ++ other.theorems,
      definitions ++ other.definitions)
  }
}
