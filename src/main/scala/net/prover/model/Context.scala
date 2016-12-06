package net.prover.model

case class Context(
    connectives: Seq[Connective],
    quantifiers: Seq[Quantifier],
    predicates: Seq[Predicate],
    rules: Seq[Rule],
    theorems: Seq[Theorem],
    axioms: Seq[Axiom],
    termDefinitions: Seq[TermDefinition[_]]) {

  def +(other: Context): Context = {
    Context(
      connectives ++ other.connectives,
      quantifiers ++ other.quantifiers,
      predicates ++ other.predicates,
      rules ++ other.rules,
      theorems ++ other.theorems,
      axioms ++ other.axioms,
      termDefinitions ++ other.termDefinitions)
  }

  def definitions: Seq[Definition] = connectives.flatMap(_.definition) ++
    predicates.flatMap(_.definition) ++
    quantifiers.flatMap(_.definition)
}

object Context {
  val empty = Context(Nil, Nil, Nil, Nil, Nil, Nil, Nil)
}
