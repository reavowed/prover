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

  def statementDefinitions: Seq[StatementDefinition] = connectives ++ predicates ++ quantifiers

  def theoremLineParsers: Seq[TheoremLineParser] =
    rules ++
      theorems ++
      axioms ++
      statementDefinitions.flatMap(d => d.forwardDeduction ++ d.reverseDeduction) ++
      termDefinitions.flatMap(_.deduction)

  def deductions: Seq[Deduction] = theoremLineParsers.ofType[Deduction]
}

object Context {
  val empty = Context(Nil, Nil, Nil, Nil, Nil, Nil, Nil)
}
