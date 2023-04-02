package net.prover.model.proof

import net.prover.model.definitions.Equality
import net.prover.model.expressions.Term

case class KnownEquality(lhs: Term, rhs: Term, equality: Equality, derivation: Seq[DerivationStep])
