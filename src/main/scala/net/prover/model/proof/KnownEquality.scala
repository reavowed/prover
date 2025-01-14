package net.prover.model.proof

import net.prover.model.expressions.Term
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.structure.inferences.Equality

case class KnownEquality(lhs: Term, rhs: Term, equality: Equality, derivation: SimpleDerivation)
