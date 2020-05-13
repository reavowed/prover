package net.prover.model.definitions

import net.prover.model.expressions.Term

case class BinaryRelationStatement(relation: BinaryRelation, left: Term, right: Term)
