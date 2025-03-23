package net.prover.proving.structure.inferences

import net.prover.model._
import net.prover.model.definitions.Definitions
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.structure.statements.{BinaryConnective, BinaryConnectiveStatement, BinaryRelationStatement, BinaryStatement}

/**
  * An inference similar to (φ ∨ ψ) ↔ (ψ ∨ φ), where two statements with equivalent substitutions are linked by a
  * chainable connective.
  */
case class ChainableRewriteInference(inference: Inference.Summary, connective: BinaryConnective, left: Statement, right: Statement) {
  def findValidTransitivity(targetConnective: BinaryConnective)(implicit provingContext: ProvingContext): Option[Transitivity[Statement]] = {
    provingContext.transitivities
      .ofType[Transitivity[Statement]]
      .find(t => t.firstPremiseJoiner == connective && t.secondPremiseJoiner == targetConnective && t.resultJoiner == targetConnective)
  }
}

object ChainableRewriteInference {
  def getAll(definitions: Definitions): Seq[ChainableRewriteInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- definitions.inferenceEntries
      if inference.premises.isEmpty
      BinaryConnectiveStatement(connective, lhs, rhs) <- definitions.asBinaryConnectiveStatement(inference.conclusion)
      case BinaryStatement(joiner, _, _) <- definitions.asBinaryStatement(lhs)
      if lhs == joiner.withVariables(0, 1) && rhs == joiner.withVariables(1, 0)
    } yield ChainableRewriteInference(inference.summary, connective, lhs, rhs)
  }
}
