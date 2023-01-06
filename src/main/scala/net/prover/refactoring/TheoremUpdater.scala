package net.prover.refactoring

import net.prover.entries.TheoremWithContext
import net.prover.model.entries.Theorem

import scala.util.Try

trait TheoremUpdater {
  def updateTheorem(theorem: TheoremWithContext): Try[Theorem]
}
