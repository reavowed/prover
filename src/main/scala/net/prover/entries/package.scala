package net.prover

import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.Step

package object entries {
  type EntryWithContext = TypedEntryWithContext[ChapterEntry]
  type TheoremWithContext = TypedEntryWithContext[Theorem]

  type StepWithContext = TypedStepWithContext[Step]
}
