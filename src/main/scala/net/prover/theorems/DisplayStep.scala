package net.prover.theorems

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, PreviousLineReference, StepReference}

sealed trait DisplayStep {
  @JsonSerialize
  def `type`: String = getClass.getSimpleName
}

object DisplayStep {
  case class Target(statement: Statement, path: Seq[Int]) extends DisplayStep
  case class Assertion(statement: Statement, inference: Inference.Summary, path: Seq[Int], premises: Seq[Premise]) extends DisplayStep
  case class Deduction(assumption: Statement, statement: Statement, path: Seq[Int], substeps: Seq[DisplayStep]) extends DisplayStep
  case class Generalization(variableName: String, statement: Statement, substeps: Seq[DisplayStep], path: Seq[Int]) extends DisplayStep
  case class Naming(variableName: String, assumption: Statement, statement: Statement, inference: Inference.Summary, path: Seq[Int], premises: Seq[Premise], substeps: Seq[DisplayStep]) extends DisplayStep
  case class Subproof(name: String, statement: Statement, path: Seq[Int], substeps: Seq[DisplayStep]) extends DisplayStep
  case class ElidedInference(statement: Statement, inference: Inference.Summary, path: Seq[Int], substeps: Seq[DisplayStep]) extends DisplayStep
  case class ElidedWithDescription(statement: Statement, description: String, path: Seq[Int], substeps: Seq[DisplayStep]) extends DisplayStep
}
