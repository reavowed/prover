package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.definitions.{Qualifier, TermListAdapter}
import net.prover.model.entries.{PropertyDefinitionOnType, TypeDefinition, TypeQualifierDefinition}
import org.specs2.mutable.Specification

class PropertyDefinitionSpec extends Specification {
  val functionDefinition = TypeDefinition("function", "f", None, None, φ)
  val functionFromDefinition = TypeQualifierDefinition("from", functionDefinition, Qualifier(Seq("A", "B"), Format.Explicit("from A B", Seq("A", "B"), true, true)), None, φ, Conjunction)
  val relationDefinition = TypeDefinition("relation", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true))), None, φ)
  val sequenceDefinition = TypeDefinition("sequence", "a", None, None, φ)
  val sequenceOnDefinition = TypeQualifierDefinition("on", sequenceDefinition, Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true)), None, φ, Conjunction)
  "defining statement" should {
    "add simple parent type condition with no qualifiers" in {
      val definition = PropertyDefinitionOnType("injective", functionDefinition, None, None, None, ψ, Conjunction)
      definition.statementDefinition.definingStatement must beSome(Conjunction(functionDefinition.statementDefinition(f), ψ))
    }
    "add simple parent type condition with default qualifier on parent" in {
      val definition = PropertyDefinitionOnType("reflexive", relationDefinition, None, None, None, ψ, Conjunction)
      definition.statementDefinition.definingStatement must beSome(Conjunction(relationDefinition.statementDefinition(R, A), ψ))
    }
    "add parent type and qualifier condition with explicit qualifier on parent" in {
      val definition = PropertyDefinitionOnType("surjective", functionDefinition, Some(functionFromDefinition), None, None, ψ, Conjunction)
      definition.statementDefinition.definingStatement must beSome(Conjunction(Conjunction(functionDefinition.statementDefinition(f), functionFromDefinition.statementDefinition(f, A, B)), ψ))
    }
    "add adapted parent type and qualifier condition with explicit qualifier on parent" in {
      val definition = PropertyDefinitionOnType("cauchy", sequenceDefinition, Some(sequenceOnDefinition), Some(TermListAdapter(Nil, Seq(Naturals))), None, ψ, Conjunction)
      definition.statementDefinition.definingStatement must beSome(Conjunction(Conjunction(sequenceDefinition.statementDefinition(a), sequenceOnDefinition.statementDefinition(a, Naturals)), ψ))
    }
  }
}
