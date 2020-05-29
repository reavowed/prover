package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.definitions.{Qualifier, TermListAdapter}
import net.prover.model.entries.PropertyDefinitionOnType.RequiredParentObjects
import net.prover.model.entries.{PropertyDefinitionOnType, RelatedObjectDefinition, TypeDefinition, TypeQualifierDefinition}
import org.specs2.mutable.Specification

class PropertyDefinitionSpec extends Specification {
  val functionDefinition = TypeDefinition("function", "f", None, None, φ)
  val functionFromDefinition = TypeQualifierDefinition("from", functionDefinition, Qualifier(Seq("A", "B"), Format.Explicit("from A B", Seq("A", "B"), true, true)), None, φ, ConjunctionDefinition)
  val relationDefinition = TypeDefinition("relation", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true))), None, φ)
  val sequenceDefinition = TypeDefinition("sequence", "a", None, None, φ)
  val sequenceOnDefinition = TypeQualifierDefinition("on", sequenceDefinition, Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true)), None, φ, ConjunctionDefinition)
  val binaryOperationDefinition = TypeDefinition("binaryOperation", "f", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true))), None, φ)
  val identityDefinition = RelatedObjectDefinition("identity", binaryOperationDefinition, "e", None, None, φ, ConjunctionDefinition)
  
  val f = TermVariablePlaceholder("f", 0)
  val A = TermVariablePlaceholder("A", 1)
  val B = TermVariablePlaceholder("B", 2)
  val R= TermVariablePlaceholder("R", 0)

  "defining statement" should {
    "add simple parent type condition with no qualifiers" in {
      val definition = PropertyDefinitionOnType("injective", functionDefinition, None, None, None, None, ψ, ConjunctionDefinition)
      definition.statementDefinition.definingStatement must beSome(Conjunction(functionDefinition.statementDefinition(f), ψ))
    }
    "add simple parent type condition with default qualifier on parent" in {
      val definition = PropertyDefinitionOnType("reflexive", relationDefinition, None, None, None, None, ψ, ConjunctionDefinition)
      definition.statementDefinition.definingStatement must beSome(Conjunction(relationDefinition.statementDefinition(R, A), ψ))
    }
    "add parent type and qualifier condition with explicit qualifier on parent" in {
      val definition = PropertyDefinitionOnType("surjective", functionDefinition, Some(functionFromDefinition), None, None, None, ψ, ConjunctionDefinition)
      definition.statementDefinition.definingStatement must beSome(Conjunction(Conjunction(functionDefinition.statementDefinition(f), functionFromDefinition.statementDefinition(f, A, B)), ψ))
    }
    "add adapted parent type and qualifier condition with explicit qualifier on parent" in {
      val definition = PropertyDefinitionOnType("cauchy", sequenceDefinition, Some(sequenceOnDefinition), None, Some(TermListAdapter(Nil, Seq(Naturals))), None, ψ, ConjunctionDefinition)
      definition.statementDefinition.definingStatement must beSome(Conjunction(Conjunction(sequenceDefinition.statementDefinition(a), sequenceOnDefinition.statementDefinition(a, Naturals)), ψ))
    }
    "add related object condition by generalizing" in {
      val definition = PropertyDefinitionOnType("invertible", binaryOperationDefinition, None, Some(RequiredParentObjects(Seq(identityDefinition), UniqueExistenceDefinition, GeneralizationDefinition, DeductionDefinition)), None, None, ψ($), ConjunctionDefinition)
      definition.statementDefinition.definingStatement must beSome(
        Conjunction(
          binaryOperationDefinition.statementDefinition(f, A),
          Conjunction(
            ExistsUnique("e")(identityDefinition.statementDefinition($, f, A)),
            ForAll("e")(Implication(identityDefinition.statementDefinition($, f, A), ψ($))))))
    }
  }
}
