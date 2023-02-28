package net.prover.proving.premiseFinding

import net.prover.model.TestDefinitions.{a, b, _}
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.Qualifier
import net.prover.model.entries.{ParentTypeConditions, PropertyDefinitionOnType, TermDefinitionEntry, TypeDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model.{EntryContext, Format, TermVariablePlaceholder, TestDefinitions, VariableDefinitions}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class DerivationOrTargetFinderSpec extends Specification {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name
  implicit val entryContext = defaultEntryContext
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "premise finder" should {
    def getStepContext(premises: Seq[Statement], target: Statement, depth: Int): StepContext = {
      val emptyContext = createBaseStepContext(Nil, premises :+ target)
      val contextWithDepth = (0 until depth).foldLeft(emptyContext){ (stepContext, i) => stepContext.addBoundVariable(s"x_$i")}
      premises.zipWithIndex.foldLeft(contextWithDepth) { case (context, (premise, index)) =>
        context.addStatement(premise, PremiseReference(index))
      }
    }

    def findPremiseOrTarget(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit entryContext: EntryContext): (Seq[Step], Seq[Statement]) = {
      implicit val stepContext = getStepContext(premises, target, depth)
      DerivationOrTargetFinder.findDerivationsOrTargets(Seq(target))(entryContextAndStepContextToStepProvingContext(entryContext, stepContext))
        .mapLeft(_.steps)
        .mapRight(_.map(_.statement))
    }

    "only add one half of a conjunction as a target if the other half is present as a premise" in {
      findPremiseOrTarget(
        Conjunction(φ, ψ),
        Seq(φ)
      ) mustEqual (
        Seq(assertion(combineConjunction, Seq(φ, ψ), Nil)(SubstitutionContext.outsideProof)),
        Seq(ψ.toVariable)
      )
    }

    "replace terms in a target using a fact" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 2), Nil)
      val axiom = createInference(
        "Function Properties of Natural Addition",
        Nil,
        Conjunction(
          Function(Addition),
          FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)))

      findPremiseOrTarget(
        ForAllIn("x", Domain(Addition))(ForAllIn("y", Domain(Addition))(φ($.^, $))),
        Nil)(
        defaultEntryContext.addEntry(axiom)
      ) mustEqual (
        Seq(
          elided(axiom, Seq(
            elided(axiom, Seq(
              assertion(axiom, Nil, Nil),
              assertion(extractRightConjunct, Seq(Function(Addition), FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)), Nil))),
            elided(FunctionFrom.deconstructionInference, Seq(
              assertion(FunctionFrom.deconstructionInference, Nil, Seq(Addition, Product(Naturals, Naturals), Naturals)),
              assertion(reverseEquality, Nil, Seq(Domain(Addition), Product(Naturals, Naturals))))))),
          assertion(substitutionOfEquals, Seq(ForAllIn("x", $.^)(ForAllIn("y", $.^^)(φ($.^, $)))), Seq(Product(Naturals, Naturals), Domain(Addition))))(SubstitutionContext.outsideProof),
        Seq(ForAllIn("x", Product(Naturals, Naturals))(ForAllIn("y", Product(Naturals, Naturals))(φ($.^, $)))))
    }

    "deconstruct a non-type-statement target" in {
      findPremiseOrTarget(Conjunction(φ, ψ), Nil) mustEqual (
        Seq(assertion(combineConjunction, Seq(φ, ψ), Nil)(SubstitutionContext.outsideProof)),
        Seq(φ.toVariable, ψ.toVariable)
      )
    }

    "not deconstruct a type-statement target" in {
      findPremiseOrTarget(Conjunction(Function(a), FunctionFrom(a, b, c)), Nil) mustEqual (Nil, Seq(Conjunction(Function(a), FunctionFrom(a, b, c))))
    }
  }
}