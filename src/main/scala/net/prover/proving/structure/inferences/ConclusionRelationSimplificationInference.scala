package net.prover.proving.structure.inferences

import net.prover.model._
import net.prover.model.definitions.{Definitions, DerivedInference, DerivedPremise}
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext
import net.prover.model.utils.ExpressionUtils
import net.prover.model.utils.ExpressionUtils.TypeLikeStatement
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}
import net.prover.proving.structure.statements.{BinaryRelation, BinaryRelationStatement}

import scala.Ordering.Implicits._

/**
  * An inference that can be applied in reverse to a binary relation statement, simplifying one or both sides.
  * Used in premise finding to break a target premise down into simpler possibilities.
  */
case class ConclusionRelationSimplificationInference(inferenceExtraction: InferenceExtraction, typePremiseOption: Option[TypeLikeStatement], derivedPremises: Seq[DerivedPremise]) extends DerivedInference {
  def getConclusionSimplification(target: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[BinaryRelationStatement], SimpleDerivation)] = {
    for {
      substitutions <- conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality(variableDefinitions))
      (extractionApplication, _) <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      if extractionApplication.statement == target
      substitutedTypeStatement <- typePremiseOption.map(_.baseStatement.applySubstitutions(substitutions)).swap
      (simplifiedTargets, derivationSteps) <- derivedPremises.getSubstitutedPremises(substitutions)
      targetRelationStatements <- simplifiedTargets.map(provingContext.asBinaryRelationStatement).traverseOption
    } yield (substitutedTypeStatement.toSeq, targetRelationStatements, SimpleDerivation.fromAssertions(derivationSteps) :+ extractionApplication)
  }
}

object ConclusionRelationSimplificationInference {
  def getAll(definitions: Definitions): Map[BinaryRelation, Seq[ConclusionRelationSimplificationInference]] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof

    def isValidSimplification(premises: Seq[BinaryRelationStatement], conclusion: BinaryRelationStatement, optionalTypeStatement: Option[TypeLikeStatement]): Boolean = {
      // A simplification where the left-hand sides of the premise break down the left-hand side of the conclusion,
      // and all the right-hand sides are constant.
      // e.g. a ∈ ℕ, b ∈ ℕ -> (a, b)ℤ ∈ ℤ
      // e.g. a ∈ ℤ, b ∈ ℤ/{0} -> (a, b)ℚ ∈ ℚ
      // or a ∈ ℤ, b ∈ ℤ -> a + b ∈ ℤ
      val isLhsBreakdown: Boolean = {
        ExpressionUtils.isCombinationOfTermConstants(conclusion.right) &&
          premises.forall(p => ExpressionUtils.isCombinationOfTermConstants(p.right)) &&
          premises.forall(p => p.left.complexity < conclusion.left.complexity) &&
          ExpressionUtils.getCombinationOfSimpleTermVariables(conclusion.left).exists { termVariables =>
            termVariables.zipStrict(premises).exists(_.forall { case (variableName, premise) => ExpressionUtils.getSimpleTermVariable(premise.left).contains(variableName)})
          }
      }
      // A simplification where the right-hand sides of the premise break down the right-hand side of the conclusion,
      // and all the left-hand sides are the same variable.
      // e.g. a ∈ A, a ∉ B -> a ∈ A/B
      val isRhsBreakdown: Boolean = {
        ExpressionUtils.getSimpleTermVariable(conclusion.left).exists(v => premises.forall(p => ExpressionUtils.getSimpleTermVariable(p.left).contains(v))) &&
          premises.forall(p => p.right.complexity < conclusion.right.complexity) &&
          ExpressionUtils.getCombinationOfSimpleTermVariables(conclusion.right).exists { termVariables =>
            termVariables.zipStrict(premises).exists(_.forall { case (variableName, premise) => ExpressionUtils.getSimpleTermVariable(premise.right).contains(variableName)})
          }
      }
      // A simplification where the last premise is of the form a = b, the conclusion is of the form F(a) = G(b),
      // and the other premises relate a single variable to a constant.
      // e.g. a ∈ ℕ, b ∈ ℕ, a = b -> aℤ = bℤ
      val isDoubleSimplification: Boolean = {
        premises.lastOption.exists { lastPremise =>
          lastPremise.left.complexity < conclusion.left.complexity &&
            lastPremise.right.complexity < conclusion.right.complexity &&
            ExpressionUtils.getSimpleTermVariable(lastPremise.left).exists { lastPremiseLhsVariable =>
              ExpressionUtils.getSimpleTermVariable(lastPremise.right).exists { lastPremiseRhsVariable =>
                ExpressionUtils.getSingleSimpleTermVariable(conclusion.left).contains(lastPremiseLhsVariable) &&
                  ExpressionUtils.getSingleSimpleTermVariable(conclusion.right).contains(lastPremiseRhsVariable) &&
                  premises.init.forall { premise =>
                    ExpressionUtils.getSimpleTermVariable(premise.left).exists(v => v == lastPremiseLhsVariable || v == lastPremiseRhsVariable) &&
                      ExpressionUtils.isTermConstant(premise.right)
                  }
              }
            }
        }
      }
      // A simplification that takes the LHS to a variable, using an auxiliary type definition on the RHS
      // e.g. f is a function, a ∈ domain(f) -> f(a) ∈ range(f)
      val isTypeDefinitionSimplification: Boolean = {
        (for {
          typeStatement <- optionalTypeStatement
          typeVariable <- ExpressionUtils.getSimpleTermVariable(typeStatement.mainTerm)
          singlePremise <- premises.single
          conclusionLhsVariables <- ExpressionUtils.getCombinationOfSimpleTermVariables(conclusion.left)
          otherVariable <- conclusionLhsVariables match {
            case Seq(`typeVariable`, otherVariable) => Some(otherVariable)
            case Seq(otherVariable, `typeVariable`) => Some(otherVariable)
            case _ => None
          }
          if ExpressionUtils.getWrappedSimpleTermVariable(conclusion.right).contains(typeVariable)
          if ExpressionUtils.getWrappedSimpleTermVariable(singlePremise.right).contains(typeVariable)
          if ExpressionUtils.getSimpleTermVariable(singlePremise.left).contains(otherVariable)
        } yield true) getOrElse false
      }
      isLhsBreakdown || isRhsBreakdown || isDoubleSimplification || isTypeDefinitionSimplification
    }

    def breakOffTypeStatement(premises: Seq[Statement]): (Option[TypeLikeStatement], Seq[Statement]) = {
      premises.headAndTailOption
        .flatMap { case (head, tail) => ExpressionUtils.getTypeLikeStatement(head)(definitions.allAvailableEntries).map(t => Some(t) -> tail)}
        .getOrElse(None -> premises)
    }

    (for {
      inferenceExtraction <- definitions.allInferenceExtractions
      conclusion <- definitions.asBinaryRelationStatement(inferenceExtraction.conclusion).toSeq
      if inferenceExtraction.premises.nonEmpty && inferenceExtraction.conclusion.usedVariables.usesAll(inferenceExtraction.variableDefinitions)
      (optionalTypeStatement, otherPremises) = breakOffTypeStatement(inferenceExtraction.premises)
      premiseDesimplifications <- definitions.getPossiblePremiseDesimplifications(otherPremises)
      premises <- premiseDesimplifications.flatMap(_.getRootPremises).map(definitions.asBinaryRelationStatement).traverseOption.toSeq
      if isValidSimplification(premises, conclusion, optionalTypeStatement)
    } yield conclusion.relation -> ConclusionRelationSimplificationInference(inferenceExtraction, optionalTypeStatement, premiseDesimplifications)).toSeqMap
  }
}
