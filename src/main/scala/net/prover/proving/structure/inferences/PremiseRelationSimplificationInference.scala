package net.prover.proving.structure.inferences

import net.prover.model.ProvingContext
import net.prover.model.definitions.{Definitions, KnownStatement}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.SubstitutionContext
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}
import net.prover.proving.structure.statements.{BinaryRelation, BinaryRelationStatement}

import scala.Ordering.Implicits._

/**
  * An inference that takes a binary relation premise to one with a simpler right-hand side.
  */
case class PremiseRelationSimplificationInference(inferenceExtraction: InferenceExtraction, premise: Statement) extends PremiseSimplificationInference {
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement] = {
    for {
      substitutions <- premise.calculateSubstitutions(currentStatement.statement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      appliedSimplification <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(appliedSimplification)
  }
}

object PremiseRelationSimplificationInference {
  def getAll(definitions: Definitions): Map[BinaryRelation, Seq[PremiseRelationSimplificationInference]] = {
    def checkLhsIsValid(premiseLhs: Term, conclusionLhs: Term): Boolean = {
      // valid examples:
      //   a -> a
      //   a -> a_0
      //   (a, b) -> a
      (ExpressionUtils.isSimpleTermVariable(premiseLhs) && premiseLhs == conclusionLhs) || // a -> a
        ExpressionUtils.getSimpleTermVariable(premiseLhs).exists(ExpressionUtils.getWrappedSimpleTermVariable(conclusionLhs).contains) || // a -> a_0
        ExpressionUtils.getSimpleTermVariable(conclusionLhs).exists(premiseLhs.usedVariables.terms.variableIndices.contains) // (a, b) -> a
    }
    def checkRhsIsValidSimplification(premiseRhs: Term, conclusionRhs: Term): Boolean = {
      // valid examples:
      //   {a} -> a
      //   A x B -> A
      //   ℤ+ -> ℤ
      premiseRhs.complexity > conclusionRhs.complexity &&
        (ExpressionUtils.getSimpleTermVariable(conclusionRhs).exists(premiseRhs.usedVariables.terms.variableIndices.contains) ||
          ExpressionUtils.getTermConstantDefinition(conclusionRhs).exists(conclusionDefinition => ExpressionUtils.getTermConstantDefinition(premiseRhs).exists(premiseDefinition => premiseDefinition.definingStatement.referencedDefinitions.contains(conclusionDefinition))))
    }
    def checkNoSubstitutionOverlap(premiseLhs: Term, conclusionRhs: Term): Boolean = {
      (premiseLhs.usedVariables.terms.variableIndices.toSet intersect conclusionRhs.usedVariables.terms.variableIndices.toSet).isEmpty
    }

    implicit val substitutionContext = SubstitutionContext.outsideProof
    (for {
      inferenceExtraction <- definitions.allInferenceExtractions
      singlePremise <- inferenceExtraction.premises.single.toSeq
      if singlePremise.usedVariables.contains(inferenceExtraction.conclusion.usedVariables)
      if inferenceExtraction.variableDefinitions.statements.isEmpty && inferenceExtraction.variableDefinitions.hasNoApplications
      BinaryRelationStatement(_, conclusionLhs, conclusionRhs) <- definitions.asBinaryRelationStatement(inferenceExtraction.conclusion).toSeq
      BinaryRelationStatement(premiseRelation, premiseLhs, premiseRhs) <- definitions.asBinaryRelationStatement(singlePremise).toSeq
      if checkLhsIsValid(premiseLhs, conclusionLhs) && checkRhsIsValidSimplification(premiseRhs, conclusionRhs) && checkNoSubstitutionOverlap(premiseLhs, conclusionRhs)
    } yield premiseRelation -> PremiseRelationSimplificationInference(inferenceExtraction, singlePremise)).toSeqMap
  }
}
