package net.prover.proving.structure.inferences

import net.prover.model.*
import net.prover.model.definitions.Definitions
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{StepProvingContext, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}
import net.prover.proving.premiseFinding.DerivationFinder
import net.prover.proving.structure.statements.{BinaryRelation, BinaryRelationFromGeneralShorthand, BinaryRelationStatement}

/**
  * An inference that takes a relation of the form "a R X" to one of the form "a R Y" or "Y R a" where X and Y are
  * either both variables or both constants. Optionally allows a supporting premise that is either a type statement
  * or a different relation.
  */
case class RelationRewriteInference(
    inferenceExtraction: InferenceExtraction,
    initialPremiseOption: Option[Statement],
    mainPremise: Statement,
    premiseRelation: BinaryRelation,
    conclusionRelation: BinaryRelation,
    initialSubstitutions: Substitutions.Possible)
  extends PremiseSimplificationInference
{
  def getPremiseSimplification(currentStatement: DerivedStatement, existingPremises: Seq[DerivedStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[DerivedStatement] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(currentStatement.statement, initialSubstitutions)
      (initialPremiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findKnownStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption.map(_.mapLeft(Some(_)))
        case None =>
          Some((None, substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      (derivationStep, _) <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
    } yield DerivedStatement.RelationRewritten(currentStatement, initialPremiseDerivation, derivationStep)
  }
  /**
    * Finds a statement that this inference would rewrite to the given target, along with a function that takes a
    * derivation of that statement to a derivation of the target.
    */
  def rewriteTarget(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[(BinaryRelationStatement, DerivedStatement => DerivedStatement)] = {
    for {
      substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement, initialSubstitutions)
      (initialPremiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findDerivationForStatementBySubstituting(initialPremise, substitutionsAfterConclusion, stepProvingContext.knownStatementsFromPremises).headOption.map(_.mapLeft(Some(_)))
        case None =>
          Some((None, substitutionsAfterConclusion))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      (derivationStep, _) <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      substitutedPremise <- mainPremise.applySubstitutions(substitutions)
      premiseRelationStatement <- stepProvingContext.provingContext.asBinaryRelationStatement(substitutedPremise)
    } yield (premiseRelationStatement, DerivedStatement.RelationRewritten(_, initialPremiseDerivation, derivationStep))
  }
}

object RelationRewriteInference {
  def getAll(definitions: Definitions): Seq[RelationRewriteInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof

    def findPremiseRelation(lastPremise: Statement): Seq[(BinaryRelationStatement, Substitutions.Possible)] = {
      val relationsFromTemplate = definitions.definedBinaryRelations.ofType[BinaryRelationFromGeneralShorthand].mapCollect { relation =>
        val generalTemplate = relation.shorthand.template.expand(
          Map.empty,
          Map(
            relation.lhsVariableName -> TermVariable(0),
            relation.rhsVariableName -> TermVariable(1),
            relation.symbolVariableName -> TermVariable(2))
        ).asInstanceOf[Statement]
        for {
          substitutions <- generalTemplate.calculateSubstitutions(lastPremise)
          lhsVariableIndex <- ExpressionUtils.getSimpleTermVariable(substitutions.terms(0))
          rhsVariableIndex <- ExpressionUtils.getSimpleTermVariable(substitutions.terms(1))
          symbolVariableIndex <- ExpressionUtils.getSimpleTermVariable(substitutions.terms(2))
        } yield (BinaryRelationStatement.construct(relation, TermVariable(lhsVariableIndex), TermVariable(rhsVariableIndex)), Substitutions.Possible(terms = Map(symbolVariableIndex -> relation.definition())))
      }
      if (relationsFromTemplate.nonEmpty)
        relationsFromTemplate
      else
        definitions.asBinaryRelationStatement(lastPremise).toSeq.map(_ -> Substitutions.Possible.empty)
    }
    def areValidSecondaryComponents(premiseComponent: Term, conclusionComponent: Term): Boolean = {
      (ExpressionUtils.isSimpleTermVariable(premiseComponent) && ExpressionUtils.isSimpleTermVariable(conclusionComponent)) ||
        (ExpressionUtils.isCombinationOfTermConstants(premiseComponent) && ExpressionUtils.isCombinationOfTermConstants(conclusionComponent))
    }
    def isValidRewrite(premise: BinaryRelationStatement, conclusion: BinaryRelationStatement): Boolean = {
      // e.g. a ∈ A -> a ∈ B
      // or   a ~ b -> a ≠ b
      // or   a ∈ ℤ+ -> 0 < a
      ExpressionUtils.isSimpleTermVariable(premise.left) &&
        ((premise.left == conclusion.left && areValidSecondaryComponents(premise.right, conclusion.right)) ||
        (premise.left == conclusion.right && areValidSecondaryComponents(premise.right, conclusion.left)))
    }
    def isValidInitialPremise(initialPremiseOption: Option[Statement], conclusion: BinaryRelationStatement) = {
      def isValidTypeStatement(initialPremise: Statement): Boolean = {
        ExpressionUtils.getTypeLikeStatement(initialPremise)(definitions.allAvailableEntries).exists { typeStatement =>
          ExpressionUtils.isSimpleTermVariable(typeStatement.mainTerm)
        }
      }
      def isValidRelation(initialPremise: Statement): Boolean = {
        definitions.asBinaryRelationStatement(initialPremise).exists { initialPremiseRelation =>
          ExpressionUtils.isSimpleTermVariable(initialPremiseRelation.left) &&
            ExpressionUtils.isSimpleTermVariable(initialPremiseRelation.right) &&
            initialPremiseRelation.relation != conclusion.relation
        }
      }
      initialPremiseOption match {
        case None =>
          true
        case Some(initialPremise) =>
          isValidTypeStatement(initialPremise) || isValidRelation(initialPremise)
      }
    }

    // Reversals don't rewrite one relation to another, and are applied separately
    val reversalInferenceIds = definitions.reversals.map(_.inference.id).toSet

    for {
      inferenceExtraction <- definitions.allInferenceExtractions
      if !reversalInferenceIds.contains(inferenceExtraction.inference.id)
      (initialPremiseOption, mainPremiseStatement) <- inferenceExtraction.premises match {
        case Seq(a) => Seq((None, a))
        case Seq(a, b) => Seq((Some(a), b))
        case _ => Nil
      }
      if inferenceExtraction.premises.usedVariables.contains(inferenceExtraction.conclusion.usedVariables)
      if (initialPremiseOption.toSeq :+ inferenceExtraction.conclusion).usedVariables.contains(mainPremiseStatement.usedVariables)
      conclusion <- definitions.asBinaryRelationStatement(inferenceExtraction.conclusion).toSeq
      (mainPremise, initialSubstitutions) <- findPremiseRelation(mainPremiseStatement)
      if isValidRewrite(mainPremise, conclusion) && isValidInitialPremise(initialPremiseOption, conclusion)
    } yield RelationRewriteInference(inferenceExtraction, initialPremiseOption, mainPremiseStatement, mainPremise.relation, conclusion.relation, initialSubstitutions)
  }
}
