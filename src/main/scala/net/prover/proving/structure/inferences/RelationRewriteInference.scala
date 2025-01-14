package net.prover.proving.structure.inferences

import net.prover.model.definitions.{Definitions, KnownStatement}
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{StepProvingContext, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.model.{ProvingContext, Substitutions}
import net.prover.proving.derivation.SimpleDerivation
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
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(currentStatement.statement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findKnownStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((SimpleDerivation.empty, substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      derivationStep <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(premiseDerivation :+ derivationStep)
  }
  def rewriteTarget(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[(BinaryRelationStatement, SimpleDerivation)] = {
    for {
      substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findDerivationForStatementBySubstituting(initialPremise, substitutionsAfterConclusion, stepProvingContext.knownStatementsFromPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((SimpleDerivation.empty, substitutionsAfterConclusion))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      derivationStep <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      substitutedPremise <- mainPremise.applySubstitutions(substitutions)
      premiseRelationStatement <- stepProvingContext.provingContext.findRelation(substitutedPremise)
    } yield (premiseRelationStatement, premiseDerivation :+ derivationStep)
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
        definitions.findRelation(lastPremise).toSeq.map(_ -> Substitutions.Possible.empty)
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
        definitions.findRelation(initialPremise).exists { initialPremiseRelation =>
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

    for {
      inferenceExtraction <- definitions.allInferenceExtractions
      (initialPremiseOption, mainPremiseStatement) <- inferenceExtraction.premises match {
        case Seq(a) => Seq((None, a))
        case Seq(a, b) => Seq((Some(a), b))
        case _ => Nil
      }
      if inferenceExtraction.premises.usedVariables.contains(inferenceExtraction.conclusion.usedVariables)
      if (initialPremiseOption.toSeq :+ inferenceExtraction.conclusion).usedVariables.contains(mainPremiseStatement.usedVariables)
      conclusion <- definitions.findRelation(inferenceExtraction.conclusion).toSeq
      (mainPremise, initialSubstitutions) <- findPremiseRelation(mainPremiseStatement)
      if isValidRewrite(mainPremise, conclusion) && isValidInitialPremise(initialPremiseOption, conclusion)
    } yield RelationRewriteInference(inferenceExtraction, initialPremiseOption, mainPremiseStatement, mainPremise.relation, conclusion.relation, initialSubstitutions)
  }
}
