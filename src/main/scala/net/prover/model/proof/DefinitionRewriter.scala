package net.prover.model.proof

import net.prover._
import net.prover.model._
import net.prover.model.definitions.Wrapper
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.old.OldSubstitutionApplier
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

object DefinitionRewriter {
  private case class DefinitionRewriteStep(steps: Seq[Step], inference: Inference, source: Statement, result: Statement) {
    def toStep: Step = steps match {
      case Seq(singleAssertion: Step.Assertion) =>
        singleAssertion
      case _ =>
        Step.Elided(steps, Some(inference.summary), None)
    }
  }

  @scala.annotation.tailrec
  private def unifyBoundVariables(base: Statement, target: Statement, wrapper: Wrapper[Statement, Statement] = Wrapper.identity): Statement = {
    @scala.annotation.tailrec
    def getNextVariable(currentTarget: Statement): Option[(String, Statement)] = {
      currentTarget match {
        case definedTarget @ DefinedStatement(Seq(singleComponent: Statement), _) =>
          definedTarget.boundVariableNames match {
            case Seq(singleVariableName) =>
              Some((singleVariableName, singleComponent))
            case Nil =>
              getNextVariable(singleComponent)
            case _ =>
              None
          }
        case _ =>
          None
      }
    }

    base match {
      case definedBase @ DefinedStatement(Seq(innerBase: Statement), definition) =>
        definedBase.boundVariableNames match {
          case Seq(_) =>
            getNextVariable(target) match {
              case Some((targetVariableName, innerTarget)) =>
                unifyBoundVariables(innerBase, innerTarget, wrapper.insert[Statement]((s, _) => definition.bind(targetVariableName)(s)))
              case None =>
                wrapper(base)(SubstitutionContext.outsideProof)
            }
          case Nil =>
            unifyBoundVariables(innerBase, target, wrapper.insert[Statement]((s, _) => definition(s)))
          case _ =>
            wrapper(base)(SubstitutionContext.outsideProof)
        }
      case _ =>
        wrapper(base)(SubstitutionContext.outsideProof)
    }
  }

  private def getRewriteStep(premise: Statement, target: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Seq[DefinitionRewriteStep] = {
    def byDeconstructingPremise: Seq[DefinitionRewriteStep] = {
      (for {
        premiseDefinition <- premise.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        targetDefinition <- target.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        inference <- premiseDefinition.deconstructionInference
        inferencePremise <- inference.premises.single
        if inference.conclusion.asOptionalInstanceOf[DefinedStatement].exists(_.definition == targetDefinition)
        substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inferencePremise, premise).flatMap(_.confirmTotality(inference.variableDefinitions))
        deconstructionStep <- Step.Assertion.forInference(inference, substitutions)
      } yield DefinitionRewriteStep(Seq(deconstructionStep), inference, deconstructionStep.premises.head.statement, deconstructionStep.statement)).toSeq
    }
    def byConstructingTarget: Seq[DefinitionRewriteStep] = {
      (for {
        premiseDefinition <- premise.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        targetDefinition <- target.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        inference <- targetDefinition.constructionInference
        inferencePremise <- inference.premises.single
        if inferencePremise.asOptionalInstanceOf[DefinedStatement].exists(_.definition == premiseDefinition)
        substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inference.conclusion, target).flatMap(_.confirmTotality(inference.variableDefinitions))
        constructionStep <- Step.Assertion.forInference(inference, substitutions)
      } yield DefinitionRewriteStep(Seq(constructionStep), inference, constructionStep.premises.head.statement, constructionStep.statement)).toSeq
    }
    def insideDeductableStatement: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (wrappingInference, deductionPremise, otherPremise, _, _, _) <- provingContext.statementDeductionInferences
        preliminarySubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(otherPremise, premise)
          .flatMap(PossibleSubstitutionCalculator.calculatePossibleSubstitutions(wrappingInference.conclusion, target, _))
          .flatMap(_.confirmTotality(wrappingInference.variableDefinitions)).toSeq
        (innerPremise, innerTarget) <- OldSubstitutionApplier.applySubstitutions(deductionPremise, preliminarySubstitutions).toOption.flatMap(deductionDefinition.unapply).toSeq
        innerRewriteStep <- getRewriteStep(innerPremise, innerTarget)
        deductionStep = Step.Deduction(innerRewriteStep.source, innerRewriteStep.steps, deductionDefinition)
        deductionResult = deductionDefinition(innerRewriteStep.source, innerRewriteStep.result)
        substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(wrappingInference.premises.head, deductionResult).flatMap(_.confirmTotality(wrappingInference.variableDefinitions)).toSeq
        assertionStep <- Step.Assertion.forInference(wrappingInference, substitutions).toSeq
      } yield DefinitionRewriteStep(Seq(deductionStep, assertionStep), innerRewriteStep.inference, assertionStep.premises(1).statement, assertionStep.statement)
    }
    def insideDeductionStatement: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (eliminationInference, eliminationPremise, _) <- provingContext.deductionEliminationInferenceOption.toSeq
        (antecedent, premiseConsequent) <- deductionDefinition.unapply(premise).toSeq
        (targetAntecedent, targetConsequent) <- deductionDefinition.unapply(target).toSeq
        if antecedent == targetAntecedent
        innerRewriteStep <- getRewriteStep(premiseConsequent, targetConsequent)
        source = deductionDefinition(antecedent, innerRewriteStep.source)
        result = deductionDefinition(antecedent, innerRewriteStep.result)
        eliminationSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(eliminationPremise, source).flatMap(_.confirmTotality(eliminationInference.variableDefinitions))
        eliminationStep <- Step.Assertion.forInference(eliminationInference, eliminationSubstitutions)
        deductionStep = Step.Deduction(antecedent, eliminationStep +: innerRewriteStep.steps, deductionDefinition)
      } yield DefinitionRewriteStep(Seq(deductionStep), innerRewriteStep.inference, source, result)
    }
    def insideGeneralizationStatement: Seq[DefinitionRewriteStep] = {
      for {
        generalizationDefinition <- provingContext.generalizationDefinitionOption.toSeq
        (specificationInference, specificationPremise) <- provingContext.specificationInferenceOption.toSeq
        (_, premisePredicate) <- generalizationDefinition.unapply(premise).toSeq
        (_, targetPredicate) <- generalizationDefinition.unapply(target).toSeq
        variableName <- premise.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).toSeq
        innerSubstitutionContext = SubstitutionContext.withExtraParameter
        innerRewriteStep <- getRewriteStep(premisePredicate, targetPredicate)(implicitly, innerSubstitutionContext)
        source = generalizationDefinition(variableName, innerRewriteStep.source)
        result = generalizationDefinition(variableName, innerRewriteStep.result)
        specificationSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(specificationPremise, source.insertExternalParameters(1))(innerSubstitutionContext)
          .flatMap(PossibleSubstitutionCalculator.calculatePossibleSubstitutions(specificationInference.conclusion, innerRewriteStep.source, _)(innerSubstitutionContext))
          .flatMap(_.confirmTotality(specificationInference.variableDefinitions))
        specificationStep <- Step.Assertion.forInference(specificationInference, specificationSubstitutions)(innerSubstitutionContext)
        generalizationStep = Step.Generalization(variableName, specificationStep +: innerRewriteStep.steps, generalizationDefinition)
      } yield DefinitionRewriteStep(Seq(generalizationStep), innerRewriteStep.inference, source, result)
    }
    def byEliminatingPremise: Seq[DefinitionRewriteStep] = {
      for {
        (eliminationInference, eliminationPremise) <- provingContext.statementDefinitionEliminationInferences
        substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(eliminationPremise, premise).flatMap(_.confirmTotality(eliminationInference.variableDefinitions))
        result <- OldSubstitutionApplier.applySubstitutions(eliminationInference.conclusion, substitutions).toOption
        step <- Step.Assertion.forInference(eliminationInference, substitutions)
      } yield DefinitionRewriteStep(Seq(step), eliminationInference, premise, result)
    }
    def byIntroducingTarget: Seq[DefinitionRewriteStep] = {
      for {
        (introductionInference, introductionPremise) <- provingContext.statementDefinitionIntroductionInferences
        substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(introductionInference.conclusion, target).flatMap(_.confirmTotality(introductionInference.variableDefinitions))
        source <- OldSubstitutionApplier.applySubstitutions(introductionPremise, substitutions).toOption
        step <- Step.Assertion.forInference(introductionInference, substitutions)
      } yield DefinitionRewriteStep(Seq(step), introductionInference, source, target)
    }
    def byDeconstructingPremiseForElimination: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (deductionInference, _, deductionPremise, premiseIndex, conclusionIndex, wrappingSwapper) <- provingContext.statementDeductionInferences
        initialDeductionSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(deductionPremise, premise)
        innerPremise <- initialDeductionSubstitutions.statements.get(premiseIndex)
        definition <- innerPremise.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        deconstructionInference <- wrappingSwapper.getSource(definition.deconstructionInference, definition.constructionInference)
        deconstructionSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(definition.defaultValue, innerPremise).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
        deconstructedInnerPremise <- definition.definingStatement.flatMap(OldSubstitutionApplier.applySubstitutions(_, deconstructionSubstitutions).toOption).map(unifyBoundVariables(_, innerPremise))
        deductionSubstitutions <- initialDeductionSubstitutions.copy(statements = initialDeductionSubstitutions.statements + (conclusionIndex -> deconstructedInnerPremise))
          .confirmTotality(deductionInference.variableDefinitions)
        deconstructedSource <- OldSubstitutionApplier.applySubstitutions(deductionInference.conclusion, deductionSubstitutions).toOption
        if provingContext.statementDefinitionEliminationInferences.exists { case (_, premise) => PossibleSubstitutionCalculator.calculatePossibleSubstitutions(premise, deconstructedSource).nonEmpty }
        deconstructionPremise <- OldSubstitutionApplier.applySubstitutions(deconstructionInference.premise, deconstructionSubstitutions).toOption.map(unifyBoundVariables(_, innerPremise))
        deconstructionConclusion <- OldSubstitutionApplier.applySubstitutions(deconstructionInference.conclusion, deconstructionSubstitutions).toOption.map(unifyBoundVariables(_, innerPremise))
        deconstructionStep = Step.Assertion(deconstructionConclusion, deconstructionInference.summary, Seq(Premise.Pending(deconstructionPremise)), deconstructionSubstitutions)
        deductionStep = Step.Deduction(deconstructionStep.premises.head.statement, Seq(deconstructionStep), deductionDefinition)
        assertionStep <- Step.Assertion.forInference(deductionInference, deductionSubstitutions)
      } yield DefinitionRewriteStep(Seq(deductionStep, assertionStep), deconstructionInference, assertionStep.premises(1).statement, assertionStep.statement)
    }
    def byConstructingTargetForIntroduction: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (deductionInference, _, deductionPremise, premiseIndex, conclusionIndex, wrappingSwapper) <- provingContext.statementDeductionInferences
        initialDeductionSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(deductionInference.conclusion, target)
        innerTarget <- initialDeductionSubstitutions.statements.get(conclusionIndex)
        definition <- innerTarget.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        constructionInference <- wrappingSwapper.getSource(definition.constructionInference, definition.deconstructionInference)
        constructionSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(definition.defaultValue, innerTarget).flatMap(_.confirmTotality(constructionInference.variableDefinitions))
        deconstructedInnerTarget <- definition.definingStatement.flatMap(OldSubstitutionApplier.applySubstitutions(_, constructionSubstitutions).toOption).map(unifyBoundVariables(_, innerTarget))
        deductionSubstitutions <- initialDeductionSubstitutions.copy(statements = initialDeductionSubstitutions.statements + (premiseIndex -> deconstructedInnerTarget))
          .confirmTotality(deductionInference.variableDefinitions)
        deconstructedTarget <- OldSubstitutionApplier.applySubstitutions(deductionPremise, deductionSubstitutions).toOption
        if provingContext.statementDefinitionIntroductionInferences.exists { case (inference, _) => PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inference.conclusion, deconstructedTarget).nonEmpty }
        constructionPremise <- OldSubstitutionApplier.applySubstitutions(constructionInference.premise, constructionSubstitutions).toOption.map(unifyBoundVariables(_, innerTarget))
        constructionConclusion <- OldSubstitutionApplier.applySubstitutions(constructionInference.conclusion, constructionSubstitutions).toOption.map(unifyBoundVariables(_, innerTarget))
        constructionStep = Step.Assertion(constructionConclusion, constructionInference.summary, Seq(Premise.Pending(constructionPremise)), constructionSubstitutions)
        deductionStep = Step.Deduction(constructionStep.premises.head.statement, Seq(constructionStep), deductionDefinition)
        assertionStep <- Step.Assertion.forInference(deductionInference, deductionSubstitutions)
      } yield DefinitionRewriteStep(Seq(deductionStep, assertionStep), constructionInference, assertionStep.premises(1).statement, assertionStep.statement)
    }

    byDeconstructingPremise ++ byConstructingTarget ++
      insideDeductableStatement ++ insideDeductionStatement ++ insideGeneralizationStatement ++
      byEliminatingPremise ++ byIntroducingTarget ++
      byDeconstructingPremiseForElimination ++ byConstructingTargetForIntroduction
  }

  private def getRewriteSteps(premise: Statement, target: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Seq[Seq[DefinitionRewriteStep]] = {
    if (premise == target)
      Seq(Nil)
    else
      for {
        rewriteStep <- getRewriteStep(premise, target)
        wasPremiseRewritten = (rewriteStep.source == premise)
        (newPremise, newTarget) = if (wasPremiseRewritten) (rewriteStep.result, target) else (premise, rewriteStep.source)
        otherSteps <- getRewriteSteps(newPremise, newTarget)
      } yield if (wasPremiseRewritten) rewriteStep +: otherSteps else otherSteps :+ rewriteStep
  }

  def rewriteDefinitions(source: Statement, target: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[Step] = {
    (for {
      rewriteSteps <- getRewriteSteps(source, target)
      steps = rewriteSteps.map(_.toStep)
      step <- Step.Elided.ifNecessary(steps, "By definition")
    } yield step).headOption
  }
}
