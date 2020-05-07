package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.Wrapper
import net.prover.model.expressions.{DefinedStatement, Statement}

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
        substitutions <- inferencePremise.calculateSubstitutions(premise).flatMap(_.confirmTotality)
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
        substitutions <- inference.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
        constructionStep <- Step.Assertion.forInference(inference, substitutions)
      } yield DefinitionRewriteStep(Seq(constructionStep), inference, constructionStep.premises.head.statement, constructionStep.statement)).toSeq
    }
    def insideDeductableStatement: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (wrappingInference, deductionPremise, otherPremise, _, _, _) <- provingContext.statementDeductionInferences
        preliminarySubstitutions <- otherPremise.calculateSubstitutions(premise).flatMap(wrappingInference.conclusion.calculateSubstitutions(target, _)).flatMap(_.confirmTotality).toSeq
        (innerPremise, innerTarget) <- deductionPremise.applySubstitutions(preliminarySubstitutions).flatMap(deductionDefinition.unapply).toSeq
        innerRewriteStep <- getRewriteStep(innerPremise, innerTarget)
        deductionStep = Step.Deduction(innerRewriteStep.source, innerRewriteStep.steps, deductionDefinition)
        deductionResult = deductionDefinition(innerRewriteStep.source, innerRewriteStep.result)
        substitutions <- wrappingInference.premises.head.calculateSubstitutions(deductionResult).flatMap(_.confirmTotality).toSeq
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
        eliminationSubstitutions <- eliminationPremise.calculateSubstitutions(source).flatMap(_.confirmTotality)
        eliminationStep <- Step.Assertion.forInference(eliminationInference, eliminationSubstitutions)
        deductionStep = Step.Deduction(antecedent, eliminationStep +: innerRewriteStep.steps, deductionDefinition)
      } yield DefinitionRewriteStep(Seq(deductionStep), innerRewriteStep.inference, source, result)
    }
    def insideGeneralizationStatement: Seq[DefinitionRewriteStep] = {
      for {
        generalizationDefinition <- provingContext.generalizationDefinitionOption.toSeq
        (specificationInference, specificationPremise, _, _) <- provingContext.specificationInferenceOption.toSeq
        (_, premisePredicate) <- generalizationDefinition.unapply(premise).toSeq
        (_, targetPredicate) <- generalizationDefinition.unapply(target).toSeq
        variableName <- premise.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).toSeq
        innerSubstitutionContext = SubstitutionContext.withExtraParameter
        innerRewriteStep <- getRewriteStep(premisePredicate, targetPredicate)(implicitly, innerSubstitutionContext)
        source = generalizationDefinition(variableName, innerRewriteStep.source)
        result = generalizationDefinition(variableName, innerRewriteStep.result)
        specificationSubstitutions <- specificationPremise.calculateSubstitutions(source.insertExternalParameters(1))(innerSubstitutionContext)
          .flatMap(specificationInference.conclusion.calculateSubstitutions(innerRewriteStep.source, _)(innerSubstitutionContext))
          .flatMap(_.confirmTotality)
        specificationStep <- Step.Assertion.forInference(specificationInference, specificationSubstitutions)(innerSubstitutionContext)
        generalizationStep = Step.Generalization(variableName, specificationStep +: innerRewriteStep.steps, generalizationDefinition)
      } yield DefinitionRewriteStep(Seq(generalizationStep), innerRewriteStep.inference, source, result)
    }
    def byEliminatingPremise: Seq[DefinitionRewriteStep] = {
      for {
        (eliminationInference, eliminationPremise) <- provingContext.statementDefinitionEliminationInferences
        substitutions <- eliminationPremise.calculateSubstitutions(premise).flatMap(_.confirmTotality)
        result <- eliminationInference.conclusion.applySubstitutions(substitutions)
        step <- Step.Assertion.forInference(eliminationInference, substitutions)
      } yield DefinitionRewriteStep(Seq(step), eliminationInference, premise, result)
    }
    def byIntroducingTarget: Seq[DefinitionRewriteStep] = {
      for {
        (introductionInference, introductionPremise) <- provingContext.statementDefinitionIntroductionInferences
        substitutions <- introductionInference.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
        source <- introductionPremise.applySubstitutions(substitutions)
        step <- Step.Assertion.forInference(introductionInference, substitutions)
      } yield DefinitionRewriteStep(Seq(step), introductionInference, source, target)
    }
    def byDeconstructingPremiseForElimination: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (deductionInference, _, deductionPremise, premiseName, conclusionName, wrappingSwapper) <- provingContext.statementDeductionInferences
        initialDeductionSubstitutions <- deductionPremise.calculateSubstitutions(premise).flatMap(_.confirmTotality)
        (0, innerPremise) <- initialDeductionSubstitutions.statements.get(premiseName)
        definition <- innerPremise.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        deconstructionSubstitutions <- definition.defaultValue.calculateSubstitutions(innerPremise).flatMap(_.confirmTotality)
        deconstructedInnerPremise <- definition.definingStatement.flatMap(_.applySubstitutions(deconstructionSubstitutions)).map(unifyBoundVariables(_, innerPremise))
        deductionSubstitutions = initialDeductionSubstitutions.copy(statements = initialDeductionSubstitutions.statements + (conclusionName -> (0, deconstructedInnerPremise)))
        deconstructedSource <- deductionInference.conclusion.applySubstitutions(deductionSubstitutions)
        if provingContext.statementDefinitionEliminationInferences.exists(_._2.calculateSubstitutions(deconstructedSource).nonEmpty)
        deconstructionInference <- wrappingSwapper.getSource(definition.deconstructionInference, definition.constructionInference)
        deconstructionPremise <- deconstructionInference.premise.applySubstitutions(deconstructionSubstitutions).map(unifyBoundVariables(_, innerPremise))
        deconstructionConclusion <- deconstructionInference.conclusion.applySubstitutions(deconstructionSubstitutions).map(unifyBoundVariables(_, innerPremise))
        deconstructionStep = Step.Assertion(deconstructionConclusion, deconstructionInference.summary, Seq(Premise.Pending(deconstructionPremise)), deconstructionSubstitutions)
        deductionStep = Step.Deduction(deconstructionStep.premises.head.statement, Seq(deconstructionStep), deductionDefinition)
        assertionStep <- Step.Assertion.forInference(deductionInference, deductionSubstitutions)
      } yield DefinitionRewriteStep(Seq(deductionStep, assertionStep), deconstructionInference, assertionStep.premises(1).statement, assertionStep.statement)
    }
    def byConstructingTargetForIntroduction: Seq[DefinitionRewriteStep] = {
      for {
        deductionDefinition <- provingContext.deductionDefinitionOption.toSeq
        (deductionInference, _, deductionPremise, premiseName, conclusionName, wrappingSwapper) <- provingContext.statementDeductionInferences
        initialDeductionSubstitutions <- deductionInference.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
        (0, innerTarget) <- initialDeductionSubstitutions.statements.get(conclusionName)
        definition <- innerTarget.asOptionalInstanceOf[DefinedStatement].map(_.definition)
        constructionSubstitutions <- definition.defaultValue.calculateSubstitutions(innerTarget).flatMap(_.confirmTotality)
        deconstructedInnerTarget <- definition.definingStatement.flatMap(_.applySubstitutions(constructionSubstitutions)).map(unifyBoundVariables(_, innerTarget))
        deductionSubstitutions = initialDeductionSubstitutions.copy(statements = initialDeductionSubstitutions.statements + (premiseName -> (0, deconstructedInnerTarget)))
        deconstructedTarget <- deductionPremise.applySubstitutions(deductionSubstitutions)
        if provingContext.statementDefinitionIntroductionInferences.exists(_._1.conclusion.calculateSubstitutions(deconstructedTarget).nonEmpty)
        constructionInference <- wrappingSwapper.getSource(definition.constructionInference, definition.deconstructionInference)
        constructionPremise <- constructionInference.premise.applySubstitutions(constructionSubstitutions).map(unifyBoundVariables(_, innerTarget))
        constructionConclusion <- constructionInference.conclusion.applySubstitutions(constructionSubstitutions).map(unifyBoundVariables(_, innerTarget))
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
