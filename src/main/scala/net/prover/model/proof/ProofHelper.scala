package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object ProofHelper {
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference, externalDepth: Int): Option[Premise.Simplification] = {
    for {
      inferencePremise <- simplificationInference.premises.single
      substitutions <- inferencePremise.calculateSubstitutions(premise.statement, Substitutions.empty, 0, externalDepth).headOption
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions, 0, externalDepth)
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  private def getSingleSimplifications(premise: Premise.SingleLinePremise, simplificationInferences: Seq[Inference], externalDepth: Int): Seq[Premise.Simplification] = {
    simplificationInferences.mapCollect(i => getSimplification(premise, i, externalDepth))
  }
  def getSimplifications(premise: Premise.Given, entryContext: EntryContext, externalDepth: Int): Seq[Premise.Simplification] = {
    def helper(acc: Seq[Premise.Simplification], toCalculate: Seq[Premise.SingleLinePremise]): Seq[Premise.Simplification] = {
      if (toCalculate.isEmpty)
        acc
      else {
        val next = toCalculate.flatMap(getSingleSimplifications(_, entryContext.simplificationInferences, externalDepth))
        helper(acc ++ next, next)
      }
    }
    helper(Nil, Seq(premise))
  }

  def findFact(target: Statement, stepContext: StepContext, entryContext: EntryContext): Option[Step.Assertion] = {
    entryContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target, Substitutions.empty, 0, stepContext.externalDepth).headOption
          .map { substitutions =>
            Step.Assertion(target, inference.summary, Nil, substitutions)
          }
      }
  }

  def findNamingInferences(entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement)] = {
    entryContext.inferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i, entryContext).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference, entryContext: EntryContext): Option[(Seq[Statement], Statement)] = {
    (entryContext.scopingDefinitionOption, entryContext.deductionDefinitionOption) match {
      case (Some(scopingDefinition), Some(deductionDefinition)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+
              DefinedStatement(
              Seq(DefinedStatement(
                Seq(assumption: Statement, StatementVariable(deductionConclusionVariableName)),
                `deductionDefinition`
                )),
              `scopingDefinition`),
            StatementVariable(conclusionVariableName)
          ) if deductionConclusionVariableName == conclusionVariableName =>
            Some((initialPremises, assumption))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  def extract(targetStatement: Statement, entryContext: EntryContext, stepContext: StepContext, premiseContext: PremiseContext): Option[Step.Elided] = {
    val statementExtractionInferences = entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, firstPremise +: otherPremises, conclusion)
        if conclusion.singleStatementVariable.isDefined &&
          firstPremise.requiredSubstitutions.contains(inference.requiredSubstitutions) &&
          conclusion.complexity < firstPremise.complexity &&
          firstPremise.definitionUsages.contains(conclusion.definitionUsages)

      =>
        (inference, firstPremise, otherPremises)
    }
    val predicateSpecificationInferences = entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity < singlePremise.complexity
      =>
        conclusion.singlePredicateApplication
          .flatMap(pa => pa.arguments.map(_.asOptionalInstanceOf[TermVariable].map(_.name)).traverseOption.map(pa.variableName -> _))
          .filter { case (predicateName, argumentNames) =>
            singlePremise.requiredSubstitutions.isEquivalentTo(Substitutions.Required(Nil, Nil, Seq((predicateName, argumentNames.length)), Nil)) &&
              inference.requiredSubstitutions.isEquivalentTo(Substitutions.Required(Nil, argumentNames, Seq((predicateName, argumentNames.length)), Nil))
          }
          .map { case (predicateName, argumentNames) => (inference, singlePremise, predicateName, argumentNames)}
    }.collectDefined
    val simplificationInferences = entryContext.availableEntries.ofType[Inference].collectOption {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity > singlePremise.complexity &&
          conclusion.requiredSubstitutions.isEquivalentTo(singlePremise.requiredSubstitutions) &&
          singlePremise.referencedDefinitions.subsetOf(conclusion.referencedDefinitions)
      =>
        ( conclusion.definitionUsages - singlePremise.definitionUsages).map { addedDefinitions =>
          (inference, singlePremise, addedDefinitions)
        }
    }
    val rewriteInferences = entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity == singlePremise.complexity &&
          conclusion.requiredSubstitutions.isEquivalentTo(singlePremise.requiredSubstitutions) &&
          conclusion != singlePremise
      => (inference, singlePremise)
    }

    def findSubsidiaryPremiseFromGivenPremise(subsidiaryPremise: Statement, givenPremise: Statement): Option[Seq[Step]] = {
      if (subsidiaryPremise == givenPremise)
        Some(Nil)
      else if (subsidiaryPremise.complexity > givenPremise.complexity && subsidiaryPremise.definitionUsages.contains(givenPremise.definitionUsages))
        simplificationInferences.iterator.findFirst { case (inference, singlePremise, addedDefinitions) =>
          if ((subsidiaryPremise.definitionUsages ++ addedDefinitions).contains(givenPremise.definitionUsages))
            (for {
              substitutions <- singlePremise.calculateSubstitutions(givenPremise, Substitutions.empty, 0, stepContext.externalDepth)
              simplifiedPremise <- inference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth)
              innerSteps <- findSubsidiaryPremiseFromGivenPremise(subsidiaryPremise, simplifiedPremise)
              newStep = Step.Assertion(simplifiedPremise, inference.summary, Seq(Premise.Pending(givenPremise)), substitutions)
            } yield newStep +: innerSteps).headOption
          else
            None
        }
      else None
    }
    def findSubsidiaryPremise(premise: Statement): Option[Seq[Step]] = {
      def fromGivenPremises = premiseContext.premisesAndSimplifications
        .flatMap(x => x._2.reverse :+ x._1)
        .map(_.statement)
        .mapFind(findSubsidiaryPremiseFromGivenPremise(premise, _))
      def fromFact = findFact(premise, stepContext, entryContext).map(Seq(_))
      fromGivenPremises orElse fromFact
    }

    def matchDirectly(extractionCandidate: Statement, termsSoFar: Int): Option[(Seq[Term], Seq[Step])] = {
      (for {
        substitutions <- extractionCandidate.calculateSubstitutions(targetStatement, Substitutions.empty, 0, stepContext.externalDepth)
        if substitutions.statements.forall {
          case (name, StatementVariable(name2)) if name == name2 =>
            true
          case _ =>
            false
        }
        if substitutions.terms.filter(!_._1.startsWith("placeholder")).forall {
          case (name, TermVariable(name2)) if name == name2 =>
            true
          case _ =>
            false
        }
        if substitutions.predicates.forall {
          case ((name, _), PredicateApplication(name2, arguments))
            if name == name2 &&
              arguments == arguments.indices.map(i => FunctionParameter(i, stepContext.externalDepth))
          =>
            true
          case _ =>
            false
        }
        if substitutions.functions.forall {
          case ((name, _), FunctionApplication(name2, arguments))
            if name == name2 &&
              arguments == arguments.indices.map(i => FunctionParameter(i, stepContext.externalDepth))
          =>
            true
          case _ =>
            false
        }
        terms <- (0 until termsSoFar).map(i => substitutions.terms.get(s"placeholder$i")).traverseOption.toSeq
      } yield (terms, Nil)).headOption
    }

    def removePlaceholders(extractionCandidate: Statement, terms: Seq[Term]): Option[Statement] = {
      val requiredSubstitutions = extractionCandidate.requiredSubstitutions
      val termLookup = terms.mapWithIndex((t, i) => s"placeholder$i" -> t).toMap.withDefault(TermVariable)
      val substitutions = Substitutions(
        statements = requiredSubstitutions.statements.mapToMap(StatementVariable),
        terms = requiredSubstitutions.terms.mapToMap(termLookup(_)),
        predicates = requiredSubstitutions.predicates.mapToMap { case (name, arity) => PredicateApplication(name, (0 until arity).map(i => FunctionParameter(i, stepContext.externalDepth)))},
        functions = requiredSubstitutions.functions.mapToMap { case (name, arity) => FunctionApplication(name, (0 until arity).map(i => FunctionParameter(i, stepContext.externalDepth)))}
      )
      extractionCandidate.applySubstitutions(substitutions, 0, stepContext.externalDepth)
    }

    def extractRecursively(extractionCandidate: Statement, termsSoFar: Int): Option[(Seq[Term], Seq[Step])] = {
      statementExtractionInferences.iterator.findFirst {
        case (inference, firstPremise, otherPremises) =>
          (for {
            innerSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth)
            inferenceConclusion <- inference.conclusion.applySubstitutions(innerSubstitutions, 0, stepContext.externalDepth).toSeq
            (terms, innerSteps) <- extract(inferenceConclusion, termsSoFar).toSeq
            actualFirstPremise <- removePlaceholders(extractionCandidate, terms).toSeq
            actualSubstitutions <- firstPremise.calculateSubstitutions(actualFirstPremise, Substitutions.empty, 0, stepContext.externalDepth)
            actualOtherPremises <- otherPremises.map(_.applySubstitutions(actualSubstitutions, 0, stepContext.externalDepth)).traverseOption
            actualConclusion <- inference.conclusion.applySubstitutions(actualSubstitutions, 0, stepContext.externalDepth)
            premiseSteps <- actualOtherPremises.map(findSubsidiaryPremise).traverseOption.map(_.flatten)
            newStep = Step.Assertion(
              actualConclusion,
              inference.summary,
              (actualFirstPremise +: actualOtherPremises).map(Premise.Pending),
              actualSubstitutions)
          } yield (terms, (premiseSteps :+ newStep) ++ innerSteps)).headOption
      } orElse predicateSpecificationInferences.iterator.findFirst {
        case (inference, singlePremise, predicateName, argumentNames) =>
          (for {
            extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth)
            predicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
            nextPremise = predicate.specify(argumentNames.mapWithIndex((_, index) => TermVariable(s"placeholder${termsSoFar + index}")), 0, stepContext.externalDepth)
            (terms, laterSteps) <- extract(nextPremise, termsSoFar + argumentNames.length).toSeq
            actualPremise <- removePlaceholders(extractionCandidate, terms).toSeq
            actualSubstitutions <- singlePremise.calculateSubstitutions(actualPremise, Substitutions.empty, 0, stepContext.externalDepth)
              .map(_.copy(terms = argumentNames.zip(terms.slice(termsSoFar, termsSoFar + argumentNames.length)).toMap))
            actualConclusion <- inference.conclusion.applySubstitutions(actualSubstitutions, 0, stepContext.externalDepth)
            newStep = Step.Assertion(
              actualConclusion,
              inference.summary,
              Seq(Premise.Pending(actualPremise)),
              actualSubstitutions)
          } yield (terms, newStep +: laterSteps)).headOption
      }
    }

    def extractWithoutRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Seq[Term], Seq[Step])] = {
      matchDirectly(extractionCandidate, termsSoFar) orElse
        extractRecursively(extractionCandidate, termsSoFar)
    }

    def extractWithRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Seq[Term], Seq[Step])] = {
      rewriteInferences.iterator.findFirst { case (inference, singlePremise) =>
        (for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth)
          inferenceConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions, 0, stepContext.externalDepth).toSeq
          (terms, innerSteps) <- extractWithoutRewrite(inferenceConclusion, termsSoFar).toSeq
          actualPremise <- removePlaceholders(extractionCandidate, terms).toSeq
          actualSubstitutions <- singlePremise.calculateSubstitutions(actualPremise, Substitutions.empty, 0, stepContext.externalDepth)
          actualConclusion <- inference.conclusion.applySubstitutions(actualSubstitutions, 0, stepContext.externalDepth)
          newStep = Step.Assertion(
            actualConclusion,
            inference.summary,
            Seq(Premise.Pending(actualPremise)),
            actualSubstitutions)
        } yield (terms, newStep +: innerSteps)).headOption
      }
    }

    def extract(extractionCandidate: Statement, termsSoFar: Int): Option[(Seq[Term], Seq[Step])] = {
      extractWithoutRewrite(extractionCandidate, termsSoFar) orElse
        extractWithRewrite(extractionCandidate, termsSoFar)
    }

    def extractPremise(premise: Premise.SingleLinePremise): Option[Step.Elided] = {
      extract(premise.statement, 0).map { case (_, steps) =>
        Step.Elided(steps, None, Some("Simplified"))
      }
    }

    premiseContext.premisesAndSimplifications
      .flatMap(x => x._2.reverse :+ x._1)
      .mapFind(extractPremise)
  }

  implicit class ExpressionOps(expression: Expression) {
    def singleStatementVariable: Option[StatementVariable] = expression match {
      case sv: StatementVariable =>
        Some(sv)
      case DefinedStatement(Seq(singleStatement: Statement), _) =>
        singleStatement.singleStatementVariable
      case _ =>
        None
    }
    def singlePredicateApplication: Option[PredicateApplication] = expression match {
      case pa: PredicateApplication =>
        Some(pa)
      case DefinedStatement(Seq(singleStatement: Statement), _) =>
        singleStatement.singlePredicateApplication
      case _ =>
        None
    }
  }
}
