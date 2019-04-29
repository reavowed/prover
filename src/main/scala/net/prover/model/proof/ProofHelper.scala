package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions._

import scala.util.Try

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

  def findBySimplifying(target: Statement, entryContext: EntryContext, premiseContext: PremiseContext, stepContext: StepContext): Option[Seq[Step]] = {
    val simplificationInferences = entryContext.availableEntries.ofType[Inference].filter {
      case inference @ Inference(_, premises, conclusion)
        if premises.nonEmpty &&
          premises.forall(_.complexity < conclusion.complexity ) &&
          conclusion.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
          conclusion.requiredSubstitutions.predicates.isEmpty && conclusion.requiredSubstitutions.functions.isEmpty &&
          premises.forall(_.referencedDefinitions.subsetOf(conclusion.referencedDefinitions))
      =>
        true
      case _ =>
        false
    }

    def fromGivenPremises = premiseContext.premisesAndSimplifications
      .flatMap(x => x._2.reverse :+ x._1)
      .map(_.statement)
      .find(_ == target)
      .map(_ => Nil)
    def fromFact = findFact(target, stepContext, entryContext).map(Seq(_))
    def bySimplifying = simplificationInferences.iterator.findFirst { inference =>
      (for {
        substitutions <- inference.conclusion.calculateSubstitutions(target, Substitutions.empty, 0, stepContext.externalDepth)
        premiseStatements <- Try(inference.substitutePremises(substitutions, stepContext)).toOption
        premiseSteps <- premiseStatements.map(findBySimplifying(_, entryContext, premiseContext, stepContext)).traverseOption.map(_.reduce(_ ++ _))
        assertionStep = Step.Assertion(target, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep).headOption
    }
    fromGivenPremises orElse fromFact orElse bySimplifying
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

  def extract(targetStatement: Statement, entryContext: EntryContext, stepContext: StepContext, premiseContext: PremiseContext): Option[Step] = {
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
    val rewriteInferences = entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity == singlePremise.complexity &&
          conclusion.requiredSubstitutions.isEquivalentTo(singlePremise.requiredSubstitutions) &&
          conclusion != singlePremise
      => (inference, singlePremise)
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
            premiseSteps <- actualOtherPremises.map(findBySimplifying(_, entryContext, premiseContext, stepContext)).traverseOption.map(_.flatten)
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

    def extractPremise(premise: Premise.SingleLinePremise): Option[Step] = {
      extract(premise.statement, 0).map(_._2).flatMap(wrapAsElidedIfNecessary(_, "Simplified"))
    }

    premiseContext.premisesAndSimplifications
      .flatMap(x => x._2.reverse :+ x._1)
      .mapFind(extractPremise)
  }

  def getAssertionWithPremises(
    inference: Inference.Summary,
    substitutions: Substitutions,
    stepContext: StepContext,
    premiseContext: PremiseContext,
    entryContext: EntryContext
  ): Seq[Step] = {
    val premiseStatements = inference.substitutePremises(substitutions, stepContext)
    val conclusion = inference.substituteConclusion(substitutions, stepContext)
    val premises = premiseStatements.map(premiseContext.createPremise)
    val (targetSteps, premiseSteps) = premises.ofType[Premise.Pending].foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((targetStepsSoFar, premiseStepsSoFar), premise) =>
      ProofHelper.findBySimplifying(premise.statement, entryContext, premiseContext, stepContext) match {
        case Some(newPremiseSteps) =>
          (targetStepsSoFar, premiseStepsSoFar ++ newPremiseSteps)
        case None =>
          (targetStepsSoFar :+ Step.Target(premise.statement), premiseStepsSoFar)
      }
    }
    val assertionStep = Step.Assertion(
      conclusion,
      inference,
      premises,
      substitutions)
    val baseStep = if (premiseSteps.nonEmpty) {
      Step.Elided(premiseSteps :+ assertionStep, Some(inference.summary), None)
    } else {
      assertionStep
    }
    targetSteps :+ baseStep
  }

  private def wrapAsElidedIfNecessary(steps: Seq[Step], description: String): Option[Step] = {
    steps match {
      case Nil =>
        None
      case Seq(singleStep) =>
        Some(singleStep)
      case _ =>
        Some(Step.Elided(steps, None, Some(description)))
    }
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

  sealed trait OperatorTree {
    def baseTerm: Term
    def allLeaves: Seq[Term]
    def isRearranged(other: OperatorTree): Boolean = other.allLeaves.toSet == allLeaves.toSet
    def contains(other: OperatorTree): Boolean = other.allLeaves.toSet.subsetOf(allLeaves.toSet)
  }
  case class Leaf(baseTerm: Term) extends OperatorTree {
    override def allLeaves: Seq[Term] = Seq(baseTerm)
  }
  case class Operator(l: OperatorTree, r: OperatorTree, baseTerm: Term) extends OperatorTree {
    override def allLeaves: Seq[Term] = l.allLeaves ++ r.allLeaves
  }

  private def rearrange(
    baseLhs: OperatorTree,
    baseRhs: OperatorTree,
    operatorDefinition: Term,
    equalityDefinition: StatementDefinition,
    associativityInference: Inference,
    commutativityInference: Inference,
    equalityReversalInference: Inference,
    equalityTransitivityInference: Inference,
    equalityExpansionInference: Inference,
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Option[Step] = {
    def operator(a: Term, b: Term): Term = {
      operatorDefinition.specify(Seq(a, b), 0, stepContext.externalDepth)
    }
    def transitivityStep(a: Term, b: Term, c: Term): Step = {
      Step.Assertion(
        equalityDefinition(a, c),
        equalityTransitivityInference.summary,
        Seq(Premise.Pending(equalityDefinition(a, b)), Premise.Pending(equalityDefinition(b, c))),
        Substitutions(terms = equalityTransitivityInference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap))
    }
    def normalisedTriple(a: Term, b: Term, c: Term): Term = {
      operator(a, operator(b, c))
    }
    def reversedTriple(a: Term, b: Term, c: Term): Term = {
      operator(operator(a, b), c)
    }
    def wrap(wrappingFunction: Term, t: Term): Term = {
      wrappingFunction.specify(Seq(t), 0, stepContext.externalDepth)
    }
    def wrappingStep(wrappingFunction: Term, a: Term, b: Term): Step = {
      Step.Assertion(
        equalityDefinition(wrap(wrappingFunction, a), wrap(wrappingFunction, b)),
        equalityExpansionInference.summary,
        Seq(Premise.Pending(equalityDefinition(a, b))),
        Substitutions(
          terms = equalityExpansionInference.requiredSubstitutions.terms.zip(Seq(a, b)).toMap,
          functions = equalityExpansionInference.requiredSubstitutions.functions.zip(Seq(wrappingFunction)).toMap))
    }
    def associativity(a: Term, b: Term, c: Term, wrappingFunction: Term): Seq[Step] = {
      val normalised = normalisedTriple(a, b, c)
      val reversed = reversedTriple(a, b, c)
      val assertionSteps = getAssertionWithPremises(
        associativityInference.summary,
        Substitutions(terms = associativityInference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap),
        stepContext,
        premiseContext,
        entryContext)
      val steps = if (wrappingFunction.isInstanceOf[FunctionParameter]) {
        assertionSteps
      } else {
        Seq(Step.Elided(
          assertionSteps :+ wrappingStep(wrappingFunction, normalised, reversed),
          Some(associativityInference.summary),
          None))
      }
      steps :+
        transitivityStep(
          baseLhs.baseTerm,
          wrap(wrappingFunction, normalised),
          wrap(wrappingFunction, reversed))
    }
    def reverseAssociativity(a: Term, b: Term, c: Term, wrappingFunction: Term): Seq[Step] = {
      val normalised = normalisedTriple(a, b, c)
      val reversed = reversedTriple(a, b, c)
      val associativityAndReversalSteps =
        getAssertionWithPremises(
          associativityInference.summary,
          Substitutions(terms = associativityInference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap),
          stepContext,
          premiseContext,
          entryContext) :+
        Step.Assertion(
          equalityDefinition(reversed, normalised),
          equalityReversalInference.summary,
          Seq(Premise.Pending(equalityDefinition(normalised, reversed))),
          Substitutions(terms = equalityReversalInference.requiredSubstitutions.terms.zip(Seq(normalised, reversed)).toMap))
      val stepsToElide = if (wrappingFunction.isInstanceOf[FunctionParameter]) {
        associativityAndReversalSteps
      } else {
        associativityAndReversalSteps :+ wrappingStep(wrappingFunction, reversed, normalised)
      }
      Seq(
        Step.Elided(
          stepsToElide,
          Some(associativityInference.summary),
          None),
        transitivityStep(
          baseLhs.baseTerm,
          wrap(wrappingFunction, reversed),
          wrap(wrappingFunction, normalised)))
    }
    def commutativity(a: Term, b: Term, wrappingFunction: Term): Seq[Step] = {
      val forward = operator(a, b)
      val reverse = operator(b, a)
      val assertionSteps = getAssertionWithPremises(
        commutativityInference.summary,
        Substitutions(terms = commutativityInference.requiredSubstitutions.terms.zip(Seq(a, b)).toMap),
        stepContext,
        premiseContext,
        entryContext)
      val steps = if (wrappingFunction.isInstanceOf[FunctionParameter]) {
        assertionSteps
      } else {
        Seq(
          Step.Elided(
            assertionSteps :+ wrappingStep(wrappingFunction, forward, reverse),
            Some(commutativityInference.summary),
            None))
      }
      steps :+ transitivityStep(
        baseLhs.baseTerm,
        wrap(wrappingFunction, forward),
        wrap(wrappingFunction, reverse))
    }
    def addRight(wrappingFunction: Term, rhs: OperatorTree): Term = {
      wrap(wrappingFunction, operator(FunctionParameter(0, stepContext.externalDepth), rhs.baseTerm))
    }

    def pullLeft(tree: OperatorTree, targetLeft: OperatorTree, wrappingFunction: Term): Option[(Seq[Step], OperatorTree)] = {
      tree match {
        case Operator(`targetLeft`, r, _) =>
          Some((Nil, r))
        case Operator(l, r, _) if l.isRearranged(targetLeft) =>
          for {
            leftSteps <- matchTrees(l, targetLeft, addRight(wrappingFunction, r))
          } yield (leftSteps, r)
        case Operator(l, r, _) if l.contains(targetLeft) =>
          for {
            (steps, remainingRight) <- pullLeft(
              l,
              targetLeft,
              wrappingFunction.specify(
                Seq(operator(FunctionParameter(0, stepContext.externalDepth), r.baseTerm)),
                0,
                stepContext.externalDepth))
            associativitySteps = reverseAssociativity(targetLeft.baseTerm, remainingRight.baseTerm, r.baseTerm, wrappingFunction)
          } yield (steps ++ associativitySteps, Operator(remainingRight, r, operator(remainingRight.baseTerm, r.baseTerm)))
        case Operator(l, r, _) if r.contains(targetLeft) =>
          for {
            (steps, remainingRight) <- pullLeft(Operator(r, l, operator(r.baseTerm, l.baseTerm)), targetLeft, wrappingFunction)
            commutativitySteps = commutativity(l.baseTerm, r.baseTerm, wrappingFunction)
          } yield (commutativitySteps ++ steps, remainingRight)
        case _ =>
          targetLeft match {
            case Operator(targetLeftLeft, targetLeftRight, _) =>
              for {
                (stepsForLeftLeft, treeWithoutLeftLeft) <- pullLeft(tree, targetLeftLeft, wrappingFunction)
                (stepsForLeftRight, treeWithoutLeft) <- pullLeft(
                  treeWithoutLeftLeft,
                  targetLeftRight,
                  wrap(wrappingFunction, operator(targetLeftLeft.baseTerm, FunctionParameter(0, stepContext.externalDepth))))
                associativitySteps = associativity(targetLeftLeft.baseTerm, targetLeftRight.baseTerm, treeWithoutLeft.baseTerm, wrappingFunction)
              } yield (stepsForLeftLeft ++ stepsForLeftRight ++ associativitySteps, treeWithoutLeft)
            case _ =>
              None
          }
      }
    }
    def matchTrees(lhs: OperatorTree, rhs: OperatorTree, wrappingFunction: Term): Option[Seq[Step]] = {
      rhs match {
        case Operator(rhsLeft, rhsRight, _) =>
          for {
            (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, wrappingFunction)
            stepsToMatchRight <- matchTrees(
              lhsRight,
              rhsRight,
              wrappingFunction.specify(
                Seq(operator(rhsLeft.baseTerm, FunctionParameter(0, stepContext.externalDepth))),
                0,
                stepContext.externalDepth))
          } yield stepsToPullLeft ++ stepsToMatchRight
        case Leaf(t) if lhs.baseTerm == t =>
          Some(Nil)
        case _ =>
          None
      }
    }
    matchTrees(baseLhs, baseRhs, FunctionParameter(0, stepContext.externalDepth))
      .map(steps => steps.take(1) ++ steps.drop(2))
      .flatMap(wrapAsElidedIfNecessary(_, "Rearranged"))
  }

  def rearrange(targetStatement: Statement, entryContext: EntryContext, premiseContext: PremiseContext, stepContext: StepContext): Option[Step] = {
    def disassemble(term: Term, operator: Term): OperatorTree = {
      operator.calculateArguments(term, Map.empty, 0, stepContext.externalDepth).flatMap { map =>
        for {
          a <- map.get(0)
          b <- map.get(1)
        } yield Operator(disassemble(a, operator), disassemble(b, operator), term)
      }.getOrElse(Leaf(term))
    }

    for {
      (lhs, rhs, equalityDefinition) <- entryContext.matchEqualityStatement(targetStatement)
      (operator, commutativityInference, associativityInference) <- entryContext.findRearrangableFunctions(equalityDefinition)
        .map { case (f, c, a) => (f.insertExternalParameters(stepContext.externalDepth), c, a)}
        .find { case (f, _, _) =>
          f.calculateArguments(lhs, Map.empty, 0, stepContext.externalDepth).nonEmpty
        }
      equalityReversalInference <- entryContext.findReversalInference(equalityDefinition)
      equalityTransitivityInference <- entryContext.findTransitivityInference(equalityDefinition)
      equalityExpansionInference <- entryContext.findExpansionInference(equalityDefinition)
      lhsTree = disassemble(lhs, operator)
      rhsTree = disassemble(rhs, operator)
      result <- rearrange(
        lhsTree,
        rhsTree,
        operator,
        equalityDefinition,
        associativityInference,
        commutativityInference,
        equalityReversalInference,
        equalityTransitivityInference,
        equalityExpansionInference,
        entryContext,
        premiseContext,
        stepContext)
    } yield result
  }
}
