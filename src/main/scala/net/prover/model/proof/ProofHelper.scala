package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
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

  def findFact(target: Statement, stepContext: StepContext): Option[Step.Assertion] = {
    stepContext.entryContext.inferences
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

  def extract(targetStatement: Statement, stepContext: StepContext): Option[Step] = {
    val statementExtractionInferences = stepContext.entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, firstPremise +: otherPremises, conclusion)
        if conclusion.singleStatementVariable.isDefined &&
          inference.requiredSubstitutions.copy(statements = Nil).isEmpty &&
          firstPremise.requiredSubstitutions.contains(inference.requiredSubstitutions) &&
          (conclusion.complexity < firstPremise.complexity || firstPremise.requiredSubstitutions.statements.length > 1)

      =>
        (inference, firstPremise, otherPremises)
    }
    val predicateSpecificationInferences = stepContext.entryContext.availableEntries.ofType[Inference].collect {
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
    val rewriteInferences = stepContext.entryContext.rewriteInferences

    def matchDirectly(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      for {
        terms <- extractionCandidate.calculateArguments(targetStatement, Map.empty, 0, stepContext.externalDepth)
      } yield (terms, Nil)
    }

    def extractRecursively(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      statementExtractionInferences.iterator.findFirst {
        case (inference, firstPremise, otherPremises) =>
          (for {
            extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth)
            extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions, 0, stepContext.externalDepth).toSeq
            (conclusionTerms, innerSteps) <- extractFromStatement(extractedConclusion, termsSoFar).toSeq
            extractedOtherPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions, 0, stepContext.externalDepth)).traverseOption.toSeq
            (premiseSteps, terms) <- PremiseFinder.findParameterisedPremiseSteps(extractedOtherPremises, conclusionTerms, stepContext)
            substitutedFirstPremise <- extractionCandidate.specify(terms, 0, stepContext.externalDepth).toSeq
            substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise, Substitutions.empty, 0, stepContext.externalDepth)
            substitutedOtherPremises <- otherPremises.map(_.applySubstitutions(substitutions, 0, stepContext.externalDepth)).traverseOption.toSeq
            substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth)
            assertionStep = Step.Assertion(
              substitutedConclusion,
              inference.summary,
              (substitutedFirstPremise +: substitutedOtherPremises).map(Premise.Pending),
              substitutions)
            newStep <- wrapAsElidedIfNecessary(premiseSteps :+ assertionStep, inference)
          } yield (terms, newStep +: innerSteps)).headOption
      } orElse predicateSpecificationInferences.iterator.findFirst {
        case (inference, singlePremise, predicateName, argumentNames) =>
          (for {
            extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1)
            extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
            nextPremise <- extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1).toSeq
            (terms, laterSteps) <- extractFromStatement(nextPremise, termsSoFar + argumentNames.length).toSeq
            specifiedPremise <- extractionCandidate.specify(terms, 0, stepContext.externalDepth).toSeq
            substitutionsWithTerms <- singlePremise.calculateSubstitutions(specifiedPremise, Substitutions.empty, 0, stepContext.externalDepth)
              .map(_.copy(terms = argumentNames.mapWithIndex((n, i) => n -> terms(termsSoFar + i)).toMap))
            substitutedConclusion <- inference.conclusion.applySubstitutions(substitutionsWithTerms, 0, stepContext.externalDepth).toSeq
            specifiedConclusion <- substitutedConclusion.specify(terms, 0, stepContext.externalDepth).toSeq
            newStep = Step.Assertion(
              specifiedConclusion,
              inference.summary,
              Seq(Premise.Pending(specifiedPremise)),
              substitutionsWithTerms)
          } yield (terms, newStep +: laterSteps)).headOption
      }
    }

    def extractWithoutRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      matchDirectly(extractionCandidate, termsSoFar) orElse
        extractRecursively(extractionCandidate, termsSoFar)
    }

    def extractWithRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      rewriteInferences.iterator.findFirst { case (inference, singlePremise) =>
        (for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth)
          rewrittenStatement <- inference.conclusion.applySubstitutions(extractionSubstitutions, 0, stepContext.externalDepth).toSeq
          (terms, innerSteps) <- extractWithoutRewrite(rewrittenStatement, termsSoFar).toSeq
          substitutedPremise <- extractionCandidate.specify(terms, 0, stepContext.externalDepth).toSeq
          substitutions <- singlePremise.calculateSubstitutions(substitutedPremise, Substitutions.empty, 0, stepContext.externalDepth)
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth).toSeq
          newStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            Seq(Premise.Pending(substitutedPremise)),
            substitutions)
        } yield (terms, newStep +: innerSteps)).headOption
      }
    }

    def extractFromStatement(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      extractWithoutRewrite(extractionCandidate, termsSoFar) orElse
        extractWithRewrite(extractionCandidate, termsSoFar)
    }

    def extractPremise(premise: Premise.SingleLinePremise): Option[Step] = {
      extractFromStatement(premise.statement, 0).map(_._2).flatMap(wrapAsElidedIfNecessary(_, "Extracted"))
    }

    stepContext.allPremisesSimplestFirst.mapFind(extractPremise)
  }

  def getAssertionWithPremises(
    inference: Inference.Summary,
    substitutions: Substitutions,
    stepContext: StepContext
  ): Seq[Step] = {
    val premiseStatements = inference.substitutePremises(substitutions, stepContext)
    val conclusion = inference.substituteConclusion(substitutions, stepContext)
    val (targetSteps, premiseSteps) = premiseStatements.foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((targetStepsSoFar, premiseStepsSoFar), premiseStatement) =>
      PremiseFinder.findPremiseSteps(premiseStatement, stepContext) match {
        case Some(newPremiseSteps) =>
          (targetStepsSoFar, premiseStepsSoFar ++ newPremiseSteps)
        case None =>
          val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement, stepContext)
          val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
            PremiseFinder.findPremiseSteps(deconstructedStatement, stepContext) match {
              case Some(newPremiseSteps) =>
                (otherTargetStepsSoFar, otherPremiseStepsSoFar ++ newPremiseSteps)
              case None =>
                (otherTargetStepsSoFar :+ Step.Target(deconstructedStatement), otherPremiseStepsSoFar)
            }
          }
          (targetStepsSoFar ++ deconstructionTargetSteps, premiseStepsSoFar ++ deconstructionPremiseSteps ++ deconstructionSteps)
      }
    }
    val assertionStep = Step.Assertion(
      conclusion,
      inference,
      premiseStatements.map(Premise.Pending),
      substitutions)
    if (InferenceTypes.isTransitivity(inference)) {
      (targetSteps ++ premiseSteps) :+ assertionStep
    } else {
      val baseStep = if (premiseSteps.nonEmpty) {
        Step.Elided(premiseSteps :+ assertionStep, Some(inference.summary), None)
      } else {
        assertionStep
      }
      targetSteps :+ baseStep
    }
  }

  def wrapAsElidedIfNecessary(steps: Seq[Step], description: String): Option[Step] = {
    wrapAsElidedIfNecessary(steps, Step.Elided(_, None, Some(description)))
  }

  def wrapAsElidedIfNecessary(steps: Seq[Step], inference: Inference): Option[Step] = {
    wrapAsElidedIfNecessary(steps, Step.Elided(_, Some(inference.summary), None))
  }

  private def wrapAsElidedIfNecessary(steps: Seq[Step], f: Seq[Step] => Step.Elided): Option[Step] = {
    steps match {
      case Nil =>
        None
      case Seq(singleStep) =>
        Some(singleStep)
      case _ =>
        Some(f(steps))
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
    stepContext: StepContext
  ): Option[Step] = {
    def operator(a: Term, b: Term): Term = {
      operatorDefinition.specify(Seq(a, b), 0, stepContext.externalDepth).get
    }
    def reversalStep(a: Term, b: Term): Step = {
      Step.Assertion(
        equalityDefinition(b, a),
        equalityReversalInference.summary,
        Seq(Premise.Pending(equalityDefinition(a, b))),
        Substitutions(terms = equalityReversalInference.requiredSubstitutions.terms.zip(Seq(a, b)).toMap))
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
      wrappingFunction.specify(Seq(t), 0, stepContext.externalDepth).get
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
        stepContext)
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
          stepContext) :+
        reversalStep(normalised, reversed)
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
        stepContext)
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
            leftSteps <- matchTreesRaw(l, targetLeft, addRight(wrappingFunction, r))
          } yield (leftSteps, r)
        case Operator(l, r, _) if l.contains(targetLeft) =>
          for {
            (steps, remainingRight) <- pullLeft(
              l,
              targetLeft,
              wrappingFunction.specify(
                Seq(operator(FunctionParameter(0, stepContext.externalDepth), r.baseTerm)),
                0,
                stepContext.externalDepth
              ).get)
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
    def matchTreesRaw(lhs: OperatorTree, rhs: OperatorTree, wrappingFunction: Term): Option[Seq[Step]] = {
      rhs match {
        case Operator(rhsLeft, rhsRight, _) =>
          for {
            (stepsToPullLeft, lhsRight) <- pullLeft(lhs, rhsLeft, wrappingFunction)
            stepsToMatchRight <- matchTreesRaw(
              lhsRight,
              rhsRight,
              wrappingFunction.specify(
                Seq(operator(rhsLeft.baseTerm, FunctionParameter(0, stepContext.externalDepth))),
                0,
                stepContext.externalDepth
              ).get)
          } yield stepsToPullLeft ++ stepsToMatchRight
        case Leaf(t) if lhs.baseTerm == t =>
          Some(Nil)
        case _ =>
          None
      }
    }
    def matchTrees(lhs: OperatorTree, rhs: OperatorTree): Option[Seq[Step]] = {
      matchTreesRaw(lhs, rhs, FunctionParameter(0, stepContext.externalDepth)).map(steps => steps.take(1) ++ steps.drop(2))
    }
    def rearrangeDirectly = matchTrees(baseLhs, baseRhs)


    def rearrangeUsingPremise(premiseLhs: OperatorTree, premiseRhs: OperatorTree): Option[Seq[Step]] = {
      (for {
        lhsMatch <- matchTrees(baseLhs, premiseLhs)
        joiner = if (lhsMatch.nonEmpty) Seq(transitivityStep(baseLhs.baseTerm, premiseLhs.baseTerm, premiseRhs.baseTerm)) else Nil
        rhsMatch <- if (lhsMatch.nonEmpty) matchTreesRaw(premiseRhs, baseRhs, FunctionParameter(0, stepContext.externalDepth)) else matchTrees(premiseRhs, baseRhs)
      } yield lhsMatch ++ joiner ++ rhsMatch) orElse
        (for {
          firstMatch <- matchTrees(baseLhs, premiseRhs)
          joiner = Seq(reversalStep(premiseLhs.baseTerm, premiseRhs.baseTerm), transitivityStep(baseLhs.baseTerm, premiseRhs.baseTerm, premiseLhs.baseTerm))
          secondMatch <- matchTreesRaw(premiseLhs, baseRhs, FunctionParameter(0, stepContext.externalDepth))
        } yield firstMatch ++ joiner ++ secondMatch)
    }

    def rearrangeUsingPremises: Option[Seq[Step]] = (for {
      premise <- stepContext.allPremisesSimplestFirst
      (premiseLhsTerm, premiseRhsTerm) <- (premise.statement match {
        case equalityDefinition(l: Term, r: Term) => Some((l, r))
        case _ => None
      }).toSeq
      premiseLhs = disassemble(premiseLhsTerm, operatorDefinition, stepContext)
      premiseRhs = disassemble(premiseRhsTerm, operatorDefinition, stepContext)
      result <- rearrangeUsingPremise(premiseLhs, premiseRhs)
    } yield result).headOption

    (rearrangeDirectly orElse rearrangeUsingPremises).flatMap(wrapAsElidedIfNecessary(_, "Rearranged"))
  }

  private def disassemble(term: Term, operator: Term, stepContext: StepContext): OperatorTree = {
    operator.calculateArguments(term, Map.empty, 0, stepContext.externalDepth).flatMap { map =>
      for {
        a <- map.get(0)
        b <- map.get(1)
      } yield Operator(disassemble(a, operator, stepContext), disassemble(b, operator, stepContext), term)
    }.getOrElse(Leaf(term))
  }

  def rearrange(targetStatement: Statement, stepContext: StepContext): Option[Step] = {
    val entryContext = stepContext.entryContext
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
      lhsTree = disassemble(lhs, operator, stepContext)
      rhsTree = disassemble(rhs, operator, stepContext)
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
        stepContext)
    } yield result
  }

  private def rewrite(
    targetStatement: Statement,
    stepContext: StepContext,
    equalityDefinition: StatementDefinition,
    equalitySubstitutionInference: Inference,
    equalityExpansionInference: Inference,
    equalityTransitivityInference: Inference,
    equalityReversalInference: Inference
  ): Option[Step] = {
    val entryContext = stepContext.entryContext
    val termSimplificationInferences = entryContext.availableEntries.ofType[Inference]
      .collect {
        case inference @ Inference(
          _,
          _,
          equalityDefinition(left: Term, right: Term))
        if left.complexity > right.complexity && left.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions) =>
          (inference, left, right)
      }
    val termDesimplificationInferences = entryContext.availableEntries.ofType[Inference]
      .collect {
        case inference @ Inference(
        _,
        _,
        equalityDefinition(left: Term, right: Term))
          if left.complexity < right.complexity && right.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions) =>
          (inference, left, right)
      }

    def reverseStep(left: Term, right: Term): Step = {
      Step.Assertion(
        equalityDefinition(left, right),
        equalityReversalInference.summary,
        Seq(Premise.Pending(equalityDefinition(right, left))),
        Substitutions(terms = equalityReversalInference.requiredSubstitutions.terms.zip(Seq(right, left)).toMap))
    }
    def expandStep(left: Term, right: Term, function: Term): Step = {
      Step.Assertion(
        equalityDefinition(function.specify(Seq(left), 0, stepContext.externalDepth).get, function.specify(Seq(right), 0, stepContext.externalDepth).get),
        equalityExpansionInference.summary,
        Seq(Premise.Pending(equalityDefinition(left, right))),
        Substitutions(
          terms = equalityExpansionInference.requiredSubstitutions.terms.zip(Seq(left, right)).toMap,
          functions = equalityExpansionInference.requiredSubstitutions.functions.zip(Seq(function)).toMap))
    }

    def findSimplificationsDirectly(premiseTerm: Term, reverse: Boolean): Seq[(Term, Step, Inference)] = {
      (for {
        (inference, left, right) <- termSimplificationInferences
        conclusionSubstitutions <- left.calculateSubstitutions(premiseTerm, Substitutions.empty, 0, stepContext.externalDepth)
        simplifiedTerm <- right.applySubstitutions(conclusionSubstitutions, 0, stepContext.externalDepth).flatMap(_.asOptionalInstanceOf[Term])
        (premiseSteps, substitutedPremises, finalSubstitutions) <- PremiseFinder.findPremiseSteps(inference.premises, conclusionSubstitutions, stepContext)
        assertionStep = Step.Assertion(
          equalityDefinition(premiseTerm, simplifiedTerm),
          inference.summary,
          substitutedPremises.map(Premise.Pending),
          finalSubstitutions)
        steps =
          if (reverse)
            premiseSteps ++ Seq(assertionStep, reverseStep(simplifiedTerm, premiseTerm))
          else
            premiseSteps :+ assertionStep
        step <- wrapAsElidedIfNecessary(steps, inference)
      } yield (simplifiedTerm, step, inference)) ++
        (for {
        (inference, left, right) <- termDesimplificationInferences
        conclusionSubstitutions <- right.calculateSubstitutions(premiseTerm, Substitutions.empty, 0, stepContext.externalDepth)
        simplifiedTerm <- left.applySubstitutions(conclusionSubstitutions, 0, stepContext.externalDepth).flatMap(_.asOptionalInstanceOf[Term])
        (premiseSteps, substitutedPremises, finalSubstitutions) <- PremiseFinder.findPremiseSteps(inference.premises, conclusionSubstitutions, stepContext)
        assertionStep = Step.Assertion(
          equalityDefinition(simplifiedTerm, premiseTerm),
          inference.summary,
          substitutedPremises.map(Premise.Pending),
          finalSubstitutions)
        steps =
          if (!reverse)
            premiseSteps ++ Seq(assertionStep, reverseStep(premiseTerm, simplifiedTerm))
          else
            premiseSteps :+ assertionStep
        step <- wrapAsElidedIfNecessary(steps, inference)
      } yield (simplifiedTerm, step, inference))
    }

    def findSimplificationsWithinExpansion(premiseTerm: Term, wrappingFunction: Term, reverse: Boolean): Seq[(Term, Step, Inference)] = {
      premiseTerm match {
        case DefinedTerm(components, termDefinition) =>
          def helper(previousComponents: Seq[Expression], nextComponents: Seq[Expression], resultsSoFar: Seq[(Term, Step, Inference)]): Seq[(Term, Step, Inference)]  = {
            nextComponents match {
              case (innerTerm: Term) +: moar =>
                val newWrapper = wrappingFunction.specify(
                  Seq(termDefinition((previousComponents :+ FunctionParameter(0, stepContext.externalDepth)) ++ moar: _*)),
                  0,
                  stepContext.externalDepth
                ).get
                val newResults = findSimplifications(innerTerm, reverse) map { case (simplifiedInnerTerm, simplificationStep, simplificationInference) =>
                  val simplifiedTerm = newWrapper.specify(Seq(simplifiedInnerTerm), 0, stepContext.externalDepth).get
                  val expansionStep = if (reverse) expandStep(simplifiedInnerTerm, innerTerm, newWrapper) else expandStep(innerTerm, simplifiedInnerTerm, newWrapper)
                  val elidedStep = Step.Elided(Seq(simplificationStep, expansionStep), Some(simplificationInference.summary), None)
                  (simplifiedTerm, elidedStep, simplificationInference)
                }
                helper(previousComponents :+ innerTerm, moar, resultsSoFar ++ newResults)
              case nonTerm +: moar =>
                helper(previousComponents :+ nonTerm, moar, resultsSoFar)
              case Nil =>
                resultsSoFar
            }
          }
          helper(Nil, components, Nil)
        case _ =>
          Nil
      }
    }

    def findSimplifications(premiseTerm: Term, reverse: Boolean): Seq[(Term, Step, Inference)] = {
      findSimplificationsDirectly(premiseTerm, reverse) ++
        findSimplificationsWithinExpansion(premiseTerm, FunctionParameter(0, stepContext.externalDepth), reverse)
    }

    def findKnownEqualityPath(premiseTerm: Term, targetTerm: Term, wrappingFunction: Term): Option[Seq[(Term, Option[Step])]] = {
      def getWrappingStep(l: Term, r: Term): Seq[Step] = {
        if (wrappingFunction.isInstanceOf[FunctionParameter])
          Nil
        else
          Seq(expandStep(l, r, wrappingFunction))
      }
      def findExactly = if (premiseTerm == targetTerm) Some(Nil) else None
      def findDirectly = PremiseFinder.findPremiseSteps(
        equalityDefinition(premiseTerm, targetTerm),
        stepContext
      ).map(steps => Seq((
        wrappingFunction.specify(Seq(targetTerm), 0, stepContext.externalDepth).get,
        wrapAsElidedIfNecessary(steps ++ getWrappingStep(premiseTerm, targetTerm), "Rewritten"))))
      def findReverse = PremiseFinder.findPremiseSteps(
        equalityDefinition(targetTerm, premiseTerm),
        stepContext
      ).map(steps => Seq((
        wrappingFunction.specify(Seq(targetTerm), 0, stepContext.externalDepth).get,
        wrapAsElidedIfNecessary((steps :+ reverseStep(premiseTerm, targetTerm)) ++ getWrappingStep(premiseTerm, targetTerm), "Rewritten"))))

      def findByComponentsHelper(
        previousComponents: Seq[(Term, Term)],
        nextComponents: Seq[(Term, Term)],
        stepsSoFar: Seq[(Term, Option[Step])],
        getWrapperForComponents: Seq[Term] => Term
      ): Option[Seq[(Term, Option[Step])]]  = {
        nextComponents match {
          case (premiseComponent, targetComponent) +: moreComponents =>
            def newWrapper = getWrapperForComponents((previousComponents.map(_._2) :+ FunctionParameter(0, stepContext.externalDepth)) ++ moreComponents.map(_._1))
            for {
              theseSteps <- findKnownEqualityPath(premiseComponent, targetComponent, newWrapper)
              result <- findByComponentsHelper(previousComponents :+ (premiseComponent, targetComponent), moreComponents, stepsSoFar ++ theseSteps, getWrapperForComponents)
            } yield result
          case Nil =>
            Some(stepsSoFar)
        }
      }
      def findByComponents = (premiseTerm, targetTerm) match {
        case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition =>
          for {
            premiseTerms <- premiseComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            targetTerms <- targetComponents.map(_.asOptionalInstanceOf[Term]).traverseOption
            componentTerms <- premiseTerms.zipStrict(targetTerms)
            result <- findByComponentsHelper(Nil, componentTerms, Nil, components => wrappingFunction.specify(Seq(premiseDefinition(components:_*)), 0, stepContext.externalDepth).get)
          } yield result
        case (FunctionApplication(f, premiseComponents), FunctionApplication(g, targetComponents)) if f == g =>
          for {
            componentTerms <- premiseComponents.zipStrict(targetComponents)
            result <- findByComponentsHelper(Nil, componentTerms, Nil, arguments => wrappingFunction.specify(Seq(FunctionApplication(f, arguments)), 0, stepContext.externalDepth).get)
          } yield result
        case _ =>
          None
      }

      findExactly orElse findDirectly orElse findReverse orElse findByComponents
    }
    def findKnownEqualitySteps(premiseTerm: Term, targetTerm: Term, wrappingFunction: Term): Option[Seq[Step]] = {
      for {
        simplificationPath <- findKnownEqualityPath(premiseTerm, targetTerm, wrappingFunction)
        transitivitySteps <- buildTransitivity(premiseTerm, simplificationPath)
      } yield transitivitySteps
    }
    def findByKnownEquality(premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement): Option[Step] = {
      for {
        simplificationSteps <- findKnownEqualitySteps(premiseTerm, targetTerm, FunctionParameter(0, stepContext.externalDepth))
        result <- wrap(simplificationSteps, premiseTerm, targetTerm, wrappingPredicate, "Rewritten")
      } yield result
    }

    def findSimplificationPath(premiseTerm: Term, targetTerm: Term, reverse: Boolean): Option[Seq[(Term, Option[Step])]] = {
      def findDirectly = if (premiseTerm == targetTerm) Some(Nil) else None
      def findBySimplifying = {
        if (!reverse && premiseTerm.complexity > targetTerm.complexity)
          (for {
            (simplifiedPremise, simplificationStep, _) <- findSimplifications(premiseTerm, reverse = false)
            remainingSteps <- findSimplificationPath(simplifiedPremise, targetTerm, reverse = false)
          } yield (simplifiedPremise, Some(simplificationStep)) +: remainingSteps).headOption
        else if (reverse && premiseTerm.complexity < targetTerm.complexity)
          (for {
            (simplifiedTarget, simplificationStep, _) <- findSimplifications(targetTerm, reverse = true)
            remainingSteps <- findSimplificationPath(premiseTerm, simplifiedTarget, reverse = true)
          } yield remainingSteps :+ (targetTerm, Some(simplificationStep))).headOption
        else
          None
      }
      findDirectly orElse findBySimplifying
    }

    def buildTransitivity(left: Term, path: Seq[(Term, Option[Step])]): Option[Seq[Step]] = {
      for {
        (firstRight, firstStep) <- path.headOption
        remainingPath = path.tail
      } yield remainingPath.foldLeft((firstRight, firstStep.toSeq)) { case ((previousRight, stepsSoFar), (currentRight, currentStep)) =>
        (currentRight, stepsSoFar ++ currentStep.toSeq :+ Step.Assertion(
          equalityDefinition(left, currentRight),
          equalityTransitivityInference.summary,
          Seq(Premise.Pending(equalityDefinition(left, previousRight)), Premise.Pending(equalityDefinition(previousRight, currentRight))),
          Substitutions(terms = equalityTransitivityInference.requiredSubstitutions.terms.zip(Seq(left, previousRight, currentRight)).toMap)))
      }._2
    }

    def findSimplificationSteps(
      premiseTerm: Term,
      targetTerm: Term
    ): Option[Seq[Step]] = {
      for {
        simplificationPath <- findSimplificationPath(premiseTerm, targetTerm, premiseTerm.complexity < targetTerm.complexity)
        transitivitySteps <- buildTransitivity(premiseTerm, simplificationPath)
      } yield transitivitySteps
    }

    def wrap(steps: Seq[Step], premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement, description: String): Option[Step] = {
      val wrappingStep = Step.Assertion(
        wrappingPredicate.specify(Seq(targetTerm), 0, stepContext.externalDepth).get,
        equalitySubstitutionInference.summary,
        Seq(
          Premise.Pending(equalityDefinition(premiseTerm, targetTerm)),
          Premise.Pending(wrappingPredicate.specify(Seq(premiseTerm), 0, stepContext.externalDepth).get)),
        Substitutions(
          terms = equalitySubstitutionInference.requiredSubstitutions.terms.zip(Seq(premiseTerm, targetTerm)).toMap,
          predicates = equalitySubstitutionInference.requiredSubstitutions.predicates.zip(Seq(wrappingPredicate)).toMap))
      wrapAsElidedIfNecessary(steps :+ wrappingStep, description)
    }

    def findBySimplifying(premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement): Option[Step] = {
      for {
        simplificationSteps <- findSimplificationSteps(premiseTerm, targetTerm)
        result <- wrap(simplificationSteps, premiseTerm, targetTerm, wrappingPredicate, if (premiseTerm.complexity > targetTerm.complexity) "Simplified" else "Expanded")
      } yield result
    }

    def rewriteComponents(premiseComponents: Seq[Expression], targetComponents: Seq[Expression], wrappingFunction: Seq[Expression] => Statement): Option[Seq[Step]] = {
      def helper(previousComponents: Seq[(Expression, Expression)], nextComponents: Seq[(Expression, Expression)], currentSteps: Seq[Step]): Option[Seq[Step]] = {
        nextComponents match {
          case Nil =>
            Some(currentSteps)
          case (premise: Statement, target: Statement) +: moar =>
            rewriteStatement(premise, target, s => wrappingFunction((previousComponents.map(_._2) :+ s) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case (premise: Term, target: Term) +: moar =>
            rewriteTerm(premise, target, wrappingFunction((previousComponents.map(_._2) :+ FunctionParameter(0, stepContext.externalDepth)) ++ moar.map(_._1)))
              .flatMap(newSteps => helper(previousComponents :+ (premise, target), moar, currentSteps ++ newSteps))
          case _ =>
            None
        }
      }
      premiseComponents.zipStrict(targetComponents).flatMap(helper(Nil, _, Nil))
    }

    def rewriteStatement(premiseStatement: Statement, currentTarget: Statement, wrappingFunction: Statement => Statement): Option[Seq[Step]] = {
      if (premiseStatement == currentTarget)
        Some(Nil)
      else (premiseStatement, currentTarget) match {
        case (DefinedStatement(premiseComponents, premiseDefinition), DefinedStatement(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
          rewriteComponents(premiseComponents, targetComponents, components => wrappingFunction(premiseDefinition(components:_*)))
        case _ =>
          None
      }
    }

    def rewriteTerm(premiseTerm: Term, targetTerm: Term, wrappingPredicate: Statement): Option[Seq[Step]] = {
      if (premiseTerm == targetTerm)
        Some(Nil)
      else
        (premiseTerm, targetTerm) match {
          case (DefinedTerm(premiseComponents, premiseDefinition), DefinedTerm(targetComponents, targetDefinition)) if premiseDefinition == targetDefinition && premiseDefinition.boundVariableNames.isEmpty =>
            rewriteComponents(premiseComponents, targetComponents, components => wrappingPredicate.specify(Seq(premiseDefinition(components:_*)), 0, stepContext.externalDepth).get)
          case _ =>
            (findBySimplifying(premiseTerm, targetTerm, wrappingPredicate) orElse findByKnownEquality(premiseTerm, targetTerm, wrappingPredicate))
              .map(Seq(_))
        }
    }

    def rewritePremise(premise: Premise): Option[Seq[Step]] = {
      rewriteStatement(premise.statement, targetStatement, identity)
    }

    val resultStepsOption = (targetStatement match {
      case equalityDefinition(premiseTerm: Term, targetTerm: Term) =>
        findSimplificationSteps(premiseTerm, targetTerm) orElse findKnownEqualitySteps(premiseTerm, targetTerm, FunctionParameter(0, stepContext.externalDepth))
      case _ =>
        None
    }) orElse stepContext.allPremisesSimplestFirst.mapFind(rewritePremise)

    resultStepsOption.flatMap(wrapAsElidedIfNecessary(_, "Rewritten"))
  }

  def rewrite(
    targetStatement: Statement,
    stepContext: StepContext
  ): Option[Step] = {
    val entryContext = stepContext.entryContext
    for {
      equalityDefinition <- entryContext.equalityDefinitionOption
      equalitySubstitutionInference <- entryContext.findSubstitutionInference(equalityDefinition)
      equalityExpansionInference <- entryContext.findExpansionInference(equalityDefinition)
      equalityTransitivityInference <- entryContext.findTransitivityInference(equalityDefinition)
      equalityReversalInference <- entryContext.findReversalInference(equalityDefinition)
      result <- rewrite(targetStatement, stepContext, equalityDefinition, equalitySubstitutionInference, equalityExpansionInference, equalityTransitivityInference, equalityReversalInference)
    } yield result
  }
}
