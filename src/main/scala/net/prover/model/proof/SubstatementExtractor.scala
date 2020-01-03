package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

class SubstatementExtractor(implicit stepProvingContext: StepProvingContext) {
  import stepProvingContext._
  import provingContext._

  private type Result[T] = Iterator[(Map[Int, Term], Seq[Step], T)]

  private def extractStatementFromInference[T](
    extractionCandidate: Statement,
    termsSoFar: Int,
    recurse: (Statement, Int) => Result[T],
    inference: Inference,
    firstPremise: Statement,
    otherPremises: Seq[Statement]
  ): Result[T] = {
    for {
      extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).iterator
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).iterator
      (conclusionTerms, innerSteps, t) <- recurse(extractedConclusion, termsSoFar)
      extractedOtherPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions)).traverseOption.iterator
      (premiseSteps, terms) <- PremiseFinder.findParameterisedPremiseSteps(extractedOtherPremises, conclusionTerms)
      substitutedFirstPremise <- extractionCandidate.specify(terms)
      substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise).flatMap(_.confirmTotality)
      substitutedOtherPremises <- otherPremises.map(_.applySubstitutions(substitutions)).traverseOption
      substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
      assertionStep = Step.Assertion(
        substitutedConclusion,
        inference.summary,
        (substitutedFirstPremise +: substitutedOtherPremises).map(Premise.Pending),
        substitutions)
      newStep <- Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference.summary)
    } yield (terms, newStep +: innerSteps, t)
  }

  private def extractStatement[T](
    extractionCandidate: Statement,
    termsSoFar: Int,
    callStack: Seq[(Statement, Inference)],
    recurse: (Statement, Int, Seq[(Statement, Inference)]) => Result[T]
  ): Result[T] = {
    statementExtractionInferences.iterator.flatMap { case (inference, firstPremise, otherPremises) =>
      extractStatementFromInference(
        extractionCandidate,
        termsSoFar,
        (s, i) => recurse(s, i, callStack :+ (extractionCandidate, inference)),
        inference,
        firstPremise,
        otherPremises)
    }
  }

  private def extractPredicate[T](
    extractionCandidate: Statement,
    termsSoFar: Int,
    callStack: Seq[(Statement, Inference)],
    recurse: (Statement, Int, Seq[(Statement, Inference)]) => Result[T]
  ): Result[T] = {
    predicateSpecificationInferences.iterator.flatMap {
      case (inference, singlePremise, predicateName, argumentNames) =>
        for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1).flatMap(_.confirmTotality).iterator
          extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).iterator
          nextPremise <- extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1).iterator
          (terms, laterSteps, t) <- recurse(nextPremise, termsSoFar + argumentNames.length, callStack :+ (extractionCandidate, inference))
          specifiedPremise <- extractionCandidate.specify(terms, 0, stepContext.externalDepth).iterator
          substitutionsWithTerms <- singlePremise.calculateSubstitutions(specifiedPremise).flatMap(_.confirmTotality).iterator
            .map(_.copy(terms = argumentNames.mapWithIndex((n, i) => n -> terms(termsSoFar + i)).toMap))
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutionsWithTerms).iterator
          specifiedConclusion <- substitutedConclusion.specify(terms, 0, stepContext.externalDepth)
          newStep = Step.Assertion(
            specifiedConclusion,
            inference.summary,
            Seq(Premise.Pending(specifiedPremise)),
            substitutionsWithTerms)
        } yield (terms, newStep +: laterSteps, t)
    }
  }

  private def deconstructDefinition[T](
    extractionCandidate: Statement,
    termsSoFar: Int,
    callStack: Seq[(Statement, Inference)],
    recurse: (Statement, Int, Seq[(Statement, Inference)]) => Result[T]
  ): Result[T] = {
    for {
      definedStatement <- extractionCandidate.asOptionalInstanceOf[DefinedStatement].iterator
      definition = definedStatement.definition
      if (
        entryContext.typeDefinitions.map(_.statementDefinition) ++
        entryContext.propertyDefinitionsByType.values.flatten.map(_.statementDefinition)
      ).contains(definition)
      destructionInference <- definedStatement.definition.destructionInference.iterator
      premise <- destructionInference.premises.single.iterator
      extractedSubstitutions <- premise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).iterator
      deconstructedStatement <- destructionInference.conclusion.applySubstitutions(extractedSubstitutions).iterator
      (terms, innerSteps, t) <- recurse(deconstructedStatement, termsSoFar, callStack :+ (extractionCandidate, destructionInference))
      substitutedPremise <- extractionCandidate.specify(terms).iterator
      substitutions <- premise.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality).iterator
      substitutedConclusion <- destructionInference.conclusion.applySubstitutions(substitutions).iterator
      assertionStep = Step.Assertion(
        substitutedConclusion,
        destructionInference.summary,
        Seq(Premise.Pending(substitutedPremise)),
        substitutions)
    } yield (terms, assertionStep +: innerSteps, t)
  }

  private def extractRecursively[T](
    extractionCandidate: Statement,
    matchAtEnd: (Statement, Int) => Result[T]
  ): Result[T] = {
    def recurse(s: Statement, i: Int, callStack: Seq[(Statement, Inference)]): Result[T] =
      matchAtEnd(s, i) ++
        extractStatement(s, i, callStack, recurse) ++
        extractPredicate(s, i, callStack, recurse) ++
        deconstructDefinition(s, i, callStack, recurse)
    recurse(extractionCandidate, 0, Nil)
  }

  private def extractFromStatement[T](extractionCandidate: Statement, matchAtEnd: (Statement, Int) => Result[T]): Option[(Seq[Step], T)] = {
    for {
      (_, steps, t) <- extractRecursively(extractionCandidate, matchAtEnd).headOption
    } yield (steps, t)
  }
  private def extractFromPremise[T](premiseStatement: Statement, matchAtEnd: (Statement, Int) => Result[T]): Option[(Step, T)] = for {
    (steps, t) <- extractFromStatement(premiseStatement, matchAtEnd)
    finalStep <- Step.Elided.ifNecessary(steps, "Extracted")
  } yield (finalStep, t)
  private def extractFromFact[T](fact: Inference, matchAtEnd: (Statement, Int) => Result[T]): Option[(Step, T)] = {
    for {
      (steps, t) <- extractFromStatement(fact.conclusion, matchAtEnd)
      assertion = Step.Assertion(fact.conclusion, fact.summary, Nil, Substitutions.empty)
      finalStep <- Step.Elided.ifNecessary(assertion +: steps, fact)
    } yield (finalStep, t)
  }

  private def extractFromPremisesOrFact[T](targetStatement: Statement, matchAtEnd: (Statement, Int) => Result[T]): Option[(Step, T)] = {
    def extractFromPremises: Option[(Step, T)] = allPremisesSimplestFirst.mapFind(p => extractFromPremise(p.statement, matchAtEnd))
    def extractFromFacts: Option[(Step, T)] = provingContext.facts.mapFind(extractFromFact(_, matchAtEnd))
    extractFromPremises orElse extractFromFacts
  }
  private def matchDirectly(targetStatement: Statement, extractionCandidate: Statement, termsSoFar: Int): Option[Map[Int, Term]] = {
    extractionCandidate.calculateArguments(targetStatement, Map.empty)
  }
  private def matchDirectlyOrFromSingleInference(targetStatement: Statement, extractionCandidate: Statement, termsSoFar: Int): Result[Unit] = {
    matchDirectly(targetStatement, extractionCandidate, termsSoFar).map((_, Nil, ())).iterator ++
      finalStatementExtractionInferences.flatMap { case (inference, firstPremise, otherPremises) =>
        extractStatementFromInference(
          extractionCandidate,
          termsSoFar,
          matchDirectly(targetStatement, _, _).map((_, Nil, ())).iterator,
          inference,
          firstPremise,
          otherPremises)
      }
  }

  private def extractDirectly(targetStatement: Statement, f: ((Statement, Int) => Result[Unit]) => Option[(Step, Unit)]): Option[Step] = {
    f(matchDirectlyOrFromSingleInference(targetStatement, _, _)).map(_._1)
  }

  def extract(targetStatement: Statement): Option[Step] = {
    extractDirectly(targetStatement, extractFromPremisesOrFact(targetStatement, _))
  }

  def extractFromPremise(premiseStatement: Statement, targetStatement: Statement): Option[Step] = {
    extractDirectly(targetStatement, extractFromPremise(premiseStatement, _))
  }
  def extractFromFact(fact: Inference, targetStatement: Statement): Option[Step] = {
    extractDirectly(targetStatement, extractFromFact(fact, _))
  }

  private def extractStatementWithTarget(targetStatement: Statement, extractionCandidate: Statement, termsSoFar: Int): Result[Step.Target] = {
    statementExtractionInferences.iterator.mapCollect {
      case (inference, firstPremise, otherPremises) =>
        (for {
          otherPremise <- otherPremises.single
          extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
          extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
          terms <- matchDirectly(targetStatement, extractedConclusion, termsSoFar)
          substitutedFirstPremise <- extractionCandidate.specify(terms)
          substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise).flatMap(_.confirmTotality)
          substitutedOtherPremise <- otherPremise.applySubstitutions(substitutions)
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
          assertionStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            Seq(substitutedFirstPremise, substitutedOtherPremise).map(Premise.Pending),
            substitutions)
        } yield (terms, Seq(assertionStep), Step.Target(substitutedOtherPremise)))
    }
  }

  def extractFromPremiseWithTarget(premiseStatement: Statement, targetStatement: Statement): Option[(Step, Step.Target)] = {
    extractFromPremise(premiseStatement, extractStatementWithTarget(targetStatement, _, _))
  }
  def extractFromFactWithTarget(fact: Inference, targetStatement: Statement): Option[(Step, Step.Target)] = {
    extractFromFact(fact, extractStatementWithTarget(targetStatement, _, _))
  }

  private def matchFinalWithHelper(helperPremiseStatement: Statement, extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
    statementExtractionInferences.iterator.findFirst {
      case (inference, firstPremise, otherPremises) =>
        (for {
          otherPremise <- otherPremises.single
          extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
          extractedOtherPremise <- otherPremise.applySubstitutions(extractionSubstitutions)
          terms <- extractedOtherPremise.calculateArguments(helperPremiseStatement, Map.empty)
          substitutedFirstPremise <- extractionCandidate.specify(terms)
          substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise).flatMap(_.confirmTotality)
          substitutedOtherPremise <- otherPremise.applySubstitutions(substitutions)
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
          assertionStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            Seq(substitutedFirstPremise, substitutedOtherPremise).map(Premise.Pending),
            substitutions)
        } yield (terms, Seq(assertionStep)))
    }
  }

  def extractFromFactWithPremise(fact: Inference, helperPremiseStatement: Statement): Option[Step] = {
    for {
      (_, steps, _) <- extractRecursively(
        fact.conclusion,
        matchFinalWithHelper(helperPremiseStatement, _, _).map { case (m, ss) => (m, ss, ()) }.iterator
      ).headOption
      assertionStep = Step.Assertion(fact.conclusion, fact.summary, Nil, Substitutions.empty)
      finalStep <- Step.Elided.ifNecessary(assertionStep +: steps, fact)
    } yield finalStep
  }

  def extractFromBasePremiseWithPremise(basePremiseStatement: Statement, helperPremiseStatement: Statement): Option[Step] = {
    for {
      (_, steps, _) <- extractRecursively(
        basePremiseStatement,
        matchFinalWithHelper(helperPremiseStatement, _, _).map { case (m, ss) => (m, ss, ()) }.iterator
      ).headOption
      finalStep <- Step.Elided.ifNecessary(steps, "Extracted")
    } yield finalStep
  }
}
