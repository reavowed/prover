package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

class SubstatementExtractor(implicit stepProvingContext: StepProvingContext) {
  import stepProvingContext._
  import provingContext._

  private type Result[T] = Seq[(Map[Int, Term], Seq[Step], T)]

  private def extractStatement[T](extractionCandidate: Statement, termsSoFar: Int, recurse: (Statement, Int) => Result[T]): Result[T] = {
    statementExtractionInferences.flatMap {
      case (inference, firstPremise, otherPremises) =>
        for {
          extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).toSeq
          extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
          (conclusionTerms, innerSteps, t) <- recurse(extractedConclusion, termsSoFar)
          extractedOtherPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions)).traverseOption.toSeq
          (premiseSteps, terms) <- PremiseFinder.findParameterisedPremiseSteps(extractedOtherPremises, conclusionTerms)
          substitutedFirstPremise <- extractionCandidate.specify(terms).toSeq
          substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise).flatMap(_.confirmTotality).toSeq
          substitutedOtherPremises <- otherPremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
          assertionStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            (substitutedFirstPremise +: substitutedOtherPremises).map(Premise.Pending),
            substitutions)
          newStep <- Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference.summary)
        } yield (terms, newStep +: innerSteps, t)
    }
  }

  private def extractPredicate[T](extractionCandidate: Statement, termsSoFar: Int, recurse: (Statement, Int) => Result[T]): Result[T] = {
    predicateSpecificationInferences.flatMap {
      case (inference, singlePremise, predicateName, argumentNames) =>
        for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1).flatMap(_.confirmTotality).toSeq
          extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
          nextPremise <- extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1).toSeq
          (terms, laterSteps, t) <- recurse(nextPremise, termsSoFar + argumentNames.length).toSeq
          specifiedPremise <- extractionCandidate.specify(terms, 0, stepContext.externalDepth).toSeq
          substitutionsWithTerms <- singlePremise.calculateSubstitutions(specifiedPremise).flatMap(_.confirmTotality).toSeq
            .map(_.copy(terms = argumentNames.mapWithIndex((n, i) => n -> terms(termsSoFar + i)).toMap))
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutionsWithTerms).toSeq
          specifiedConclusion <- substitutedConclusion.specify(terms, 0, stepContext.externalDepth)
          newStep = Step.Assertion(
            specifiedConclusion,
            inference.summary,
            Seq(Premise.Pending(specifiedPremise)),
            substitutionsWithTerms)
        } yield (terms, newStep +: laterSteps, t)
    }
  }

  private def extractRecursively[T](
    extractionCandidate: Statement,
    termsSoFar: Int,
    matchAtEnd: (Statement, Int) => Result[T]
  ): Result[T] = {
    def extractWithoutRewrite(s: Statement, i: Int): Result[T] =
      matchAtEnd(s, i) ++
        extractStatement(s, i, recurse) ++
        extractPredicate(s, i, recurse)
    def recurse(s: Statement, i: Int): Result[T] = extractWithRewrite(s, i, extractWithoutRewrite)
    recurse(extractionCandidate, termsSoFar)
  }

  private def extractWithRewrite[T](extractionCandidate: Statement, termsSoFar: Int, extractWithoutRewrite: (Statement, Int) => Result[T]): Result[T] = {
    extractWithoutRewrite(extractionCandidate, termsSoFar) ++ rewriteInferences.flatMap { case (inference, singlePremise) =>
      for {
        extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).toSeq
        rewrittenStatement <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
        (terms, innerSteps, target) <- extractWithoutRewrite(rewrittenStatement, termsSoFar)
        substitutedPremise <- extractionCandidate.specify(terms, 0, stepContext.externalDepth).toSeq
        substitutions <- singlePremise.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality).toSeq
        substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
        newStep = Step.Assertion(
          substitutedConclusion,
          inference.summary,
          Seq(Premise.Pending(substitutedPremise)),
          substitutions)
      } yield (terms, newStep +: innerSteps, target)
    }
  }

  private def extractFromStatement[T](extractionCandidate: Statement, matchAtEnd: (Statement, Int) => Result[T]): Option[(Seq[Step], T)] = {
    for {
      (_, steps, t) <- extractRecursively(
        extractionCandidate,
        0,
        matchAtEnd).headOption
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

  def extractDirectly(targetStatement: Statement, f: ((Statement, Int) => Result[Unit]) => Option[(Step, Unit)]): Option[Step] = {
    f(matchDirectly(targetStatement, _, _).map((_, Nil, ())).toSeq).map(_._1)
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
    statementExtractionInferences.mapCollect {
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
        0,
        matchFinalWithHelper(helperPremiseStatement, _, _).map { case (m, ss) => (m, ss, ()) }.toSeq).headOption
      assertionStep = Step.Assertion(fact.conclusion, fact.summary, Nil, Substitutions.empty)
      finalStep <- Step.Elided.ifNecessary(assertionStep +: steps, fact)
    } yield finalStep
  }

  def extractFromBasePremiseWithPremise(basePremiseStatement: Statement, helperPremiseStatement: Statement): Option[Step] = {
    for {
      (_, steps, _) <- extractRecursively(
        basePremiseStatement,
        0,
        matchFinalWithHelper(helperPremiseStatement, _, _).map { case (m, ss) => (m, ss, ()) }.toSeq).headOption
      finalStep <- Step.Elided.ifNecessary(steps, "Extracted")
    } yield finalStep
  }
}
