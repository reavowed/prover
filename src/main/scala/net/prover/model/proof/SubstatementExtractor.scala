package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

class SubstatementExtractor(implicit stepProvingContext: StepProvingContext) {
  import stepProvingContext._
  import provingContext._

  private type Result[T] = Option[(Map[Int, Term], Seq[Step], T)]

  private def extractStatement[T](extractionCandidate: Statement, termsSoFar: Int, recurse: (Statement, Int) => Result[T]): Result[T] = {
    statementExtractionInferences.iterator.findFirst {
      case (inference, firstPremise, otherPremises) =>
        (for {
          extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).toSeq
          extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
          (conclusionTerms, innerSteps, t) <- recurse(extractedConclusion, termsSoFar).toSeq
          extractedOtherPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions)).traverseOption.toSeq
          (premiseSteps, terms) <- PremiseFinder.findParameterisedPremiseSteps(extractedOtherPremises, conclusionTerms)
          substitutedFirstPremise = extractionCandidate.specify(terms)
          substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise).flatMap(_.confirmTotality).toSeq
          substitutedOtherPremises <- otherPremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
          assertionStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            (substitutedFirstPremise +: substitutedOtherPremises).map(Premise.Pending),
            substitutions)
          newStep <- Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference.summary)
        } yield (terms, newStep +: innerSteps, t)).headOption
    }
  }

  private def extractPredicate[T](extractionCandidate: Statement, termsSoFar: Int, recurse: (Statement, Int) => Result[T]): Result[T] = {
    predicateSpecificationInferences.iterator.findFirst {
      case (inference, singlePremise, predicateName, argumentNames) =>
        (for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1).flatMap(_.confirmTotality).toSeq
          extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
          nextPremise = extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1)
          (terms, laterSteps, t) <- recurse(nextPremise, termsSoFar + argumentNames.length).toSeq
          specifiedPremise = extractionCandidate.specify(terms, 0, stepContext.externalDepth)
          substitutionsWithTerms <- singlePremise.calculateSubstitutions(specifiedPremise).flatMap(_.confirmTotality).toSeq
            .map(_.copy(terms = argumentNames.mapWithIndex((n, i) => n -> terms(termsSoFar + i)).toMap))
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutionsWithTerms).toSeq
          specifiedConclusion = substitutedConclusion.specify(terms, 0, stepContext.externalDepth)
          newStep = Step.Assertion(
            specifiedConclusion,
            inference.summary,
            Seq(Premise.Pending(specifiedPremise)),
            substitutionsWithTerms)
        } yield (terms, newStep +: laterSteps, t)).headOption
    }
  }

  private def extractRecursively[T](
    extractionCandidate: Statement,
    termsSoFar: Int,
    matchAtEnd: (Statement, Int) => Result[T]
  ): Result[T] = {
    def extractWithoutRewrite(s: Statement, i: Int): Result[T] =
      matchAtEnd(s, i) orElse
        extractStatement(s, i, recurse) orElse
        extractPredicate(s, i, recurse)
    def recurse(s: Statement, i: Int): Result[T] = extractWithRewrite(s, i, extractWithoutRewrite)
    recurse(extractionCandidate, termsSoFar)
  }

  private def extractWithRewrite[T](extractionCandidate: Statement, termsSoFar: Int, extractWithoutRewrite: (Statement, Int) => Result[T]): Result[T] = {
    extractWithoutRewrite(extractionCandidate, termsSoFar) orElse rewriteInferences.iterator.findFirst { case (inference, singlePremise) =>
      for {
        extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
        rewrittenStatement <- inference.conclusion.applySubstitutions(extractionSubstitutions)
        (terms, innerSteps, target) <- extractWithoutRewrite(rewrittenStatement, termsSoFar)
        substitutedPremise = extractionCandidate.specify(terms, 0, stepContext.externalDepth)
        substitutions <- singlePremise.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality)
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
        matchAtEnd)
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

  def extract(targetStatement: Statement): Option[Step] = {
    extractFromPremisesOrFact(targetStatement, matchDirectly(targetStatement, _, _).map((_, Nil, ()))).map(_._1)
  }

  private def extractStatementWithTarget(targetStatement: Statement, extractionCandidate: Statement, termsSoFar: Int): Result[Step.Target] = {
    statementExtractionInferences.iterator.findFirst {
      case (inference, firstPremise, otherPremises) =>
        (for {
          otherPremise <- otherPremises.single
          extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
          extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
          terms <- matchDirectly(targetStatement, extractedConclusion, termsSoFar)
          substitutedFirstPremise = extractionCandidate.specify(terms)
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

  def extractFromPremise(premiseStatement: Statement, targetStatement: Statement): Option[(Step, Step.Target)] = {
    extractFromPremise(premiseStatement, extractStatementWithTarget(targetStatement, _, _))
  }
  def extractFromFact(fact: Inference, targetStatement: Statement): Option[(Step, Step.Target)] = {
    extractFromFact(fact, extractStatementWithTarget(targetStatement, _, _))
  }

  def extractFromFactWithPremise(fact: Inference, finalPremiseStatement: Statement): Option[Step] = {
    def matchFinal(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      statementExtractionInferences.iterator.findFirst {
        case (inference, firstPremise, otherPremises) =>
          (for {
            otherPremise <- otherPremises.single
            extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
            extractedOtherPremise <- otherPremise.applySubstitutions(extractionSubstitutions)
            terms <- extractedOtherPremise.calculateArguments(finalPremiseStatement, Map.empty)
            substitutedFirstPremise = extractionCandidate.specify(terms)
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
    for {
      (_, steps, _) <- extractRecursively(
        fact.conclusion,
        0,
        matchFinal(_, _).map { case (m, ss) => (m, ss, ()) })
      assertionStep = Step.Assertion(fact.conclusion, fact.summary, Nil, Substitutions.empty)
      finalStep <- Step.Elided.ifNecessary(assertionStep +: steps, fact)
    } yield finalStep
  }
}
