package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {
  def extract(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Step, Option[Step.Target])] = {
    import stepProvingContext._
    import provingContext._

    def matchDirectly(extractionCandidate: Statement, termsSoFar: Int): Option[Map[Int, Term]] = {
      extractionCandidate.calculateArguments(targetStatement, Map.empty)
    }

    def extractStatement(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      statementExtractionInferences.iterator.findFirst {
        case (inference, firstPremise, otherPremises) =>
          (for {
            extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).toSeq
            extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
            (conclusionTerms, innerSteps, target) <- extractFromStatement(extractedConclusion, termsSoFar).toSeq
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
          } yield (terms, newStep +: innerSteps, target)).headOption
      }
    }
    def extractStatementWithTarget(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      statementExtractionInferences.iterator.findFirst {
        case (inference, firstPremise, otherPremises) =>
          (for {
            otherPremise <- otherPremises.single
            extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
            extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
            terms <- matchDirectly(extractedConclusion, termsSoFar)
            substitutedFirstPremise = extractionCandidate.specify(terms)
            substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise).flatMap(_.confirmTotality)
            substitutedOtherPremise <- otherPremise.applySubstitutions(substitutions)
            substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
            assertionStep = Step.Assertion(
              substitutedConclusion,
              inference.summary,
              Seq(substitutedFirstPremise, substitutedOtherPremise).map(Premise.Pending),
              substitutions)
          } yield (terms, Seq(assertionStep), Some(Step.Target(substitutedOtherPremise)))).headOption
      }
    }
    def extractPredicate(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      predicateSpecificationInferences.iterator.findFirst {
        case (inference, singlePremise, predicateName, argumentNames) =>
          (for {
            extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1).flatMap(_.confirmTotality).toSeq
            extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
            nextPremise = extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1)
            (terms, laterSteps, target) <- extractFromStatement(nextPremise, termsSoFar + argumentNames.length).toSeq
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
          } yield (terms, newStep +: laterSteps, target)).headOption
      }
    }

    def extractRecursively(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      extractStatement(extractionCandidate, termsSoFar) orElse
        extractPredicate(extractionCandidate, termsSoFar) orElse
        extractStatementWithTarget(extractionCandidate, termsSoFar)
    }

    def extractWithoutRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      matchDirectly(extractionCandidate, termsSoFar).map((_, Nil, None)) orElse
        extractRecursively(extractionCandidate, termsSoFar)
    }

    def extractWithRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      rewriteInferences.iterator.findFirst { case (inference, singlePremise) =>
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

    def extractFromStatement(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step], Option[Step.Target])] = {
      extractWithoutRewrite(extractionCandidate, termsSoFar) orElse
        extractWithRewrite(extractionCandidate, termsSoFar)
    }

    def extractPremise(premise: Premise.SingleLinePremise): Option[(Step, Option[Step.Target])] = {
      for {
        (_, steps, target) <- extractFromStatement(premise.statement, 0)
        finalStep <- Step.Elided.ifNecessary(steps, "Extracted")
      } yield (finalStep, target)
    }

    allPremisesSimplestFirst.mapFind(extractPremise)
  }
}
