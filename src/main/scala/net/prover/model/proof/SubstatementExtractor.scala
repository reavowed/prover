package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {
  def extract(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[Step] = {
    import stepProvingContext._
    import provingContext._

    def matchDirectly(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      for {
        terms <- extractionCandidate.calculateArguments(targetStatement, Map.empty)
      } yield (terms, Nil)
    }

    def extractRecursively(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      statementExtractionInferences.iterator.findFirst {
        case (inference, firstPremise, otherPremises) =>
          (for {
            extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality).toSeq
            extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
            (conclusionTerms, innerSteps) <- extractFromStatement(extractedConclusion, termsSoFar).toSeq
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
          } yield (terms, newStep +: innerSteps)).headOption
      } orElse predicateSpecificationInferences.iterator.findFirst {
        case (inference, singlePremise, predicateName, argumentNames) =>
          (for {
            extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1).flatMap(_.confirmTotality).toSeq
            extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
            nextPremise = extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1)
            (terms, laterSteps) <- extractFromStatement(nextPremise, termsSoFar + argumentNames.length).toSeq
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
          } yield (terms, newStep +: laterSteps)).headOption
      }
    }

    def extractWithoutRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      matchDirectly(extractionCandidate, termsSoFar) orElse
        extractRecursively(extractionCandidate, termsSoFar)
    }

    def extractWithRewrite(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      rewriteInferences.iterator.findFirst { case (inference, singlePremise) =>
        for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate).flatMap(_.confirmTotality)
          rewrittenStatement <- inference.conclusion.applySubstitutions(extractionSubstitutions)
          (terms, innerSteps) <- extractWithoutRewrite(rewrittenStatement, termsSoFar)
          substitutedPremise = extractionCandidate.specify(terms, 0, stepContext.externalDepth)
          substitutions <- singlePremise.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality)
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
          newStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            Seq(Premise.Pending(substitutedPremise)),
            substitutions)
        } yield (terms, newStep +: innerSteps)
      }
    }

    def extractFromStatement(extractionCandidate: Statement, termsSoFar: Int): Option[(Map[Int, Term], Seq[Step])] = {
      extractWithoutRewrite(extractionCandidate, termsSoFar) orElse
        extractWithRewrite(extractionCandidate, termsSoFar)
    }

    def extractPremise(premise: Premise.SingleLinePremise): Option[Step] = {
      extractFromStatement(premise.statement, 0).map(_._2).flatMap(Step.Elided.ifNecessary(_, "Extracted"))
    }

    allPremisesSimplestFirst.mapFind(extractPremise)
  }
}
