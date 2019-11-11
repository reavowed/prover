package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {

  import ProofHelper.WrapElided._

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
            extractionSubstitutions <- firstPremise.calculateSubstitutions(extractionCandidate, stepContext).flatMap(_.confirmTotality).toSeq
            extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions, stepContext).toSeq
            (conclusionTerms, innerSteps) <- extractFromStatement(extractedConclusion, termsSoFar).toSeq
            extractedOtherPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions, stepContext)).traverseOption.toSeq
            (premiseSteps, terms) <- PremiseFinder.findParameterisedPremiseSteps(extractedOtherPremises, conclusionTerms, stepContext)
            substitutedFirstPremise = extractionCandidate.specify(terms, 0, stepContext.externalDepth)
            substitutions <- firstPremise.calculateSubstitutions(substitutedFirstPremise, stepContext).flatMap(_.confirmTotality).toSeq
            substitutedOtherPremises <- otherPremises.map(_.applySubstitutions(substitutions, stepContext)).traverseOption.toSeq
            substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions, stepContext)
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
            extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, Substitutions.empty, 0, stepContext.externalDepth + 1).flatMap(_.confirmTotality).toSeq
            extractionPredicate <- extractionSubstitutions.predicates.get((predicateName, argumentNames.length)).toSeq
            nextPremise = extractionPredicate.specify(argumentNames.mapWithIndex((_, index) => FunctionParameter(termsSoFar + index, stepContext.externalDepth)), 0, stepContext.externalDepth + 1)
            (terms, laterSteps) <- extractFromStatement(nextPremise, termsSoFar + argumentNames.length).toSeq
            specifiedPremise = extractionCandidate.specify(terms, 0, stepContext.externalDepth)
            substitutionsWithTerms <- singlePremise.calculateSubstitutions(specifiedPremise, stepContext).flatMap(_.confirmTotality).toSeq
              .map(_.copy(terms = argumentNames.mapWithIndex((n, i) => n -> terms(termsSoFar + i)).toMap))
            substitutedConclusion <- inference.conclusion.applySubstitutions(substitutionsWithTerms, stepContext).toSeq
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
        (for {
          extractionSubstitutions <- singlePremise.calculateSubstitutions(extractionCandidate, stepContext).flatMap(_.confirmTotality)
          rewrittenStatement <- inference.conclusion.applySubstitutions(extractionSubstitutions, stepContext)
          (terms, innerSteps) <- extractWithoutRewrite(rewrittenStatement, termsSoFar)
          substitutedPremise = extractionCandidate.specify(terms, 0, stepContext.externalDepth)
          substitutions <- singlePremise.calculateSubstitutions(substitutedPremise, stepContext).flatMap(_.confirmTotality)
          substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions, stepContext)
          newStep = Step.Assertion(
            substitutedConclusion,
            inference.summary,
            Seq(Premise.Pending(substitutedPremise)),
            substitutions)
        } yield (terms, newStep +: innerSteps))
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
