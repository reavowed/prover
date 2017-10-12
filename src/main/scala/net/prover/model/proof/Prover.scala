package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, FunctionParameter, Statement, TermVariable}

import scala.util.Try

case class Prover(
  assertionToProve: Statement,
  reference: Reference.Direct,
  context: ProvingContext,
  debug: Boolean)
{
  import context._

  case class Transformation(statementDefinition: StatementDefinition, variableName: String) {
    private def transform(statement: Statement): Option[Statement] = {
      statement.makeApplicative(statementDefinition.boundVariableNames)
    }

    private def toFull(statement: Statement): Statement = {
      DefinedStatement(Seq(statement), statementDefinition, statement.depth - 1)(statementDefinition.boundVariableNames)
    }

    private def toSpecified(statement: Statement): Statement = {
      statement.specify(Seq(TermVariable("_", depth)))
    }

    private def toBound(statement: Statement): Statement = {
      statement.increaseDepth(1).specify(Seq(FunctionParameter(variableName, 0)))
    }

    private def transformAll(applicablePremiseStatements: Seq[Statement], applicableConclusion: Statement) = {
      def transformNext(s: Statement) = {
        Seq((toFull(s), Some(s)), (toSpecified(s), None))
      }
      val premisesAndStatementsToProve = applicablePremiseStatements.zipWithIndex
        .foldLeft(Seq((Seq.empty[Premise], Seq.empty[Statement]))) { case (acc, (premise, index)) =>
          for {
            (premises, toProve) <- acc
            (nextPremiseStatement, nextToProve) <- transformNext(premise)
            nextPremise = Premise(Fact.Direct(nextPremiseStatement), index)(isElidable = false)
          } yield (premises :+ nextPremise, toProve ++ nextToProve)
        }
      premisesAndStatementsToProve.headOption.map { case (p, toProve) =>
        val boundSubsteps = (toProve :+ applicableConclusion).map(toBound)
        val steps =
          if (boundSubsteps.nonEmpty)
            Seq(StepOutline.ScopedVariable(variableName, boundSubsteps.map(s => StepOutline.Assertion(s, None))))
          else
            Nil
        (p, toFull(applicableConclusion), steps)
      } ++ premisesAndStatementsToProve.drop(1).map { case (p, toProve) =>
        (p, toSpecified(applicableConclusion), toProve.map(s => StepOutline.Assertion(toSpecified(s), None)))
      }
    }

    def applyToInference(
      premiseStatements: Seq[Statement],
      conclusion: Statement
    ): Seq[(Seq[Premise], Statement, Seq[StepOutline])] = {
      for {
        applicableConclusion <- transform(conclusion).toSeq
        applicablePremiseStatements <- premiseStatements.map(transform).traverseOption.toSeq
        (premises, conclusion, stepsToProve) <- transformAll(applicablePremiseStatements, applicableConclusion)
      } yield (premises, conclusion, stepsToProve)
    }
  }

  val applicableHints = assertionHints.filter(_.conclusion == assertionToProve)
  lazy val allSimplifiedFacts = referencedFacts ++ referencedFacts.flatMap(getAllSimplifications)
  lazy val transformations: Seq[Transformation] = transformationStatementDefinitions.mapCollect { statementDefinition =>
    for {
      variableName <- statementDefinition.boundVariableNames.single
    } yield {
      Transformation(statementDefinition, variableName)
    }
  }

  def proveAssertion(): Option[Step.Assertion] = {
    proveAssertionUsingHints()
      .orElse(proveAssertionDirectlyFromInferences())
      .orElse(proveAssertionByRearranging())
  }

  def proveAssertionUsingHints(): Option[Step.Assertion] = {
    applicableHints.iterator.findFirst(h => proveUsingInference(h.inference, Some(h.substitutions)))
  }

  def proveAssertionDirectlyFromInferences(): Option[Step.Assertion] = {
    availableInferences.iterator.findFirst(proveUsingInference(_)) orElse
      availableInferences.iterator.findFirst(proveUsingTransformedInference)
  }

  private def proveUsingInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Option[Step.Assertion] = {
    initialSubstitutions.map(Iterator(_))
      .getOrElse {
        inference.conclusion.calculateSubstitutions(assertionToProve, Substitutions.emptyWithDepth(depth)).iterator
      }
      .flatMap { substitutions =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement)
      }
      .map { case (premiseReferences, substitutions) =>
        Step.Assertion(assertionToProve, InferenceApplication.Direct(inference, substitutions, premiseReferences, depth), reference, isRearrangement = false)
      }
      .headOption
  }

  private def proveUsingTransformedInference(
    inference: Inference
  ): Option[Step.Assertion] = {
    (for {
      transformation <- transformations.iterator
      premiseStatements <- inference.premises.map(_.fact.asOptionalInstanceOf[Fact.Direct].map(_.assertion)).traverseOption.iterator
      (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyToInference(premiseStatements, inference.conclusion)
      conclusionSubstitutions <- transformedConclusion.calculateSubstitutions(assertionToProve, Substitutions.emptyWithDepth(depth))
      (premiseReferences, premiseSubstitutions) <- matchPremisesToFacts(transformedPremises, conclusionSubstitutions, inference.allowsRearrangement)
      proofOutline = ProofOutline(stepsToProve :+ StepOutline.Assertion(transformedConclusion, None))
      transformationProofAttempt = Try(Proof.fillInOutline(
        transformedPremises,
        proofOutline,
        availableInferences,
        assertionHints,
        Nil))
      transformationProof <- transformationProofAttempt.toOption
    } yield Step.Assertion(
      assertionToProve,
      InferenceApplication.Transformed(
        inference,
        premiseSubstitutions,
        premiseReferences,
        transformation.statementDefinition,
        transformedPremises,
        transformedConclusion,
        transformationProof.steps,
        depth),
      reference,
      isRearrangement = false)
    ).headOption
  }

//  private def proveUsingElidedInference(
//    inference: Inference,
//    initialSubstitutions: Option[Substitutions] = None
//  ): Iterator[Step.Assertion] = {
//    splitPremisesAtElidable(inference.premises).iterator
//      .flatMap { case (prePremises, elidablePremise, postPremises) =>
//        initialSubstitutions.map(Iterator(_))
//          .getOrElse(inference.conclusion.calculateSubstitutions(assertion, Substitutions.empty).iterator)
//          .map {(prePremises, elidablePremise, postPremises, _)}
//      }
//      .flatMap { case (prePremises, elidablePremise, postPremises, substitutionsAfterConclusion) =>
//        matchPremisesToFacts(prePremises, substitutionsAfterConclusion, inference.allowsRearrangement)
//          .map { case (prePremiseMatches, substitutionsAfterPrePremises) =>
//            (prePremiseMatches, elidablePremise, postPremises, substitutionsAfterPrePremises)
//          }
//      }
//      .flatMap { case (prePremiseReferences, elidablePremise, postPremises, substitutionsAfterPrePremises) =>
//        matchPremisesToFacts(postPremises, substitutionsAfterPrePremises, inference.allowsRearrangement)
//          .map { case (postPremiseReferences, substitutionsAfterPostPremises) =>
//            (prePremiseReferences, elidablePremise, postPremiseReferences, substitutionsAfterPostPremises)
//          }
//      }
//      .flatMap { case (prePremiseReferences, elidablePremise, postPremiseReferences, substitutionsAfterPostPremises) =>
//        matchElidablePremise(elidablePremise, substitutionsAfterPostPremises)
//          .map { case (elidedPremiseReference, substitutionsAfterElidedPremise) =>
//            ((prePremiseReferences :+ elidedPremiseReference) ++ postPremiseReferences, substitutionsAfterElidedPremise)
//          }
//      }
//      .mapCollect { case (premiseReferences, substitutions) =>
//        makeAssertion(inference, substitutions, premiseReferences)
//      }
//  }

//  private def matchElidablePremise(
//    premise: Statement,
//    premiseSubstitutionsSoFar: Substitutions
//  ): Iterator[(Reference, Substitutions)] = {
//    availableInferences.iterator
//      // Match the premises first, since we don't know what the conclusion should look like
//      .flatMap { inference =>
//        matchPremisesToFacts(inference.premises, Substitutions.empty, inference.allowsRearrangement)
//          .map(inference -> _)
//      }
//      // Work out the substitutions by condensing the conclusion with the premise
//      .mapCollect { case (inference, (premiseReferences, inferenceSubstitutions)) =>
//        premise.condense(inference.conclusion, premiseSubstitutionsSoFar, inferenceSubstitutions)
//          .map((inference, premiseReferences, _))
//      }
//      // Confirm the final conclusion of the inference
//      .mapCollect { case (inference, premiseReferences, (premiseSubstitutions, inferenceSubstitutions)) =>
//        inference.conclusion.applySubstitutions(inferenceSubstitutions)
//          .map((inference, premiseReferences, premiseSubstitutions, inferenceSubstitutions, _))
//      }
//      .mapCollect { case (inference, premiseReferences, premiseSubstitutions, inferenceSubstitutions, provenConclusion) =>
//        inference.specifySubstitutions(inferenceSubstitutions)
//          .map((inference, premiseReferences, premiseSubstitutions, _, provenConclusion))
//      }
//      // And finally match the premise to our computed conclusion
//      .flatMap { case (inference, premiseReferences, premiseSubstitutions, inferenceSubstitutions, provenConclusion) =>
//        premise.calculateSubstitutions(provenConclusion, premiseSubstitutions).iterator
//          .map { substitutions =>
//            Reference.Elided(InferenceApplication(inference.summary, inferenceSubstitutions, premiseReferences)) -> substitutions
//          }
//      }
//  }
//
//  private def splitPremisesAtElidable(premises: Seq[Premise]): Option[(Seq[Premise], Statement, Seq[Premise])] = {
//    val (prePremises, elidableAndPostPremises) = premises.span {
//      case premise if premise.isElidable =>
//        false
//      case _ =>
//        true
//    }
//    elidableAndPostPremises match {
//      case Premise(Fact.Direct(premiseStatement), _) +: postPremises =>
//        Some((prePremises, premiseStatement, postPremises))
//      case _ =>
//        None
//    }
//  }

  def proveAssertionByRearranging(): Option[Step.Assertion] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .mapCollect { inference =>
        inference.premises.map(_.fact).toType[Fact.Direct]
          .map(_.map(_.assertion))
          .map((inference, _))
      }
    def findAssertionByExpanding(assertion: Statement): Option[InferenceApplication] = {
      expansions.iterator
        .findFirst { case (inference, inferencePremises) =>
          (
            for {
              substitutions <- inference.conclusion.calculateSubstitutions(assertion, Substitutions.emptyWithDepth(depth))
              substitutedPremises <- inferencePremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
              premiseReferences <- substitutedPremises.map(getAssertionByRearranging).traverseOption.toSeq
              if inference.conclusion.applySubstitutions(substitutions).contains(assertion)
            } yield InferenceApplication.Direct(inference, substitutions, premiseReferences, depth)
          ).headOption
        }
    }
    def getAssertionByRearranging(assertion: Statement): Option[Reference] = {
      findAssertionInFacts(assertion) orElse findAssertionByExpanding(assertion).map(Reference.Expansion)
    }
    findAssertionByExpanding(assertionToProve).map { inferenceApplication =>
      Step.Assertion(assertionToProve, inferenceApplication, reference, isRearrangement = true)
    }
  }

  def findAssertionInFacts(assertion: Statement): Option[Reference] = {
    allSimplifiedFacts.find(_.fact == Fact.Direct(assertion)).map(_.reference)
  }

  private def matchPremisesToFacts(
    premises: Seq[Premise],
    substitutions: Substitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(Seq[Reference], Substitutions)] = {
    val initial = Iterator((Seq.empty[Reference], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (referencesSoFar, substitutionsSoFar) =>
        matchPremiseToFacts(premise, substitutionsSoFar, rearrangementAllowed).map { case (premiseReference, newSubstitutions) =>
          (referencesSoFar :+ premiseReference, newSubstitutions)
        }
      }
    }
  }

  private def matchPremiseToFacts(
    premise: Premise,
    substitutionsSoFar: Substitutions,
    allowRearrangement: Boolean
  ): Iterator[(Reference, Substitutions)] = {
    val facts = if (allowRearrangement) allSimplifiedFacts else referencedFacts
    facts.iterator.flatMap { case ReferencedFact(fact, factReference) =>
      matchPremiseToFact(premise.fact, fact, factReference, substitutionsSoFar)
    }
  }

  private def matchPremiseToFact(
    premiseFact: Fact,
    knownFact: Fact,
    factReference: Reference,
    substitutionsSoFar: Substitutions
  ): Iterator[(Reference, Substitutions)] = {
    premiseFact.calculateSubstitutions(knownFact, substitutionsSoFar)
      .toIterator
      .map { newSubstitutions =>
        (factReference, newSubstitutions)
      }
  }

  private def getAllSimplifications(referencedFact: ReferencedFact): Seq[ReferencedFact] = {
    def helper(next: Seq[ReferencedFact], acc: Seq[ReferencedFact]): Seq[ReferencedFact] = {
      if (next.isEmpty)
        acc
      else {
        val newSimplifications = next.flatMap(getNextLevelSimplifications)
        helper(newSimplifications, next ++ acc)
      }
    }
    helper(referencedFact +: getContractions(referencedFact), Nil)
  }

  private def getContractions(referencedFact: ReferencedFact, level: Int = 0, additionalDepth: Int = 0): Seq[ReferencedFact] = {
    def nextLevelContractions =
      for {
        (childFact, moreDepth, updater) <- referencedFact.childDetails.toSeq
        innerFact <- getContractions(childFact, level + 1, additionalDepth + moreDepth)
      } yield updater(innerFact)
    (referencedFact +: nextLevelContractions).flatMap(getTopLevelContractions(_, level, additionalDepth))
  }

  def getTopLevelContractions(referencedFact: ReferencedFact, level: Int, additionalDepth: Int): Seq[ReferencedFact] = {
    availableInferences
      .filter(_.rearrangementType == RearrangementType.Contraction)
      .collect {
        case inference @ Inference(_, Seq(Premise(premiseFact, _)), _) =>
          (inference, premiseFact)
      }
      .flatMap { case (inference, premiseFact) =>
        premiseFact.calculateSubstitutions(referencedFact.fact, Substitutions.emptyWithDepth(depth + additionalDepth))
          .map((inference, _))
      }
      .mapCollect { case (inference, substitutions) =>
        inference.conclusion.applySubstitutions(substitutions)
          .map((inference, substitutions, _))
      }
      .map { case (inference, substitutions, conclusion) =>
        ReferencedFact(
          Fact.Direct(conclusion),
          Reference.Contraction(inference, substitutions, referencedFact.reference, level, additionalDepth, depth + additionalDepth))
      }
  }

  private def getNextLevelSimplifications(referencedFact: ReferencedFact): Seq[ReferencedFact] = {
    referencedFact match {
      case ReferencedFact(Fact.Direct(statement), factReference) =>
        availableInferences
          .filter(_.rearrangementType == RearrangementType.Simplification)
          .collect {
            case inference @ Inference(_, Seq(Premise(Fact.Direct(premiseStatement), _)), _) =>
              (inference, premiseStatement)
          }
          .flatMap { case (inference, premiseStatement) =>
            premiseStatement.findComponentPath(inference.conclusion)
              .map((inference, premiseStatement, _))
          }
          .flatMap { case (inference, premiseStatement, simplificationPath) =>
            premiseStatement.calculateSubstitutions(statement, Substitutions.emptyWithDepth(depth))
              .map((inference, simplificationPath, _))
          }
          .mapCollect { case (inference, simplificationPath, substitutions) =>
            inference.conclusion.applySubstitutions(substitutions)
              .map((inference, simplificationPath, substitutions, _))
          }
          .map { case (inference, simplificationPath, substitutions, conclusion) =>
            ReferencedFact(
              Fact.Direct(conclusion),
              Reference.Simplification(inference, substitutions, factReference, simplificationPath, depth))
          }
      case _ =>
        Nil
    }
  }
}
