package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model.components.{BoundVariable, DefinedStatement, Statement}
import net.prover.model.entries.StatementDefinition
import net.prover.model._

import scala.util.Try

case class Prover(
  assertion: Statement,
  reference: Reference.Direct,
  context: ProvingContext,
  debug: Boolean)
{
  import context._

  case class Transformation(statementDefinition: StatementDefinition, variableName: String) {
    val boundVariable = BoundVariable(0)(variableName)
    def apply(statement: Statement): Option[DefinedStatement] = {
      statement.makeApplicative(boundVariable).map(s => DefinedStatement(Seq(s), statementDefinition)(statementDefinition.boundVariableNames))
    }
  }

  val applicableHints = assertionHints.filter(_.conclusion == assertion)
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
    availableInferences.iterator.findFirst(i => proveUsingInference(i) orElse proveUsingTransformedInference(i))
  }

  private def proveUsingInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Option[Step.Assertion] = {
    initialSubstitutions.map(Iterator(_))
      .getOrElse {
        inference.conclusion.calculateSubstitutions(assertion, Substitutions.empty, 0).iterator
      }
      .flatMap { substitutions =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement)
      }
      .map { case (premiseReferences, substitutions) =>
        Step.Assertion(assertion, InferenceApplication.Direct(inference, substitutions, premiseReferences), reference, isRearrangement = false)
      }
      .headOption
  }

  private def proveUsingTransformedInference(
    inference: Inference
  ): Option[Step.Assertion] = {
    val iterator = for {
      transformation <- transformations.iterator
      premiseStatements <- inference.premises.map(_.fact.asOptionalInstanceOf[Fact.Direct].map(_.statement)).traverseOption.iterator
      transformedConclusion <- transformation(inference.conclusion).iterator
      transformedPremiseStatements <- premiseStatements.map(transformation.apply).traverseOption.iterator
      transformedPremises = transformedPremiseStatements.zipWithIndex.map { case (s, i) => Premise(Fact.Direct(s), i)(isElidable = false) }
      conclusionSubstitutions <- transformedConclusion.calculateSubstitutions(assertion, Substitutions.empty, 0)
      (premiseReferences, premiseSubstitutions) <- matchPremisesToFacts(transformedPremises, conclusionSubstitutions, inference.allowsRearrangement)
      transformationProofAttempt = Try(Proof.fillInOutline(
        transformedPremises,
        ProofOutline(Seq(
          StepOutline.ScopedVariable(
            transformation.variableName,
            (transformedPremiseStatements :+ transformedConclusion)
              .map(s => StepOutline.Assertion(s.subcomponents.head.asInstanceOf[Statement], None))),
          StepOutline.Assertion(transformedConclusion, None))),
        availableInferences,
        assertionHints,
        Nil))
      transformationProof <- transformationProofAttempt.toOption
    } yield Step.Assertion(
      assertion,
      InferenceApplication.Transformed(
        inference,
        premiseSubstitutions,
        premiseReferences,
        transformation.statementDefinition,
        transformedPremises,
        transformedConclusion,
        transformationProof.steps),
      reference,
      isRearrangement = false)
    iterator.headOption
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
          .map(_.map(_.statement))
          .map((inference, _))
      }
    def findStatementByExpanding(statement: Statement): Option[InferenceApplication] = {
      expansions.iterator
        .findFirst { case (inference, inferencePremises) =>
          (
            for {
              substitutions <- inference.conclusion.calculateSubstitutions(statement, Substitutions.empty, 0)
              substitutedPremises <- inferencePremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
              premiseReferences <- substitutedPremises.map(getStatementByRearranging).traverseOption.toSeq
              if inference.conclusion.applySubstitutions(substitutions).contains(statement)
            } yield InferenceApplication.Direct(inference, substitutions, premiseReferences)
          ).headOption
        }
    }
    def getStatementByRearranging(statement: Statement): Option[Reference] = {
      findStatementInFacts(statement) orElse findStatementByExpanding(statement).map(Reference.Expansion)
    }
    findStatementByExpanding(assertion).map { inferenceApplication =>
      Step.Assertion(assertion, inferenceApplication, reference, isRearrangement = true)
    }
  }

  def findStatementInFacts(statement: Statement): Option[Reference] = {
    allSimplifiedFacts.find(_.fact == Fact.Direct(statement)).map(_.reference)
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
    helper(getNextLevelSimplifications(referencedFact), Nil)
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
            premiseStatement.findSubcomponent(inference.conclusion)
              .map((inference, premiseStatement, _))
          }
          .flatMap { case (inference, premiseStatement, simplificationPath) =>
            premiseStatement.calculateSubstitutions(statement, Substitutions.empty, 0)
              .map((inference, simplificationPath, _))
          }
          .mapCollect { case (inference, simplificationPath, substitutions) =>
            inference.conclusion.applySubstitutions(substitutions)
              .map((inference, simplificationPath, substitutions, _))
          }
          .map { case (inference, simplificationPath, substitutions, conclusion) =>
            ReferencedFact(
              Fact.Direct(conclusion),
              Reference.Simplification(inference, substitutions, factReference, simplificationPath))
          }
      case _ =>
        Nil
    }
  }
}
