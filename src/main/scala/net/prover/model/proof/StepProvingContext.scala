package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.StepProvingContext.KnownEquality
import net.prover.model.utils.ExpressionUtils

import scala.collection.mutable

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  private implicit val substitutionContext: SubstitutionContext = stepContext

  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  private lazy val allPremiseExtractions: Seq[KnownStatement] = {
    val given = premisesAndSimplifications.map(_._1).map(p => KnownStatement(p.statement, Nil))
    val simplified = premisesAndSimplifications.flatMap(_._2).map(p => KnownStatement(p.statement, Nil))
    val givenAndSimplified = given ++ simplified
    val extracted = for {
      premise <- allPremises
      extraction <- SubstatementExtractor.getPremiseExtractions(premise.statement)(this)
      if extraction.premises.isEmpty && !extraction.innerExtraction.derivation.exists(step => givenAndSimplified.exists(_.statement == step.statement))
    } yield KnownStatement(extraction.conclusion, extraction.innerExtraction.derivation)
    (givenAndSimplified ++ extracted).deduplicate
  }

  private def simplifyAll(
    previous: Seq[KnownStatement],
    current: Seq[KnownStatement],
    simplifiersByRelation: Map[BinaryRelation, Seq[PremiseSimplificationInference]]
  ): Seq[KnownStatement] = {
    if (current.isEmpty)
      previous
    else {
      val existing = previous ++ current
      val newSimplifications = current.flatMap { currentKnownStatement => {
        val simplifiers = provingContext.findRelation(currentKnownStatement.statement).flatMap(r => simplifiersByRelation.get(r.relation)).getOrElse(Nil)
        simplifiers.mapCollect { simplifier =>
          simplifier.getPremiseSimplification(currentKnownStatement, existing)(this)
            .filter { newKnownStatement => !existing.exists(_.statement == newKnownStatement.statement)}
        }
      }}
      simplifyAll(existing, newSimplifications, simplifiersByRelation)
    }
  }

  private lazy val allPremisesAfterSimplifications = simplifyAll(Nil, allPremiseExtractions, provingContext.premiseRelationSimplificationInferences)
  private lazy val allPremisesAfterRewrites = simplifyAll(Nil, allPremisesAfterSimplifications, provingContext.premiseRelationRewriteInferences)

  lazy val knownStatementsFromPremises: Seq[KnownStatement] = allPremisesAfterRewrites.map {
    ks => ks.copy(derivation = SubstatementExtractor.groupStepsByDefinition(ks.derivation, None)(provingContext))
  }
  lazy val knownStatementsFromPremisesBySerializedStatement: Map[String, KnownStatement] = {
    knownStatementsFromPremises.map(s => s.statement.serialized -> s).toMapPreservingEarliest
  }

  lazy val knownEqualities: Seq[KnownEquality] = {
    for {
      equality <- provingContext.equalityOption.toSeq
      KnownStatement(statement, derivation) <- knownStatementsFromPremises ++ provingContext.facts.map(KnownStatement.fromSingleStep)
      (lhs, rhs) <- equality.unapply(statement)
    } yield KnownEquality(lhs, rhs, equality, derivation)
  }
  lazy val knownValuesToProperties: Seq[KnownEquality] = {
    def isValue(valueTerm: Term) = ExpressionUtils.isSimpleTermVariableOrCombinationOfTermConstants(valueTerm)
    def isProperty(propertyTerm: Term) = ExpressionUtils.isWrappedSimpleTerm(propertyTerm)
    knownEqualities.filter { case KnownEquality(lhs, rhs, _, _) =>
      isValue(lhs) && isProperty(rhs)
    }
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    allPremises.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }

  def updateStepContext(f: StepContext => StepContext): StepProvingContext = {
    withStepContext(f(stepContext))
  }
  def withStepContext(newStepContext: StepContext): StepProvingContext = {
    if (newStepContext != stepContext)
      copy(stepContext = newStepContext)
    else
      this
  }

  val cachedDerivations: mutable.Map[String, Option[Seq[DerivationStep]]] = mutable.Map.empty
}

object StepProvingContext {
  case class KnownEquality(lhs: Term, rhs: Term, equality: Equality, derivation: Seq[DerivationStep])

  def updateStepContext(f: StepContext => StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.updateStepContext(f)
  }
  def withStepContext(newStepContext: StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.withStepContext(newStepContext)
  }
}
