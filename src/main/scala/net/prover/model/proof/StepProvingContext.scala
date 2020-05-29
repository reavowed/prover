package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions.{FunctionParameter, Statement, Term, TermVariable}

import scala.collection.mutable

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  private implicit val substitutionContext: SubstitutionContext = stepContext

  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  lazy val allPremiseExtractions: Seq[KnownStatement] = {
    val given = premisesAndSimplifications.map(_._1).map(p => KnownStatement(p.statement, Nil))
    val simplified = premisesAndSimplifications.flatMap(_._2).map(p => KnownStatement(p.statement, Nil))
    val extracted = for {
      premise <- premisesAndSimplifications.map(_._1)
      extraction <- SubstatementExtractor.getPremiseExtractions(premise.statement)(this)
      if extraction.premises.isEmpty
    } yield KnownStatement(extraction.conclusion, extraction.innerExtraction.derivation)

    (given ++ simplified ++ extracted).deduplicate
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

  lazy val knownValuesToProperties: Seq[ValueToPropertyDerivation] = {
    (for {
      equality <- provingContext.equalityOption.toSeq
      knownStatement <- allPremisesAfterRewrites
      result <- ValueToPropertyDerivation.getFromKnownStatement(knownStatement, equality)(stepContext)
    } yield result) ++ provingContext.factValuesToProperties
  }
  lazy val knownPropertiesToValues: Seq[PropertyToValueDerivation] = {
    (for {
      equality <- provingContext.equalityOption.toSeq
      knownStatement <- allPremisesAfterRewrites
      result <- PropertyToValueDerivation.getFromKnownStatement(knownStatement, equality)(stepContext)
    } yield result) ++ provingContext.factPropertiesToValues
  }

  lazy val knownStatementsFromPremises: Seq[KnownStatement] = {

    val byLocalEqualities = for {
      equality <- provingContext.equalityOption.toSeq
      equalityStatement <- allPremisesAfterRewrites
      (lhs, rhs) <- equality.unapply(equalityStatement.statement).toSeq
      if lhs.asOptionalInstanceOf[TermVariable].exists(_.arguments.isEmpty) || lhs.isInstanceOf[FunctionParameter]
      knownStatement <- allPremisesAfterRewrites
      newStep <- EqualityRewriter.getForwardReplacements(knownStatement.statement, lhs, rhs, equality, Wrapper.identity)(stepContext)
    } yield KnownStatement.fromDerivation(knownStatement.derivation ++ equalityStatement.derivation :+ newStep)

    val byKnownProperties = for {
      PropertyToValueDerivation(propertyTerm, valueTerm, derivation, equality) <- knownPropertiesToValues
      knownStatement <- allPremisesAfterRewrites
      newStep <- EqualityRewriter.getForwardReplacements(knownStatement.statement, propertyTerm, valueTerm, equality, Wrapper.identity)(stepContext)
    } yield KnownStatement.fromDerivation(knownStatement.derivation ++ derivation :+ newStep)

    allPremisesAfterRewrites ++ byLocalEqualities ++ byKnownProperties
  }

  lazy val knownStatementsFromPremisesBySerializedStatement: Map[String, KnownStatement] = {
    knownStatementsFromPremises.map(s => s.statement.serialized -> s).toMapPreservingEarliest
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

  def updateStepContext(f: StepContext => StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.updateStepContext(f)
  }
  def withStepContext(newStepContext: StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.withStepContext(newStepContext)
  }
}
