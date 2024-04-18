package net.prover.model.proof

import net.prover.entries.StepWithContext
import net.prover.model.ProvingContext
import net.prover.model.definitions.{BinaryRelation, KnownStatement, PremiseSimplificationInference}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.{PremiseDerivation, SimpleDerivation}
import net.prover.proving.extraction.ExtractionCalculator

import scala.collection.mutable

class StepProvingContext(implicit val stepContext: StepContext, val provingContext: ProvingContext)
{
  import stepContext.premises

  def withStepContext(newStepContext: StepContext): StepProvingContext = {
    new StepProvingContext()(newStepContext, provingContext)
  }
  def updateStepContext(f: StepContext => StepContext): StepProvingContext = {
    withStepContext(f(stepContext))
  }

  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  private lazy val allPremiseExtractions: Seq[KnownStatement] = {
    val given = premisesAndSimplifications.map(_._1).map(p => KnownStatement(p.statement, SimpleDerivation.empty))
    val simplified = premisesAndSimplifications.flatMap(_._2).map(p => KnownStatement(p.statement, SimpleDerivation.empty))
    val givenAndSimplified = given ++ simplified
    val extracted = for {
      premise <- allPremises
      extraction <- ExtractionCalculator.getPremiseExtractions(premise.statement)
      if extraction.premises.isEmpty && !extraction.extractionDetails.derivation.exists(step => givenAndSimplified.exists(_.statement == step.statement))
    } yield KnownStatement.fromExtraction(extraction)
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
          simplifier.getPremiseSimplification(currentKnownStatement, existing)
            .filter { newKnownStatement => !existing.exists(_.statement == newKnownStatement.statement) }
        }
      }
      }
      simplifyAll(existing, newSimplifications, simplifiersByRelation)
    }
  }

  private lazy val allPremisesAfterSimplifications = simplifyAll(Nil, allPremiseExtractions, provingContext.premiseRelationSimplificationInferences)
  lazy val knownStatementsFromPremises: Seq[KnownStatement] = simplifyAll(Nil, allPremisesAfterSimplifications, provingContext.premiseRelationRewriteInferences)
  lazy val knownStatementsFromPremisesBySerializedStatement: Map[String, KnownStatement] = {
    knownStatementsFromPremises.map(s => s.statement.serializedForHash -> s).toMapPreservingEarliest
  }

  lazy val knownEqualities: Seq[KnownEquality] = {
    for {
      equality <- provingContext.equalityOption.toSeq
      KnownStatement(statement, derivation) <- knownStatementsFromPremises ++ provingContext.facts.map(_.toKnownStatement)
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

  val cachedDerivations: mutable.Map[String, Option[PremiseDerivation]] = mutable.Map.empty
}

object StepProvingContext {
  implicit def fromStepWithContext(stepWithContext: StepWithContext): StepProvingContext = stepWithContext.stepProvingContext
  implicit def implicitlyFromStepWithContext(implicit stepWithContext: StepWithContext): StepProvingContext = fromStepWithContext(stepWithContext)
}

