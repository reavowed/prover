package net.prover.model.proof

import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.{DerivedStatement, PremiseDerivation}
import net.prover.proving.extraction.ExtractionCalculator
import net.prover.proving.structure.inferences.{PremiseSimplificationInference, Reversal}
import net.prover.proving.structure.statements.BinaryRelation

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

  // Paired with whether the extraction ends in a reversal
  private lazy val allPremiseExtractions: Seq[(DerivedStatement, Boolean)] = {
    val `given` = premisesAndSimplifications.map(_._1).map(p => DerivedStatement.Known(p.statement))
    val simplified = premisesAndSimplifications.flatMap(_._2).map(p => DerivedStatement.Known(p.statement))
    val givenAndSimplified = `given` ++ simplified
    val extracted = for {
      premise <- allPremises
      extraction <- ExtractionCalculator.getPremiseExtractions(premise.statement)
      if extraction.premises.isEmpty && !extraction.extractionDetails.allSteps.exists(step => givenAndSimplified.exists(_.statement == step.statement))
    } yield (DerivedStatement.fromPremiseExtraction(extraction), extraction.extractionDetails.reversalStep.nonEmpty)
    (givenAndSimplified.map(_ -> false) ++ extracted).distinctBy(_._1.statement)
  }
  // A reversal should only come after any simplifications, so reversed extractions skip the simplification pass and
  // rejoin for the rewrite pass (which will also reverse any simplified statements)
  private lazy val unreversedPremiseExtractions: Seq[DerivedStatement] = allPremiseExtractions.filter(!_._2).map(_._1)
  private lazy val reversedPremiseExtractions: Seq[DerivedStatement] = allPremiseExtractions.filter(_._2).map(_._1)

  private def simplifyAll(
    previous: Seq[DerivedStatement],
    current: Seq[DerivedStatement],
    simplifiersByRelation: Map[BinaryRelation, Seq[PremiseSimplificationInference]],
    reversals: Seq[Reversal[_ <: Expression]]
  ): Seq[DerivedStatement] = {
    if (current.isEmpty)
      previous
    else {
      val existing = previous ++ current
      val newSimplifications = current.flatMap { currentStatement =>
        val simplifiers = provingContext.asBinaryRelationStatement(currentStatement.statement).flatMap(r => simplifiersByRelation.get(r.relation)).getOrElse(Nil)
        val simplifications = simplifiers.mapCollect(_.getPremiseSimplification(currentStatement, existing))
        val reversedStatements = reversals.mapCollect(_.reverse(currentStatement))
        (simplifications ++ reversedStatements).filter { newStatement => !existing.exists(_.statement == newStatement.statement) }
      }
      simplifyAll(existing, newSimplifications, simplifiersByRelation, reversals)
    }
  }

  // Only binary relations are rewritten, so only their reversals are needed alongside the rewrites
  private lazy val binaryRelationReversals: Seq[Reversal[_ <: Expression]] = provingContext.reversals.filter(_.joiner.isInstanceOf[BinaryRelation])
  private lazy val allPremisesAfterSimplifications = simplifyAll(Nil, unreversedPremiseExtractions, provingContext.premiseRelationSimplificationInferences, Nil)
  lazy val derivedStatementsFromPremises: Seq[DerivedStatement] = simplifyAll(Nil, (allPremisesAfterSimplifications ++ reversedPremiseExtractions).deduplicate, provingContext.premiseRelationRewriteInferences, binaryRelationReversals)
  lazy val derivedStatementsFromPremisesBySerializedStatement: Map[String, DerivedStatement] = {
    derivedStatementsFromPremises.map(s => s.statement.serializedForHash -> s).toMapPreservingEarliest
  }

  lazy val knownEqualities: Seq[KnownEquality] = {
    for {
      equality <- provingContext.equalityOption.toSeq
      derivation <- derivedStatementsFromPremises ++ provingContext.facts.map(_.toDerivedStatement)
      (lhs, rhs) <- equality.unapply(derivation.statement)
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
