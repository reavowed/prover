package net.prover.model.proof

import net.prover.entries.StepWithContext
import net.prover.model.definitions.{BinaryRelation, KnownStatement, PremiseSimplificationInference}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.utils.ExpressionUtils
import net.prover.model.{ProvingContext, VariableDefinitions}
import net.prover.proving.extraction.SubstatementExtractor

import scala.collection.mutable

case class StepContext private(
    stepReference: StepReference,
    variableDefinitions: VariableDefinitions,
    boundVariableLists: Seq[Seq[String]],
    premises: Seq[Premise.Given])(
    implicit val provingContext: ProvingContext)
  extends SubstitutionContext
{
  private implicit val stepContext: StepContext = this

  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(stepReference = stepReference.forChild(index))
  def addBoundVariable(name: String): StepContext = copy(
    boundVariableLists = boundVariableLists :+ Seq(name),
    premises = premises.map(_.insertExternalParameters(1, 0)))


  private def addPremise(givenPremise: Premise.Given): StepContext = {
    copy(premises = premises :+ givenPremise)
  }
  def addAssumption(assumption: Statement): StepContext = addStatement(assumption, "a")

  def addStatement(statement: Statement, reference: PreviousLineReference): StepContext = {
    addPremise(Premise.Given(statement, reference))
  }
  def addStatement(statement: Statement, suffix: String): StepContext = {
    addStatement(statement, stepReference.withSuffix(suffix))
  }
  def addStep(step: Step, reference: PreviousLineReference): StepContext = {
    step.provenStatement.map(addStatement(_, reference)).getOrElse(this)
  }
  def addSteps(steps: Seq[Step]): StepContext = {
    steps.zipWithIndex.foldLeft(this) { case (context, (step, index)) =>
      context.addStep(step, stepReference.forChild(index))
    }
  }

  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p))
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
      extraction <- SubstatementExtractor.getPremiseExtractions(premise.statement)
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
          simplifier.getPremiseSimplification(currentKnownStatement, existing)
            .filter { newKnownStatement => !existing.exists(_.statement == newKnownStatement.statement) }
        }
      }
      }
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

  val cachedDerivations: mutable.Map[String, Option[Seq[DerivationStep]]] = mutable.Map.empty
}

object StepContext {
  def withPremisesAndVariables(
    premises: Seq[Statement],
    variableDefinitions: VariableDefinitions)(
    implicit provingContext: ProvingContext
  ): StepContext = {
    val emptyContext = StepContext(StepReference(Nil), variableDefinitions, Nil, Nil)(provingContext)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addStatement(premise, PremiseReference(index))
    }
  }

  implicit def fromStepWithContext(stepWithContext: StepWithContext): StepContext = stepWithContext.stepContext
  implicit def implicitlyFromStepWithContext(implicit stepWithContext: StepWithContext): StepContext = fromStepWithContext(stepWithContext)
}

