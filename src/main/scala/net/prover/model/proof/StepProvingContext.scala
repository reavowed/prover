package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions._
import net.prover.model.expressions.{FunctionParameter, Statement, Term, TermVariable}
import net.prover.model.proof.StepProvingContext.ValueToPropertyDerivation
import net.prover.model.utils.ExpressionUtils

import scala.collection.mutable

case class StepProvingContext(stepContext: StepContext, provingContext: ProvingContext) {
  lazy val premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])] = {
    stepContext.premises.reverse.map(p => p -> SimplificationFinder.getSimplifications(p)(this))
  }

  lazy val allPremises: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }

  lazy val allPremiseExtractions: Seq[KnownStatement] = {
    for {
      premise <- stepContext.premises
      extractionOption <- SubstatementExtractor.getExtractionOptions(premise.statement)(this)
      if extractionOption.premises.isEmpty
    } yield KnownStatement(extractionOption.conclusion, extractionOption.derivation)
  }

  lazy val knownStatementsFromPremises: Seq[KnownStatement] = {
    implicit val substitutionContext: SubstitutionContext = stepContext
    def simplifyAll(
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
    def replaceEqualities(current: Seq[KnownStatement]): Seq[KnownStatement] = {
      def getReplacements(lhs: Term, rhs: Term, equality: Equality, equalityStatement: KnownStatement): Seq[KnownStatement] = {
        for {
          knownStatement <- current
          newStep <- EqualityRewriter.getForwardReplacements(knownStatement.statement, lhs, rhs, equality, Wrapper.identity)(stepContext)
        } yield KnownStatement.fromDerivation(equalityStatement.derivation ++ knownStatement.derivation :+ newStep)
      }

      val equalityPremises = for {
        equality <- provingContext.equalityOption.toSeq
        knownStatement <- current
        (lhs, rhs) <- equality.unapply(knownStatement.statement).toSeq
        if (lhs.asOptionalInstanceOf[TermVariable].exists(_.arguments.isEmpty) || lhs.isInstanceOf[FunctionParameter])
        newKnownStatement <- getReplacements(lhs, rhs, equality, knownStatement)
      } yield newKnownStatement

      current ++ equalityPremises
    }

    val afterSimplifications = simplifyAll(Nil, allPremiseExtractions, provingContext.premiseRelationSimplificationInferences)
    val afterRewrites = simplifyAll(Nil, afterSimplifications, provingContext.premiseRelationRewriteInferences)
    replaceEqualities(afterRewrites)
  }

  lazy val knownStatementsFromPremisesBySerializedStatement: Map[String, KnownStatement] = {
    knownStatementsFromPremises.map(s => s.statement.serialized -> s).toMapPreservingEarliest
  }

  private def isProperty(propertyTerm: Term) = ExpressionUtils.isWrappedSimpleTerm(propertyTerm)
  private def isValue(valueTerm: Term) = ExpressionUtils.isSimpleTermVariable(valueTerm) || ExpressionUtils.isCombinationOfTermConstants(valueTerm)

  lazy val knownValuesToProperties: Seq[ValueToPropertyDerivation] = {
    for {
      equality <- provingContext.equalityOption.toSeq
      knownStatement <- knownStatementsFromPremises ++ provingContext.facts.map(KnownStatement.fromSingleStep)
      (valueTerm, propertyTerm) <- equality.unapply(knownStatement.statement)(stepContext).toSeq
      if isProperty(propertyTerm) && isValue(valueTerm)
    } yield ValueToPropertyDerivation(propertyTerm, valueTerm, knownStatement.derivation, equality)
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
  case class ValueToPropertyDerivation(propertyTerm: Term, valueTerm: Term, valueToPropertyDerivation: Seq[DerivationStep], equality: Equality)

  def updateStepContext(f: StepContext => StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.updateStepContext(f)
  }
  def withStepContext(newStepContext: StepContext)(implicit stepProvingContext: StepProvingContext): StepProvingContext = {
    stepProvingContext.withStepContext(newStepContext)
  }
}
