package net.prover.model.definitions

import net.prover.model.*
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.proving.derivation.DerivedStatement

sealed trait DerivedPremise {
  def getRootPremises: Seq[Statement]
  /**
    * The substituted root premises, along with a function that takes derivations of the root premises (in the same
    * order) to a derivation of this premise.
    */
  def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[DerivedStatement] => DerivedStatement)]
}
object DerivedPremise {
  implicit class SeqOps(premiseDesimplifications: Seq[DerivedPremise]) {
    /**
      * The substituted root premises of all these premises, along with a function that takes derivations of the root
      * premises (in the same order) to derivations of each of these premises.
      */
    def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[DerivedStatement] => Seq[DerivedStatement])] = {
      for {
        rootPremisesAndDerivations <- premiseDesimplifications.map(_.getSubstitutedPremises(substitutions)).traverseOption
      } yield {
        def derivePremises(rootDerivations: Seq[DerivedStatement]): Seq[DerivedStatement] = {
          rootPremisesAndDerivations.foldLeft((Seq.empty[DerivedStatement], rootDerivations)) { case ((premiseDerivationsSoFar, remainingRootDerivations), (rootPremises, derivePremise)) =>
            val (rootDerivationsForPremise, otherRootDerivations) = remainingRootDerivations.splitAt(rootPremises.length)
            (premiseDerivationsSoFar :+ derivePremise(rootDerivationsForPremise), otherRootDerivations)
          }._1
        }
        (rootPremisesAndDerivations.flatMap(_._1), derivePremises)
      }
    }
  }
}

case class DirectPremise(premise: Statement) extends DerivedPremise {
  def getRootPremises: Seq[Statement] = Seq(premise)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[DerivedStatement] => DerivedStatement)] = {
    for {
      substitutedPremise <- premise.applySubstitutions(substitutions)
    } yield (Seq(substitutedPremise), _.head)
  }
}
case class DesimplifiedPremise(premise: Statement, inference: Inference, innerPremises: Seq[DerivedPremise]) extends DerivedPremise {
  def getRootPremises: Seq[Statement] = innerPremises.flatMap(_.getRootPremises)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[DerivedStatement] => DerivedStatement)] = {
    for {
      substitutedPremise <- premise.applySubstitutions(substitutions)
      inferenceSubstitutions <- inference.conclusion.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality(inference.variableDefinitions))
      assertionStep <- Step.AssertionStep.forInference(inference, inferenceSubstitutions)
      (rootPremises, deriveInnerPremises) <- innerPremises.getSubstitutedPremises(substitutions)
    } yield (rootPremises, rootDerivations => DerivedStatement.Composed(deriveInnerPremises(rootDerivations), assertionStep))
  }
}
