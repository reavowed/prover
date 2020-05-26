package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.proof.{DerivationStep, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils

object PropertyOrValueDerivation {
  def isProperty(propertyTerm: Term) = ExpressionUtils.isWrappedSimpleTerm(propertyTerm)
  def isValue(valueTerm: Term) = ExpressionUtils.isSimpleTermVariable(valueTerm) || ExpressionUtils.isCombinationOfTermConstants(valueTerm)
}

case class ValueToPropertyDerivation(valueTerm: Term, propertyTerm: Term, derivation: Seq[DerivationStep], equality: Equality)
object ValueToPropertyDerivation {
  def getFromKnownStatement(knownStatement: KnownStatement, equality: Equality)(implicit substitutionContext: SubstitutionContext): Option[ValueToPropertyDerivation] = {
    for {
      (valueTerm, propertyTerm) <- equality.unapply(knownStatement.statement)
      if PropertyOrValueDerivation.isProperty(propertyTerm) && PropertyOrValueDerivation.isValue(valueTerm)
    } yield ValueToPropertyDerivation(valueTerm, propertyTerm, knownStatement.derivation, equality)
  }
}

case class PropertyToValueDerivation(propertyTerm: Term, valueTerm: Term, derivation: Seq[DerivationStep], equality: Equality)
object PropertyToValueDerivation {
  def getFromKnownStatement(knownStatement: KnownStatement, equality: Equality)(implicit substitutionContext: SubstitutionContext): Option[PropertyToValueDerivation] = {
    for {
      (propertyTerm, valueTerm) <- equality.unapply(knownStatement.statement)
      if PropertyOrValueDerivation.isProperty(propertyTerm) && PropertyOrValueDerivation.isValue(valueTerm)
    } yield PropertyToValueDerivation(propertyTerm, valueTerm, knownStatement.derivation, equality)
  }
}

