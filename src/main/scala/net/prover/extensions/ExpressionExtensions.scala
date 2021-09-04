package net.prover.extensions

import net.prover.old.SubstitutionApplicationImplicits
import net.prover.utilities.references.{ReferencedCompoundExpressionsCalculatorImplicits, UsedVariablesCalculatorImplicits}

object ExpressionExtensions
  extends ReferencedCompoundExpressionsCalculatorImplicits
  with SubstitutionApplicationImplicits
  with UsedVariablesCalculatorImplicits
