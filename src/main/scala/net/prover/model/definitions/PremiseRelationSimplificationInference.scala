package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepProvingContext
import net.prover.model.proof.SubstatementExtractor.ExtractionOption

case class PremiseRelationSimplificationInference(inference: Inference, premise: Statement, conclusion: Statement, extractionOption: ExtractionOption) extends PremiseSimplificationInference {
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit stepProvingContext: StepProvingContext): Option[KnownStatement] = {
    for {
      substitutions <- premise.calculateSubstitutions(currentStatement.statement).flatMap(_.confirmTotality)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
    } yield currentStatement.extend(Seq(derivationStep))
  }
}
