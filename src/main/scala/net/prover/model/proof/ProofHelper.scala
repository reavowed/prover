package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, Fact, GeneralizationDefinition, NamingInference}
import net.prover.model.expressions._
import net.prover.proving.premiseFinding.DerivationOrTargetFinder

object ProofHelper {
  def findFactBySubstituting(target: Statement, substitutionsSoFar: Substitutions.Possible)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(Fact, Substitutions.Possible)] = {
    provingContext.facts.mapFind { fact =>
      for {
        substitutions <- target.calculateSubstitutions(fact.statement, substitutionsSoFar)
      } yield (fact, substitutions)
    }
  }

  def findNamingInferences(implicit availableEntries: AvailableEntries): Seq[NamingInference] = {
    availableEntries.allInferences.mapCollect(asNamingInference)
  }

  def asNamingInference(inference: Inference)(implicit availableEntries: AvailableEntries): Option[NamingInference] = {
    (availableEntries.generalizationDefinitionOption, availableEntries.deductionDefinitionOption) match {
      case (Some(generalizationDefinition), Some(deductionDefinition)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+ generalizationDefinition(_, deductionDefinition(assumption: Statement, StatementVariable(deductionConclusionVariableName, Nil))),
            StatementVariable(conclusionVariableName, Nil)
          ) if deductionConclusionVariableName == conclusionVariableName =>
            Some(NamingInference(inference, initialPremises, assumption, generalizationDefinition, deductionDefinition))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }
}
