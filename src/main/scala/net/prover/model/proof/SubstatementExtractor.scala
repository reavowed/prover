package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {

  case class ExtractionOption(conclusion: Statement, premises: Seq[Statement], extractionInferences: Seq[Inference], additionalVariableNames: Seq[String]) {
    def requiredSubstitutions: Substitutions.Required = (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether
  }

  case class VariableTracker(baseVariableNames: Seq[String], additionalVariableNames: Seq[String]) {
    def namesUsedSoFar: Seq[String] = baseVariableNames ++ additionalVariableNames
    def getAndAddUniqueVariableName(baseName: String): (String, VariableTracker) = {
      val newName = if (!namesUsedSoFar.contains(baseName))
        baseName
      else {
        val i = Stream.from(1).find(i => !namesUsedSoFar.contains(s"${baseName}_$i")).get
        s"${baseName}_$i"
      }
      (newName, VariableTracker(baseVariableNames, additionalVariableNames :+ newName))
    }
  }
  object VariableTracker {
    def fromInference(inference: Inference): VariableTracker = VariableTracker(inference.requiredSubstitutions.terms.map(_._1), Nil)
    def fromStepContext(implicit stepContext: StepContext): VariableTracker = VariableTracker(stepContext.termVariableNames, Nil)
  }

  private def getBaseExtractionOption(sourceStatement: Statement, variableTracker: VariableTracker): Seq[ExtractionOption] = {
    Seq(ExtractionOption(sourceStatement, Nil, Nil, variableTracker.additionalVariableNames))
  }

  private def getStatementExtractionOptions(
    sourceStatement: Statement,
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement],
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
      innerOption <- recurse(extractedConclusion, variableTracker)
      newPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)).swap.toSeq
      if !newPremiseOption.contains(innerOption.conclusion) // Filter out spurious extractions
    } yield innerOption.copy(premises = newPremiseOption.toSeq ++ innerOption.premises, extractionInferences = inference +: innerOption.extractionInferences)
  }

  private def getStatementExtractionOptions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      (inference, extractionPremise, otherPremiseOption) <- provingContext.statementExtractionInferences
      extractionOption <- getStatementExtractionOptions(sourceStatement, inference, extractionPremise, otherPremiseOption, recurse, variableTracker)
    } yield extractionOption
  }

  private def getPredicateExtractionOptions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      (inference, extractionPremise, predicateName, _) <- provingContext.specificationInferenceOption.toSeq
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq // missing external depth increase?
      boundVariableName <- sourceStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).toSeq
      (1, extractionPredicate) <- extractionSubstitutions.statements.get(predicateName).toSeq
      (newVariableName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      nextPremise <- extractionPredicate.specify(Seq(TermVariable(newVariableName))).toSeq
      innerOption <- recurse(nextPremise, newVariableTracker)
    } yield innerOption.copy(extractionInferences = inference +: innerOption.extractionInferences)
  }

  private def getDefinitionDeconstructionExtractionOptions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      definedStatement <- sourceStatement.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if implicitly[EntryContext].typeStatementDefinitions.contains(definition)
      deconstructionInference <- definedStatement.definition.deconstructionInference.toSeq
      extractionPremise <- deconstructionInference.premises.single.toSeq
      extractedSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq
      deconstructedStatement <- deconstructionInference.conclusion.applySubstitutions(extractedSubstitutions).toSeq
      innerOption <- recurse(deconstructedStatement, variableTracker)
    } yield innerOption.copy(extractionInferences = deconstructionInference +: innerOption.extractionInferences)
  }

  private def getFinalExtractionOptions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    getBaseExtractionOption(sourceStatement, variableTracker) ++
      provingContext.rewriteInferences.flatMap { case (inference, firstPremise) =>
        getStatementExtractionOptions(
          sourceStatement,
          inference,
          firstPremise,
          None,
          getBaseExtractionOption,
          variableTracker)
      }
  }

  private def getExtractionOptions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    getFinalExtractionOptions(sourceStatement, variableTracker) ++
      getStatementExtractionOptions(sourceStatement, getExtractionOptions, variableTracker) ++
      getPredicateExtractionOptions(sourceStatement, getExtractionOptions, variableTracker) ++
      getDefinitionDeconstructionExtractionOptions(sourceStatement, getExtractionOptions, variableTracker)
  }

  def getExtractionOptions(inference: Inference)(implicit provingContext: ProvingContext): Seq[ExtractionOption] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    getExtractionOptions(inference.conclusion, VariableTracker.fromInference(inference))
      .filter(extractionOption => !inference.premises.contains(extractionOption.conclusion))
      .map(extractionOption => extractionOption.copy(premises = inference.premises ++ extractionOption.premises))
  }

  def getExtractionOptions(premise: Statement)(implicit stepProvingContext: StepProvingContext): Seq[ExtractionOption] = {
    getExtractionOptions(premise, VariableTracker.fromStepContext)
  }
}
