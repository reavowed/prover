package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.Statement

sealed trait CachedInferenceApplication {
  def validate()(implicit context: ProvingContext): Option[(Statement, InferenceApplication)]
  def serializedLines: Seq[String]
}

object CachedInferenceApplication {
  case class Direct(
      inferenceId: String,
      localSubstitutions: Inference.Substitutions,
      cachedReferences: Seq[CachedReference],
      isRearrangement: Boolean,
      depth: Int)
    extends CachedInferenceApplication
  {
    override def validate()(implicit context: ProvingContext): Option[(Statement, InferenceApplication.Direct)] = {
      for {
        inference <- context.availableInferences.find(_.id == inferenceId)
        substitutions <- inference.generalizeSubstitutions(localSubstitutions, depth)
        substitutedPremiseStatements <- inference.premises.map(_.statement.applySubstitutions(substitutions)).traverseOption.ifEmpty {
          CachedProof.logger.info(
            (Seq(s"Could not substitute into premises of inference '${inference.name}'") ++
              inference.premises.map(_.serialized)
              :+ substitutions.toString
              ).mkString("\n"))
        }
        substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions).ifEmpty {
          CachedProof.logger.info(Seq(
            s"Could not substitute into conclusion of inference '${inference.name}'",
            inference.conclusion.serialized,
            substitutions.toString
          ).mkString("\n"))
        }
        validatedReferences <- cachedReferences.validate(substitutedPremiseStatements)
      } yield (substitutedConclusion, InferenceApplication.Direct(inference, substitutions, validatedReferences, isRearrangement, depth))
    }

    override def serializedLines = Seq(s"direct ${if (isRearrangement) "rearranged " else ""}$inferenceId ${localSubstitutions.serialized} {") ++
      cachedReferences.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      for {
        isRearrangement <- Parser.optionalWord("rearranged").isDefined
        inferenceId <- Parser.singleWord
        substitutions <- Inference.Substitutions.parser
        references <- CachedReference.parser.listInBraces(None)
      } yield Direct(inferenceId, substitutions, references, isRearrangement, parsingContext.parameterDepth)
    }
  }

  case class Transformed(
      inferenceId: String,
      localSubstitutions: Inference.Substitutions,
      cachedReferences: Seq[CachedReference],
      transformation: StatementDefinition,
      transformedPremises: Seq[Premise],
      transformationProof: Seq[CachedStep],
      isRearrangement: Boolean,
      depth: Int)
    extends CachedInferenceApplication
  {
    override def validate()(implicit context: ProvingContext): Option[(Statement, InferenceApplication.Transformed)] = {
      for {
        inference <- context.availableInferences.find(_.id == inferenceId)
        validatedTransformationProof <- transformationProof.validate(ProvingContext.getInitial(
          transformedPremises,
          context.assertionHints,
          context.availableInferences,
          context.deductionStatement.toSeq ++ context.scopingStatement.toSeq))
        transformedConclusion <- validatedTransformationProof.flatMap(_.provenStatements).lastOption.map(_.statement)
        transformedInference = Inference.Transformed(inference, transformedPremises, transformedConclusion)
        substitutions <- transformedInference.generalizeSubstitutions(localSubstitutions, depth)
        substitutedPremiseStatements <- transformedPremises.map(_.statement.applySubstitutions(substitutions)).traverseOption.ifEmpty {
          CachedProof.logger.info(
            (Seq(s"Could not substitute into premises of transformed inference '${inference.name}'") ++
              transformedPremises.map(_.serialized)
              :+ substitutions.toString
              ).mkString("\n"))
        }
        substitutedConclusion <- transformedConclusion.applySubstitutions(substitutions).ifEmpty {
          CachedProof.logger.info(Seq(
            s"Could not substitute into conclusion of transformed inference '${inference.name}'",
            transformedConclusion.serialized,
            substitutions.toString
          ).mkString("\n"))
        }
        validatedReferences <- cachedReferences.validate(substitutedPremiseStatements)
    } yield (substitutedConclusion, InferenceApplication.Transformed(
        inference,
        substitutions,
        validatedReferences,
        transformation,
        transformedPremises,
        transformedConclusion,
        validatedTransformationProof,
        isRearrangement,
        depth))
    }
    override def serializedLines = Seq(s"transformed ${if (isRearrangement) "rearranged " else ""}${transformation.symbol} $inferenceId ${localSubstitutions.serialized} {") ++
      cachedReferences.flatMap(_.serializedLines).indent ++
      Seq("}") ++
      transformedPremises.map(_.serialized) ++
      Seq("{") ++
      transformationProof.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Transformed {
    def parser(implicit parsingContext: ParsingContext): Parser[Transformed] = {
      for {
        isRearrangement <- Parser.optionalWord("rearranged").isDefined
        symbol <- Parser.singleWord
        transformation = parsingContext.statementDefinitions.find(_.symbol == symbol)
          .getOrElse(throw new Exception(s"Unrecognised statement type $symbol"))
        inferenceId <- Parser.singleWord
        substitutions <- Inference.Substitutions.parser
        references <- CachedReference.parser.listInBraces(None)
        resetCcntext = parsingContext.copy(parameterLists = Nil)
        transformedPremises <- Premise.listParser(resetCcntext)
        transformationProof <- CachedStep.listParser(None)(resetCcntext).inBraces
      } yield Transformed(
        inferenceId,
        substitutions,
        references,
        transformation,
        transformedPremises,
        transformationProof,
        isRearrangement,
        parsingContext.parameterDepth)
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[CachedInferenceApplication] = {
    Parser.selectWordParser("inference application") {
      case "direct" => Direct.parser
      case "transformed" => Transformed.parser
    }
  }
}
