package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.Statement

sealed trait CachedInferenceApplication {
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
  def validate(context: ProvingContext): Option[(Statement, InferenceApplication)]
  def serializedLines: Seq[String]
}

object CachedInferenceApplication {
  case class Direct(
      inferenceId: String,
      localSubstitutions: Inference.Substitutions,
      cachedReferences: Seq[CachedReference],
      depth: Int)
    extends CachedInferenceApplication
  {
    override def getAssertionHints(availableInferences: Seq[Inference]) = {
      AssertionHint.attempt(inferenceId, availableInferences, localSubstitutions, depth).toSeq ++
        cachedReferences.flatMap(_.getAssertionHints(availableInferences))
    }

    override def validate(context: ProvingContext): Option[(Statement, InferenceApplication.Direct)] = {
      for {
        inference <- context.availableInferences.find(_.id == inferenceId).ifEmpty {
          CachedProof.logger.info(s"Could not find inference $inferenceId")
        }
        substitutions <- inference.generalizeSubstitutions(localSubstitutions, depth)
        substitutedPremiseFacts <- inference.premises.map(_.fact.applySubstitutions(substitutions)).traverseOption.ifEmpty {
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
        validatedReferences <- cachedReferences.validate(substitutedPremiseFacts, context)
      } yield (substitutedConclusion, InferenceApplication.Direct(inference, substitutions, validatedReferences, depth))
    }

    override def serializedLines = Seq(s"direct $inferenceId ${localSubstitutions.serialized} {") ++
      cachedReferences.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      for {
        inferenceId <- Parser.singleWord
        substitutions <- Inference.Substitutions.parser
        references <- CachedReference.parser.listInBraces(None)
      } yield Direct(inferenceId, substitutions, references, parsingContext.parameterDepth)
    }
  }

  case class Transformed(
      inferenceId: String,
      localSubstitutions: Inference.Substitutions,
      cachedReferences: Seq[CachedReference],
      transformation: StatementDefinition,
      transformedPremises: Seq[Premise],
      transformationProof: Seq[CachedStep],
      depth: Int)
    extends CachedInferenceApplication
  {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      AssertionHint.attempt(inferenceId, availableInferences, localSubstitutions, depth).toSeq ++
        cachedReferences.flatMap(_.getAssertionHints(availableInferences)) ++
        transformationProof.flatMap(_.getAssertionHints(availableInferences))
    }
    override def validate(context: ProvingContext): Option[(Statement, InferenceApplication.Transformed)] = {
      for {
        inference <- context.availableInferences.find(_.id == inferenceId).ifEmpty {
          CachedProof.logger.info(s"Could not find inference $inferenceId")
        }
        validatedTransformationProof <- transformationProof.validate(Proof.getInitialContext(
          transformedPremises,
          context.availableInferences,
          context.assertionHints,
          Nil))
        transformedConclusion <- validatedTransformationProof.ofType[Step.WithAssertion].lastOption.map(_.assertion)
        transformedInference = Inference.Transformed(inference, transformedPremises, transformedConclusion)
        substitutions <- transformedInference.generalizeSubstitutions(localSubstitutions, depth)
        substitutedPremiseFacts <- transformedPremises.map(_.fact.applySubstitutions(substitutions)).traverseOption.ifEmpty {
          CachedProof.logger.info(
            (Seq(s"Could not substitute into premises of transformed inference '${inference.name}'") ++
              inference.premises.map(_.serialized)
              :+ substitutions.toString
              ).mkString("\n"))
        }
        substitutedConclusion <- transformedConclusion.applySubstitutions(substitutions).ifEmpty {
          CachedProof.logger.info(Seq(
            s"Could not substitute into conclusion of transformed inference '${inference.name}'",
            inference.conclusion.serialized,
            substitutions.toString
          ).mkString("\n"))
        }
        validatedReferences <- cachedReferences.validate(substitutedPremiseFacts, context)
    } yield (substitutedConclusion, InferenceApplication.Transformed(
        inference,
        substitutions,
        validatedReferences,
        transformation,
        transformedPremises,
        transformedConclusion,
        validatedTransformationProof,
        depth))
    }
    override def serializedLines = Seq(s"transformed ${transformation.symbol} $inferenceId ${localSubstitutions.serialized} {") ++
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
