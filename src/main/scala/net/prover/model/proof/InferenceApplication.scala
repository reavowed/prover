package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement

sealed trait InferenceApplication {
  def inference: Inference
  def referencedInferenceIds: Set[String]
  def referencedLines: Set[PreviousLineReference]
  def references: Seq[Reference]
  def isRearrangement: Boolean
  def conclusion: Statement
  def serialized: String
}

object InferenceApplication {
  case class Direct(
      inference: Inference.Summary,
      substitutions: Substitutions,
      conclusion: Statement,
      references: Seq[Reference],
      isRearrangement: Boolean)
    extends InferenceApplication
  {
    override def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def referencedLines = references.flatMap(_.lineReferences).toSet
    override def serialized = (
      Seq("direct", inference.id) ++
      (if (isRearrangement) Seq("rearranged") else Nil) ++
      Seq(inference.serializeSubstitutions(substitutions), references.serialized)
    ).mkString(" ")
  }
  object Direct {
    def parser(implicit context: ParsingContext): Parser[Direct] = {
      for {
        inference <- Inference.parser
        isRearrangement <- Parser.optionalWord("rearranged").isDefined
        substitutions <- inference.substitutionsParser
        references <- Reference.listParser
      } yield {
        val conclusion = inference.conclusion.applySubstitutions(substitutions, 0, context.parameterDepth)
          .getOrElse(throw new Exception(s"Inference '${inference.name}' could not be applied with substitutions $substitutions"))
        Direct(inference, substitutions, conclusion, references, isRearrangement)
      }
    }
  }

  case class Transformed(
      inference: Inference.Transformed,
      substitutions: Substitutions,
      conclusion: Statement,
      references: Seq[Reference],
      isRearrangement: Boolean)
    extends InferenceApplication
  {
    override def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet + inference.transformation.specificationInference.id
    override def referencedLines = references.flatMap(_.lineReferences).toSet
    override def serialized = (
      Seq("transformed", inference.inner.id) ++
        (if (isRearrangement) Seq("rearranged") else Nil) ++
        Seq(
          "(" + inference.arePremisesScoped.map(Transformed.serializeBoolean).mkString(" ") + ")",
          Transformed.serializeBoolean(inference.isConclusionScoped),
          inference.serializeSubstitutions(substitutions),
          references.serialized)
      ).mkString(" ")
  }
  object Transformed {
    def serializeBoolean(boolean: Boolean): String = if (boolean) "Y" else "N"
    def booleanParser: Parser[Boolean] = {
      Parser.selectWord("boolean") {
        case "Y" => true
        case "N" => false
      }
    }

    def parser(implicit context: ParsingContext): Parser[Transformed] = {
      for {
        inference <- Inference.parser
        isRearrangement <- Parser.optionalWord("rearranged").isDefined
        transformation = context.transformation
          .getOrElse(throw new Exception(s"No valid transformation found"))
        transformationSubstitutions = transformation.getSubstitutions(inference)
            .getOrElse(throw new Exception(s"Inference '${inference.name} was invalid for transformation"))
        arePremisesScoped <- booleanParser.listInParens(None)
        isConclusionScoped <- booleanParser
        transformedPremises = inference.premises.zip(arePremisesScoped).map { case (premise, isScoped) =>
            premise.withStatement(
              premise.statement
                .applySubstitutions(transformationSubstitutions, 0, 0)
                .map(if (isScoped) transformation.generalise else transformation.specify)
                .getOrElse(throw new Exception(s"Could not apply transformation to premise ${premise.toString}")))
        }
        transformedConclusion = inference.conclusion
          .applySubstitutions(transformationSubstitutions, 0, 0)
          .map(if (isConclusionScoped) transformation.generalise else transformation.specify)
          .getOrElse(throw new Exception(s"Could not apply transformation to conclusion ${inference.conclusion}"))
        transformedInference = Inference.Transformed(inference, transformation, transformedPremises, transformedConclusion)
        substitutions <- transformedInference.substitutionsParser
        references <- Reference.listParser
      } yield {
        val conclusion = transformedConclusion.applySubstitutions(substitutions, 0, context.parameterDepth)
          .getOrElse(throw new Exception(s"Transformed inference '${inference.name}' could not be applied with substitutions $substitutions"))
        Transformed(
          transformedInference,
          substitutions,
          conclusion,
          references,
          isRearrangement)
      }
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[InferenceApplication] = {
    Parser.selectWordParser("inference application") {
      case "direct" => Direct.parser
      case "transformed" => Transformed.parser
    }
  }
}
