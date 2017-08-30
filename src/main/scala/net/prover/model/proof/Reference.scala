package net.prover.model.proof

import net.prover.model._

sealed trait Reference {
  def serialized: String = serializedLines.mkString("\n")
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def directReferences: Set[String]
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
}

object Reference {
  case class Direct(value: String) extends Reference {
    def serializedLines: Seq[String] = Seq(s"direct $value")
    override val referencedInferenceIds: Set[String] = Set.empty
    override val directReferences: Set[String] = Set(value)
    override def getAssertionHints(availableInferences: Seq[Inference]) = Nil
    def withSuffix(suffix: String): Direct = Direct(value + suffix)
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      Parser.singleWord.map(Direct.apply)
    }
  }

  sealed trait ApplyingInference extends Reference {
    def inferenceApplication: InferenceApplication
    override def directReferences: Set[String] = inferenceApplication.directReferences
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      inferenceApplication.getAssertionHints(availableInferences)
    }
  }

  case class Expansion(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def serializedLines = {
      "expansion" +: inferenceApplication.serializedLines.indent
    }
  }
  object Expansion {
    def parser(implicit parsingContext: ParsingContext): Parser[Expansion] = {
      InferenceApplication.parser.map(Expansion.apply)
    }
  }

  case class Simplification(
      inferenceSummary: Inference.Summary,
      inferenceSubstitutions: Inference.Substitutions,
      inferenceReference: Reference,
      simplificationPath: Seq[Int]) extends ApplyingInference {
    override def inferenceApplication: InferenceApplication = InferenceApplication(
      inferenceSummary,
      inferenceSubstitutions,
      Seq(inferenceReference))
    override def serializedLines = {
      Seq("simplification", inferenceSummary.serialized, inferenceSubstitutions.serialized).mkString(" ") +:
        (inferenceReference.serializedLines :+ "(" + simplificationPath.mkString(" ") + ")").indent
    }
  }
  object Simplification {
    def parser(implicit parsingContext: ParsingContext): Parser[Simplification] = {
      for {
        inferenceSummary <- Inference.Summary.parser
        substitutions <- Inference.Substitutions.parser
        reference <- Reference.parser.getOrElse(throw new Exception("Invalid reference for simplification"))
        simplificationPath <- Parser.int.listInParens(None)
      } yield Simplification(inferenceSummary, substitutions, reference, simplificationPath)
    }
  }

  case class Elided(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def serializedLines = {
      "elided" +: inferenceApplication.serializedLines.indent
    }
  }
  object Elided {
    def parser(implicit parsingContext: ParsingContext): Parser[Elided] = {
      InferenceApplication.parser.map(Elided.apply)
    }
  }

  def parser(implicit parsingContext: ParsingContext): Parser[Option[Reference]] = {
    Parser.selectWord {
      case "direct" => Direct.parser
      case "expansion" => Expansion.parser
      case "simplification" => Simplification.parser
      case "elided" => Elided.parser
    }
  }
  def listParser(implicit parsingContext: ParsingContext): Parser[Seq[Reference]] = {
    parser.collectWhileDefined
  }

  def nextReference(baseReference: Option[Reference.Direct], suffix: String): Reference.Direct = {
    baseReference match {
      case Some(value) =>
        value.withSuffix("." + suffix)
      case None =>
        Reference.Direct(suffix)
    }
  }
}
