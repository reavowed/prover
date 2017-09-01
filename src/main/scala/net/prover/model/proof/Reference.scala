package net.prover.model.proof

import net.prover.model._

sealed trait Reference {
  def serialized: String = serializedLines.mkString("\n")
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def factReferences: Set[Reference.ToFact] = Set.empty
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
}

object Reference {
  sealed trait ToFact extends Reference {
    override def factReferences: Set[Reference.ToFact] = Set(this)
    def valueAndPath: (String, Seq[Int])
  }

  case class Direct(value: String) extends ToFact {
    def serializedLines: Seq[String] = Seq(s"direct $value")
    override val referencedInferenceIds: Set[String] = Set.empty
    override def getAssertionHints(availableInferences: Seq[Inference]) = Nil
    def withSuffix(suffix: String): Direct = Direct(value + suffix)
    override def valueAndPath: (String, Seq[Int]) = (value, Nil)
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      Parser.singleWord.map(Direct.apply)
    }
  }

  sealed trait ApplyingInference extends Reference {
    def inferenceApplication: InferenceApplication
    override def factReferences: Set[Reference.ToFact] = inferenceApplication.directReferences
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
      inferenceReference: Reference.ToFact,
      simplificationPath: Seq[Int])
    extends ToFact
  {
    override def referencedInferenceIds: Set[String] = inferenceReference.referencedInferenceIds
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      InferenceApplication(inferenceSummary, inferenceSubstitutions, Seq(inferenceReference)).getAssertionHints(availableInferences)
    }
    override def serializedLines = {
      Seq("simplification", inferenceSummary.serialized, inferenceSubstitutions.serialized).mkString(" ") +:
        (inferenceReference.serializedLines :+ "(" + simplificationPath.mkString(" ") + ")").indent
    }
    override def valueAndPath: (String, Seq[Int]) = inferenceReference.valueAndPath.mapRight(simplificationPath ++ _)
  }
  object Simplification {
    def parser(implicit parsingContext: ParsingContext): Parser[Simplification] = {
      for {
        inferenceSummary <- Inference.Summary.parser
        substitutions <- Inference.Substitutions.parser
        reference <- Reference.parser.map(_.asInstanceOf[Reference.ToFact])
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

  def parser(implicit parsingContext: ParsingContext): Parser[Reference] = {
    Parser.selectWord("reference") {
      case "direct" => Direct.parser
      case "expansion" => Expansion.parser
      case "simplification" => Simplification.parser
      case "elided" => Elided.parser
    }
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
