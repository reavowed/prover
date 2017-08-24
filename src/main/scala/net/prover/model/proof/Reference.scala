package net.prover.model.proof

import net.prover.model._

sealed trait Reference {
  def serialized: String = serializedLines.mkString("\n")
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
}

object Reference {
  case class Direct(value: String) extends Reference {
    def serializedLines: Seq[String] = Seq(s"direct $value")
    override val referencedInferenceIds: Set[String] = Set.empty
    override def getAssertionHints(availableInferences: Seq[Inference]) = Nil
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      Parser.singleWord.map(Direct.apply)
    }
  }

  sealed trait ApplyingInference extends Reference {
    def inferenceApplication: InferenceApplication
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      inferenceApplication.getAssertionHints(availableInferences)
    }
  }
  sealed trait Rearrangement extends ApplyingInference

  case class Expansion(inferenceApplication: InferenceApplication) extends Rearrangement {
    override def serializedLines = {
      "expansion" +: inferenceApplication.serializedLines.indent
    }
  }
  object Expansion {
    def parser(implicit parsingContext: ParsingContext): Parser[Expansion] = {
      InferenceApplication.parser.map(Expansion.apply)
    }
  }

  case class Simplification(inferenceApplication: InferenceApplication) extends Rearrangement {
    override def serializedLines = {
      "simplification" +: inferenceApplication.serializedLines.indent
    }
  }
  object Simplification {
    def parser(implicit parsingContext: ParsingContext): Parser[Simplification] = {
      InferenceApplication.parser.map(Simplification.apply)
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

  def nextReference(baseReference: Option[Reference.Direct], i: Int): Reference.Direct = {
    baseReference match {
      case Some(Reference.Direct(value)) =>
        Reference.Direct(s"$value.$i")
      case None =>
        Reference.Direct(i.toString)
    }
  }
}
