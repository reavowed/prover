package net.prover.model.proof

import net.prover.model._

sealed trait Reference {
  def lineReferences: Set[(String, Seq[Int])]
  def referencedInferenceIds: Set[String]
  def serialized: String
}

object Reference {
  sealed trait Compoundable extends Reference {
    def add(other: Direct) = Compound(other, this)
  }
  sealed trait ToSingleLine extends Reference {
    def lineReference: (String, Seq[Int])
    override def lineReferences = Set(lineReference)
  }

  case class Direct(value: String) extends Compoundable with ToSingleLine {
    override def referencedInferenceIds = Set.empty
    override def lineReference = (value, Seq.empty[Int])
    override def lineReferences = Set(lineReference)
    override def serialized = s"direct $value"
    def withSuffix(suffix: String): Direct = Direct(value + suffix)
    def getChildForAssumption = withSuffix("a")
    def getChildForDeduction = withSuffix("d")
    def getChildForResult = withSuffix("r")
    def getChildForSubstep(index: Int) = withSuffix(s".$index")
  }
  object Direct {
    def parser(implicit context: ParsingContext): Parser[Direct] = {
      Parser.singleWord.map(Direct.apply)
    }
  }

  case class Compound(first: Direct, other: Compoundable) extends Compoundable {
    override def lineReferences = other.lineReferences + first.lineReference
    override def referencedInferenceIds = other.referencedInferenceIds
    override def serialized = s"compound ${first.value} ${other.serialized}"
  }
  object Compound {
    def parser(implicit context: ParsingContext): Parser[Compound] = {
      for {
        firstValue <- Parser.singleWord
        other <- Parser.selectWordParser("compound reference") {
            case "direct" => Direct.parser
            case "compound" => Compound.parser
          }
      } yield Compound(Direct(firstValue), other)
    }
  }

  sealed trait ApplyingInference extends Reference {
    def inferenceApplication: InferenceApplication
    override def lineReferences = inferenceApplication.lineReferences
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
  }

  case class Expansion(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def serialized = s"expansion ${inferenceApplication.serialized}"
  }
  object Expansion {
    def parser(implicit context: ParsingContext): Parser[Expansion] = {
      InferenceApplication.parser.map(Expansion.apply)
    }
  }

  case class Simplification(
      inference: Inference,
      substitutions: Substitutions,
      inferenceReference: Reference.ToSingleLine,
      simplificationPath: Seq[Int])
    extends ToSingleLine
  {
    override def referencedInferenceIds = inferenceReference.referencedInferenceIds
    override def lineReference = inferenceReference.lineReference.mapRight(_ ++ simplificationPath)
    override def serialized = Seq(
      "simplification",
      inference.id,
      inference.serializeSubstitutions(substitutions),
      inferenceReference.serialized,
      "(" + simplificationPath.mkString(" ") + ")"
    ).mkString(" ")
  }
  object Simplification {
    def parser(implicit context: ParsingContext): Parser[Simplification] = {
      for {
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        reference <- Reference.parser.map(_.asInstanceOf[ToSingleLine])
        simplificationPath <- Parser.int.listInParens(None)
      } yield Simplification(inference, substitutions, reference, simplificationPath)
    }
  }

  case class Elided(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def serialized = s"elided ${inferenceApplication.serialized}"
  }
  object Elided {
    def parser(implicit context: ParsingContext): Parser[Elided] = {
      InferenceApplication.parser.map(Elided.apply)
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

  def parser(implicit context: ParsingContext): Parser[Reference] = {
    Parser.selectWordParser("reference") {
      case "direct" => Direct.parser
      case "compound" => Compound.parser
      case "expansion" => Expansion.parser
      case "simplification" => Simplification.parser
      case "elided" => Elided.parser
    }
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Reference]] = parser.listInParens(Some(","))

  implicit class ReferenceSeqOps(references: Seq[Reference]) {
    def serialized: String = "(" + references.map(_.serialized).mkString(", ") + ")"
  }
}
