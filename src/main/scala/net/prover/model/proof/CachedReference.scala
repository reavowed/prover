package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement

sealed trait CachedReference {
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
  def validate(provingContext: ProvingContext): Option[(Reference, Statement)]
  def validate(
    premiseStatement: Statement,
    context: ProvingContext
  ): Option[Reference] = {
    for {
      (validatedReference, referencedFact) <- validate(context)
      _ = if (referencedFact != premiseStatement)
        CachedProof.logger.info(s"Reference '$serialized' was to '${referencedFact.serialized}', not '${premiseStatement.serialized}'")
      if referencedFact == premiseStatement
    } yield validatedReference
  }
  def serializedLines: Seq[String]
  def serialized: String = serializedLines.mkString("\n")
}

object CachedReference {
  sealed trait ToSingleLine extends CachedReference {
    def validate(provingContext: ProvingContext): Option[(Reference.ToSingleLine, Statement)]
  }
  sealed trait Compoundable extends CachedReference {
    def unvalidated: Reference.Compoundable
  }

  case class Direct(value: String) extends Compoundable with ToSingleLine {
    override def unvalidated = Reference.Direct(value)
    override def validate(context: ProvingContext) = {
      context.referencedFacts
        .find(_.reference == unvalidated)
        .map(_.statement)
        .ifEmpty {
          CachedProof.logger.info(s"Reference '$serialized' did not exist")
        }
        .map(unvalidated -> _)
    }
    override def getAssertionHints(availableInferences: Seq[Inference]) = Nil
    override def serializedLines: Seq[String] = Seq(s"direct $value")
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      Parser.singleWord.map(Direct.apply)
    }
  }

  case class Compound(first: Direct, other: Compoundable) extends Compoundable {
    override def getAssertionHints(availableInferences: Seq[Inference]) = {
      first.getAssertionHints(availableInferences) ++ other.getAssertionHints(availableInferences)
    }
    override def unvalidated = Reference.Compound(first.unvalidated, other.unvalidated)
    override def validate(context: ProvingContext) = {
      context.referencedFacts
        .find(_.reference == unvalidated)
        .map(_.statement)
        .ifEmpty {
          CachedProof.logger.info(s"Reference '$serialized' did not exist")
        }
        .map(unvalidated -> _)
    }
    override def serializedLines: Seq[String] = Seq(s"compound ${first.serialized} ${other.serialized}")
  }
  object Compound {
    def parser(implicit parsingContext: ParsingContext): Parser[Compound] = {
      for {
        first <- CachedReference.parser
        other <- CachedReference.parser
      } yield Compound(first.asInstanceOf[Direct], other.asInstanceOf[Compoundable])
    }
  }

  case class Expansion(cachedInferenceApplication: CachedInferenceApplication) extends CachedReference {
    override def validate(context: ProvingContext): Option[(Reference, Statement)] = {
      for {
        (statement, validatedApplication) <- cachedInferenceApplication.validate(context)
      } yield (Reference.Expansion(validatedApplication), statement)
    }
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }
    override def serializedLines = {
      "expansion" +: cachedInferenceApplication.serializedLines.indent
    }
  }
  object Expansion {
    def parser(implicit parsingContext: ParsingContext): Parser[Expansion] = {
      CachedInferenceApplication.parser.map(Expansion.apply)
    }
  }

  case class Simplification(
      inferenceId: String,
      inferenceSubstitutions: Inference.Substitutions,
      inferenceReference: CachedReference.ToSingleLine,
      simplificationPath: Seq[Int],
      depth: Int)
    extends ToSingleLine
  {
    private def cachedInferenceApplication = CachedInferenceApplication.Direct(inferenceId, inferenceSubstitutions, Seq(inferenceReference), depth)
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }
    override def validate(context: ProvingContext) = {
      for {
        (conclusion, inferenceApplication) <- cachedInferenceApplication.validate(context)
        premise <- inferenceApplication.inference.premises.single.map(_.statement)
        reference <- inferenceApplication.references.single.flatMap(_.asOptionalInstanceOf[Reference.ToSingleLine])
        substitutedPremise <- premise.applySubstitutions(inferenceApplication.substitutions)
        validatedSimplificationPath <- substitutedPremise.findComponentPath(conclusion)
      } yield (
        Reference.Simplification(
          inferenceApplication.inference,
          inferenceApplication.substitutions,
          reference,
          validatedSimplificationPath,
          depth),
        conclusion)
    }
    override def serializedLines = {
      Seq(s"simplification $inferenceId ${inferenceSubstitutions.serialized}") ++
        (inferenceReference.serializedLines :+ s"(${simplificationPath.mkString(" ")})").indent
    }
  }
  object Simplification {
    def parser(implicit parsingContext: ParsingContext): Parser[Simplification] = {
      for {
        inferenceId <- Parser.singleWord
        substitutions <- Inference.Substitutions.parser
        reference <- CachedReference.parser.map(_.asInstanceOf[CachedReference.ToSingleLine])
        simplificationPath <- Parser.int.listInParens(None)
      } yield Simplification(inferenceId, substitutions, reference, simplificationPath, parsingContext.parameterDepth)
    }
  }

  case class Elided(cachedInferenceApplication: CachedInferenceApplication) extends CachedReference {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }
    override def validate(context: ProvingContext): Option[(Reference, Statement)] = {
      for {
        (statement, validatedApplication) <- cachedInferenceApplication.validate(context)
      } yield (Reference.Elided(validatedApplication), statement)
    }
    override def serializedLines = {
      "elided" +: cachedInferenceApplication.serializedLines.indent
    }
  }
  object Elided {
    def parser(implicit parsingContext: ParsingContext): Parser[Elided] = {
      CachedInferenceApplication.parser.map(Elided.apply)
    }
  }


  def parser(implicit parsingContext: ParsingContext): Parser[CachedReference] = {
    Parser.selectWordParser("reference") {
      case "direct" => Direct.parser
      case "compound" => Compound.parser
      case "expansion" => Expansion.parser
      case "simplification" => Simplification.parser
      case "elided" => Elided.parser
    }
  }

  implicit class CachedReferenceSeqOps(cachedReferences: Seq[CachedReference]) {
    def validate(premiseStatements: Seq[Statement], provingContext: ProvingContext): Option[Seq[Reference]] = {
      for {
        premiseFactsWithCachedReferences <- premiseStatements.zipStrict(cachedReferences)
        validatedReferences <- premiseFactsWithCachedReferences.map { case (premiseFact, cachedReference) =>
          cachedReference.validate(premiseFact, provingContext)
        }.traverseOption
      } yield validatedReferences
    }
  }
}