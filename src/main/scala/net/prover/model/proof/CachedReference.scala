package net.prover.model.proof

import net.prover.model._

sealed trait CachedReference {
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint]
  def validate(provingContext: ProvingContext): Option[(Reference, Fact)]
  def validate(
    premiseFact: Fact,
    context: ProvingContext
  ): Option[Reference] = {
    for {
      (validatedReference, referencedFact) <- validate(context)
      _ = if (referencedFact != premiseFact)
        CachedProof.logger.info(s"Reference '$serialized' was to '${referencedFact.serialized}', not '${premiseFact.serialized}'")
      if referencedFact == premiseFact
    } yield validatedReference
  }
  def serializedLines: Seq[String]
  def serialized: String = serializedLines.mkString("\n")
}

object CachedReference {
  sealed trait ToFact extends CachedReference {
    def validate(provingContext: ProvingContext): Option[(Reference.ToFact, Fact)]
  }
  case class Direct(value: String) extends ToFact {
    override def getAssertionHints(availableInferences: Seq[Inference]) = Nil
    override def validate(context: ProvingContext) = {
      context.referencedFacts
        .find(_.reference == Reference.Direct(value))
        .map(_.fact)
        .ifEmpty {
          CachedProof.logger.info(s"Direct reference '$value' did not exist")
        }
        .map(Reference.Direct(value) -> _)
    }
    override def serializedLines: Seq[String] = Seq(s"direct $value")
  }
  object Direct {
    def parser(implicit parsingContext: ParsingContext): Parser[Direct] = {
      Parser.singleWord.map(Direct.apply)
    }
  }

  case class Expansion(cachedInferenceApplication: CachedInferenceApplication) extends CachedReference {
    override def validate(context: ProvingContext): Option[(Reference, Fact)] = {
      for {
        (statement, validatedApplication) <- cachedInferenceApplication.validate(context)
      } yield (Reference.Expansion(validatedApplication), Fact.Direct(statement))
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
  case class Contraction(
      inferenceId: String,
      inferenceSubstitutions: Inference.Substitutions,
      inferenceReference: CachedReference.ToFact,
      level: Int,
      additionalDepth: Int,
      depth: Int)
    extends ToFact
  {
    private def cachedInferenceApplication = CachedInferenceApplication.Direct(inferenceId, inferenceSubstitutions, Seq(inferenceReference), depth)
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }

    override def validate(context: ProvingContext) = {
      for {
        inference <- context.availableInferences.find(_.id == inferenceId).ifEmpty {
          CachedProof.logger.info(s"Could not find inference $inferenceId")
        }
        substitutions <- inference.generalizeSubstitutions(inferenceSubstitutions, depth)
        premise <- inference.premises.single
        substitutedPremiseFact <- premise.fact.applySubstitutions(substitutions).ifEmpty {
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
        (validatedReference, referencedFact) <- inferenceReference.validate(context)
        (referencedChildFact, _, referencedFactUpdater) <- referencedFact.iteratedChildDetails(level)
        _ = if (referencedChildFact != substitutedPremiseFact)
          CachedProof.logger.info(s"Reference '$serialized' at level '$level' was to '${referencedFact.serialized}'," +
            s" not '${substitutedPremiseFact.serialized}'")
        if referencedChildFact == substitutedPremiseFact
      } yield {
        (Reference.Contraction(
          inference,
          substitutions,
          validatedReference,
          level,
          additionalDepth,
          depth),
          referencedFactUpdater(Fact.Direct(substitutedConclusion)))
      }
    }
    override def serializedLines = {
      Seq(s"contraction $level $additionalDepth $inferenceId ${inferenceSubstitutions.serialized}") ++ inferenceReference.serializedLines.indent
    }
  }
  object Contraction {
    def parser(implicit parsingContext: ParsingContext): Parser[Contraction] = {
      for {
        level <- Parser.int
        additionalDepth <- Parser.int
        updatedContext = (1 to additionalDepth).foldLeft(parsingContext){ case (c, _) => c.addParameterList(Nil) }
        inferenceId <- Parser.singleWord
        substitutions <- Inference.Substitutions.parser(updatedContext)
        reference <- CachedReference.parser(updatedContext).map(_.asInstanceOf[CachedReference.ToFact])
      } yield Contraction(inferenceId, substitutions, reference, level, additionalDepth, updatedContext.parameterDepth)
    }
  }

  case class Simplification(
      inferenceId: String,
      inferenceSubstitutions: Inference.Substitutions,
      inferenceReference: CachedReference.ToFact,
      simplificationPath: Seq[Int],
      depth: Int)
    extends ToFact
  {
    private def cachedInferenceApplication = CachedInferenceApplication.Direct(inferenceId, inferenceSubstitutions, Seq(inferenceReference), depth)
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }

    override def validate(context: ProvingContext) = {
      for {
        (conclusion, inferenceApplication) <- cachedInferenceApplication.validate(context)
        premise <- inferenceApplication.inference.premises.single.flatMap(_.fact.asOptionalInstanceOf[Fact.Direct]).map(_.assertion)
        reference <- inferenceApplication.references.single.flatMap(_.asOptionalInstanceOf[Reference.ToFact])
        substitutedPremise <- premise.applySubstitutions(inferenceApplication.substitutions)
        validatedSimplificationPath <- substitutedPremise.findComponentPath(conclusion)
      } yield (
        Reference.Simplification(
          inferenceApplication.inference,
          inferenceApplication.substitutions,
          reference,
          validatedSimplificationPath,
          depth),
        Fact.Direct(conclusion))
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
        reference <- CachedReference.parser.map(_.asInstanceOf[CachedReference.ToFact])
        simplificationPath <- Parser.int.listInParens(None)
      } yield Simplification(inferenceId, substitutions, reference, simplificationPath, parsingContext.parameterDepth)
    }
  }

  case class Elided(cachedInferenceApplication: CachedInferenceApplication) extends CachedReference {
    override def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
      cachedInferenceApplication.getAssertionHints(availableInferences)
    }
    override def validate(context: ProvingContext): Option[(Reference, Fact)] = {
      for {
        (statement, validatedApplication) <- cachedInferenceApplication.validate(context)
      } yield (Reference.Elided(validatedApplication), Fact.Direct(statement))
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
      case "expansion" => Expansion.parser
      case "simplification" => Simplification.parser
      case "contraction" => Contraction.parser
      case "elided" => Elided.parser
    }
  }

  implicit class CachedReferenceSeqOps(cachedReferences: Seq[CachedReference]) {
    def validate(premiseFacts: Seq[Fact], provingContext: ProvingContext): Option[Seq[Reference]] = {
      for {
        premiseFactsWithCachedReferences <- premiseFacts.zipStrict(cachedReferences)
        validatedReferences <- premiseFactsWithCachedReferences.map { case (premiseFact, cachedReference) =>
          cachedReference.validate(premiseFact, provingContext)
        }.traverseOption
      } yield validatedReferences
    }
  }
}