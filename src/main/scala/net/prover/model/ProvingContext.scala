package net.prover.model

import net.prover.model.definitions._
import net.prover.model.entries.ChapterEntry
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.StepProvingContext
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.util.Direction
import shapeless.{::, Generic, HList, HNil}

case class ProvingContext(entryContext: EntryContext, private val definitions: Definitions) {
  trait Allowable[-T] {
    def isAllowed(t: T): Boolean
  }
  trait AlwaysAllowable[-T] extends Allowable[T] {
    def isAllowed(t: T): Boolean = true
  }
  case class Filterable[T](replace: T => T)
  object Allowable {
    private val alwaysAllowable = new AlwaysAllowable[Any] {}
    private def allowable[T](f: T => Boolean) = new Allowable[T] {
      def isAllowed(t: T): Boolean = f(t)
    }

    implicit val allowableHNil: Allowable[HNil] = alwaysAllowable
    implicit def allowableHList[Head, Tail <: HList](implicit allowableHead: Allowable[Head], allowableTail: Allowable[Tail]): Allowable[Head :: Tail] = allowable { x =>
      isAllowed(x.head) && isAllowed(x.tail)
    }
    def allowableGeneric[T, Repr](generic: Generic.Aux[T, Repr])(implicit allowableRepr: Allowable[Repr]) : Allowable[T] = allowable { x =>
      isAllowed(generic.to(x))
    }

    implicit val alwaysAllowableStatement: AlwaysAllowable[Statement] = alwaysAllowable
    implicit val alwaysAllowableTerm: AlwaysAllowable[Term] = alwaysAllowable
    implicit val alwaysAllowableExpression: AlwaysAllowable[Expression] = alwaysAllowable
    implicit val alwaysAllowableString: AlwaysAllowable[String] = alwaysAllowable
    implicit val alwaysAllowableInt: AlwaysAllowable[Int] = alwaysAllowable
    implicit val alwaysAllowableDirection: AlwaysAllowable[Direction] = alwaysAllowable
    implicit val alwaysAllowablePossibleSubstitutions: AlwaysAllowable[Substitutions.Possible] = alwaysAllowable
    implicit def allowableSeq[T](implicit inner: Allowable[T]): Allowable[Seq[T]] = allowable { x => x.forall(isAllowed) }

    implicit val allowableInference: Allowable[Inference] = allowable(i => entryContext.allInferences.exists(_.id == i.id))
    implicit val allowableStatementDefinition: Allowable[StatementDefinition] = allowable(d => entryContext.statementDefinitionsBySymbol.contains(d.symbol))
    implicit val allowableTermDefinition: Allowable[TermDefinition] = allowable(d => entryContext.termDefinitionsBySymbol.contains(d.symbol))

    implicit val allowableRelation: Allowable[BinaryJoiner[_ <: Expression]] = allowable(definedBinaryJoiners.contains)
    implicit val allowableReversal: Allowable[Reversal[_ <: Expression]] = allowable(r => isAllowed(r.joiner) && isAllowed(r.inference))
    implicit val allowableTransitivity: Allowable[Transitivity[_ <: Expression]] = allowable(r => isAllowed(r.firstPremiseJoiner) && isAllowed(r.secondPremiseJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableExpansion: Allowable[Expansion[_ <: Expression]] = allowable(r => isAllowed(r.sourceJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableSubstitution: Allowable[Substitution] = allowableGeneric(Generic[Substitution])
    implicit val allowableExtractionOption: Allowable[ExtractionOption] = allowableGeneric(Generic[ExtractionOption])
    implicit def allowableDesimplifiedPremise: Allowable[DesimplifiedPremise] = allowableGeneric(Generic[DesimplifiedPremise])
    implicit def allowablePremiseDesimplification: Allowable[PremiseDesimplification] = allowable {
      case DirectPremise(_) => true
      case desimplifiedPremise: DesimplifiedPremise => allowableDesimplifiedPremise.isAllowed(desimplifiedPremise)
    }

    implicit val alwaysAllowableOperator: Allowable[BinaryOperator] = alwaysAllowable
    implicit val allowableEquality: Allowable[Equality] = allowableGeneric(Generic[Equality])
    implicit val allowableCommutativity: Allowable[Commutativity] = allowableGeneric(Generic[Commutativity])
    implicit val allowableAssociativity: Allowable[Associativity] = allowableGeneric(Generic[Associativity])
    implicit val allowablePremiseRelationSimplificationInference: Allowable[PremiseRelationSimplificationInference] = allowableGeneric(Generic[PremiseRelationSimplificationInference])
    implicit val allowablePremiseRelationRewriteInference: Allowable[PremiseRelationRewriteInference] = allowableGeneric(Generic[PremiseRelationRewriteInference])
    implicit val allowableConclusionRelationSimplificationInference: Allowable[ConclusionRelationSimplificationInference] = allowableGeneric(Generic[ConclusionRelationSimplificationInference])

    implicit val allowableTermRewriteInference: Allowable[TermRewriteInference] = allowableGeneric(Generic[TermRewriteInference])

    implicit def allowableTuple2[A, B](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B]
    ): Allowable[(A, B)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2))
    }
    implicit def allowableTuple3[A, B, C](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B],
      allowableC: Allowable[C]
    ): Allowable[(A, B, C)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2) &&  allowableC.isAllowed(t._3))
    }
    implicit def allowableTuple4[A, B, C, D](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B],
      allowableC: Allowable[C],
      allowableD: Allowable[D]
    ): Allowable[(A, B, C, D)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2) &&  allowableC.isAllowed(t._3)  &&  allowableD.isAllowed(t._4))
    }
    implicit def allowableTuple6[A, B, C, D, E, F](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B],
      allowableC: Allowable[C],
      allowableD: Allowable[D],
      allowableE: Allowable[E],
      allowableF: Allowable[F]
    ): Allowable[(A, B, C, D, E, F)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2) &&  allowableC.isAllowed(t._3)  &&  allowableD.isAllowed(t._4) && allowableE.isAllowed(t._5) && allowableF.isAllowed(t._6))
    }

    implicit def allowableOption[A](implicit allowableA: Allowable[A]): Allowable[Option[A]] = {
      allowable(o => o.forall(allowableA.isAllowed))
    }
  }

  trait LowPriorityFilterable {
    implicit def filterableSeqFromAllowable[T](implicit allowable: Allowable[T]): Filterable[Seq[T]] = Filterable[Seq[T]](_.filter(allowable.isAllowed))
    implicit def filterableOptionFromAllowable[T](implicit allowable: Allowable[T]): Filterable[Option[T]] = Filterable[Option[T]](_.filter(allowable.isAllowed))
    implicit def filterableMapFromAllowable[T, S](implicit allowableT: Allowable[T], allowableS: Allowable[S]): Filterable[Map[T, S]] = {
      Filterable[Map[T, S]](m => m.filter { case (t, s) => allowableT.isAllowed(t) && allowableS.isAllowed(s) })
    }
  }

  object Filterable extends LowPriorityFilterable {
    implicit def filterableSeqFromFilterable[T](implicit filterable: Filterable[T]): Filterable[Seq[T]] = Filterable[Seq[T]](_.map(filterable.replace))
    implicit def filterableOptionFromFilterable[T](implicit filterable: Filterable[T]): Filterable[Option[T]] = Filterable[Option[T]](_.map(filterable.replace))
    implicit def filterableMapFromFilterable[T, S](implicit allowableT: Allowable[T], filterableS: Filterable[S]): Filterable[Map[T, S]] = {
      Filterable[Map[T, S]](m => m.filterKeys(allowableT.isAllowed).mapValues(filterableS.replace))
    }
  }

  def isAllowed[T](t: T)(implicit allowable: Allowable[T]): Boolean = {
    allowable.isAllowed(t)
  }
  def filter[T](t: T)(implicit replacable: Filterable[T]): T = replacable.replace(t)

  lazy val extractionOptionsByInferenceId: Map[String, Seq[ExtractionOption]] = filter(definitions.extractionOptionsByInferenceId)

  lazy val deductionDefinitionOption: Option[StatementDefinition] = entryContext.deductionDefinitionOption
  lazy val deductionEliminationInferenceOption: Option[(Inference, Statement, Statement)] = {
    filter(definitions.deductionEliminationInferenceOption)
  }

  lazy val generalizationDefinitionOption: Option[StatementDefinition] = entryContext.generalizationDefinitionOption
  lazy val specificationInferenceOption: Option[(Inference, Statement, String, String)] = {
    filter(definitions.specificationInferenceOption)
  }

  lazy val definedBinaryJoiners: Seq[BinaryJoiner[_ <: Expression]] = {
    Definitions.getDefinedBinaryStatements(
      entryContext.statementDefinitions,
      entryContext.displayShorthands,
      entryContext.termDefinitions)
  }
  lazy val definedBinaryConnectives: Seq[BinaryConnective] = definedBinaryJoiners.ofType[BinaryConnective]
  lazy val definedBinaryRelations: Seq[BinaryRelation] = definedBinaryJoiners.ofType[BinaryRelation]

  lazy val reversals: Seq[Reversal[_ <: Expression]] = filter(definitions.reversals)
  lazy val transitivities: Seq[Transitivity[_ <: Expression]] = filter(definitions.transitivities)
  lazy val expansions: Seq[Expansion[_ <: Expression]] = filter(definitions.expansions)
  lazy val substitutions: Seq[Substitution] = filter(definitions.substitutions)

  lazy val equalityOption: Option[Equality] = {
    filter(definitions.equalityOption)
  }
  lazy val generalizationDistributions: Map[BinaryJoiner[Statement], Inference] = filter(definitions.generalizationDistributions)
  lazy val deductionDistributions: Map[BinaryJoiner[Statement], Inference] = filter(definitions.deductionDistributions)

  lazy val rearrangeableFunctions: Seq[(BinaryOperator, Commutativity, Associativity)] = {
    filter(definitions.rearrangeableFunctions)
  }

  lazy val premiseRelationSimplificationInferences: Seq[PremiseRelationSimplificationInference] = filter(definitions.premiseRelationSimplificationInferences)
  lazy val premiseRelationRewriteInferences: Seq[PremiseRelationRewriteInference] = filter(definitions.premiseRelationRewriteInferences)
  lazy val conclusionRelationSimplificationInferences: Seq[ConclusionRelationSimplificationInference] = filter(definitions.conclusionRelationSimplificationInferences)
  lazy val conclusionSimplificationInferences: Seq[Inference] = filter(definitions.conclusionSimplificationInferences)
  lazy val termDefinitionRemovals: Map[TermDefinition, Seq[ExtractionOption]] = filter(definitions.termDefinitionRemovals)

  lazy val rewriteInferences: Seq[(Inference, Statement)] = {
    filter(definitions.rewriteInferences)
  }

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Option[Statement])] = {
    filter(definitions.statementExtractionInferences)
  }

  lazy val termRewriteInferences: Seq[TermRewriteInference] = {
    filter(definitions.termRewriteInferences)
  }
  lazy val termSimplificationInferences: Seq[TermRewriteInference] = {
    filter(definitions.termSimplificationInferences)
  }
  lazy val termDesimplificationInferences: Seq[TermRewriteInference] = {
    filter(definitions.termDesimplificationInferences)
  }
  lazy val statementDefinitionDeconstructions: Seq[Inference] = {
    filter(definitions.statementDefinitionDeconstructions)
  }
  lazy val structuralSimplificationInferences: Seq[(Inference, Statement)] = {
    filter(definitions.structuralSimplificationInferences)
  }
  lazy val facts: Seq[(Statement, Inference, ExtractionOption)] = {
    filter(definitions.facts)
  }
  lazy val statementDeductionInferences: Seq[(Inference, Statement, Statement, String, String, Direction)] = {
    filter(definitions.statementDeductionInferences)
  }
  lazy val statementDefinitionIntroductionInferences: Seq[(Inference, Statement)] = {
    filter(definitions.statementDefinitionIntroductionInferences)
  }
  lazy val statementDefinitionEliminationInferences: Seq[(Inference, Statement)] = {
    filter(definitions.statementDefinitionEliminationInferences)
  }
}

object ProvingContext {
  def forEntry(allBooks: Seq[Book], definitions: Definitions, book: Book, chapter: Chapter, entry: ChapterEntry): ProvingContext = {
    ProvingContext(EntryContext.forEntry(allBooks, book, chapter, entry), definitions)
  }
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): ProvingContext = stepProvingContext.provingContext
}
