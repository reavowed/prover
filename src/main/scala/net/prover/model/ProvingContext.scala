package net.prover.model

import net.prover.model.definitions._
import net.prover.model.entries.{ChapterEntry, StatementDefinition, TermDefinition}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.StepProvingContext
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
    implicit val alwaysAllowableDirection: AlwaysAllowable[Direction] = alwaysAllowable
    implicit def allowableSeq[T](implicit inner: Allowable[T]): Allowable[Seq[T]] = allowable { x => x.forall(isAllowed) }

    implicit val allowableInference: Allowable[Inference] = allowable(i => entryContext.inferences.exists(_.id == i.id))
    implicit val allowableStatementDefinition: Allowable[StatementDefinition] = allowable(entryContext.statementDefinitions.contains)
    implicit val allowableTermDefinition: Allowable[TermDefinition] = allowable(entryContext.termDefinitions.contains)

    implicit val allowableRelation: Allowable[BinaryJoiner[_ <: Expression]] = allowable(definedBinaryStatements.contains)
    implicit val allowableReversal: Allowable[Reversal[_ <: Expression]] = allowable(r => isAllowed(r.relation) && isAllowed(r.inference))
    implicit val allowableTransitivity: Allowable[Transitivity[_ <: Expression]] = allowable(r => isAllowed(r.statement) && isAllowed(r.inference))
    implicit val allowableExpansion: Allowable[Expansion[_ <: Expression]] = allowable(r => isAllowed(r.sourceJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableSubstitution: Allowable[Substitution] = allowableGeneric(Generic[Substitution])

    implicit val alwaysAllowableOperator: Allowable[BinaryOperator] = alwaysAllowable
    implicit val allowableEquality: Allowable[Equality] = allowableGeneric(Generic[Equality])
    implicit val allowableCommutativity: Allowable[Commutativity] = allowableGeneric(Generic[Commutativity])
    implicit val allowableAssociativity: Allowable[Associativity] = allowableGeneric(Generic[Associativity])
    implicit val allowablePremiseRelationLeftHandSimplificationInference: Allowable[PremiseRelationLeftHandSimplificationInference] = allowableGeneric(Generic[PremiseRelationLeftHandSimplificationInference])
    implicit val allowablePremiseRelationDoubleSimplificationInference: Allowable[PremiseRelationDoubleSimplificationInference] = allowableGeneric(Generic[PremiseRelationDoubleSimplificationInference])
    implicit val allowableConclusionRelationDoubleSimplificationInference: Allowable[ConclusionRelationDoubleSimplificationInference] = allowableGeneric(Generic[ConclusionRelationDoubleSimplificationInference])

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

  def isAllowed[T](t: T)(implicit allowable: Allowable[T]): Boolean = allowable.isAllowed(t)
  def filter[T](t: T)(implicit replacable: Filterable[T]): T = replacable.replace(t)

  lazy val deductionDefinitionOption: Option[StatementDefinition] = filter(definitions.deductionDefinitionOption)
  lazy val deductionEliminationInferenceOption: Option[(Inference, Statement, Statement)] = {
    filter(definitions.deductionEliminationInferenceOption)
  }

  lazy val scopingDefinitionOption: Option[StatementDefinition] = filter(definitions.scopingDefinitionOption)
  lazy val specificationInferenceOption: Option[(Inference, Statement, String, String)] = {
    filter(definitions.specificationInferenceOption)
  }

  lazy val definedBinaryStatements: Seq[BinaryJoiner[_ <: Expression]] = {
    Definitions.getDefinedBinaryStatements(
      entryContext.statementDefinitions,
      entryContext.displayShorthands,
      entryContext.termDefinitions)
  }
  lazy val definedBinaryConnectives: Seq[BinaryConnective] = definedBinaryStatements.ofType[BinaryConnective]
  lazy val definedBinaryRelations: Seq[BinaryRelation] = definedBinaryStatements.ofType[BinaryRelation]

  lazy val reversals: Seq[Reversal[_ <: Expression]] = filter(definitions.reversals)
  lazy val transitivities: Seq[Transitivity[_ <: Expression]] = filter(definitions.transitivities)
  lazy val expansions: Seq[Expansion[_ <: Expression]] = filter(definitions.expansions)
  lazy val substitutions: Seq[Substitution] = filter(definitions.substitutions)

  lazy val equalityOption: Option[Equality] = {
    filter(definitions.equalityOption)
  }

  lazy val rearrangeableFunctions: Seq[(BinaryOperator, Commutativity, Associativity)] = {
    filter(definitions.rearrangeableFunctions)
  }

  lazy val premiseRelationLeftHandSimplificationInferences: Seq[PremiseRelationLeftHandSimplificationInference] = filter(definitions.premiseRelationLeftHandSimplificationInferences)
  lazy val premiseRelationDoubleSimplificationInferences: Seq[PremiseRelationDoubleSimplificationInference] = filter(definitions.premiseRelationDoubleSimplificationInferences)
  lazy val conclusionRelationDoubleSimplificationInferences: Seq[ConclusionRelationDoubleSimplificationInference] = filter(definitions.conclusionRelationDoubleSimplificationInferences)

  lazy val premiseSimplificationInferences: Seq[(Inference, Statement)] = {
    filter(definitions.premiseSimplificationInferences)
  }
  lazy val conclusionSimplificationInferences: Seq[Inference] = {
    filter(definitions.conclusionSimplificationInferences)
  }
  lazy val rewriteInferences: Seq[(Inference, Statement)] = {
    filter(definitions.rewriteInferences)
  }

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Option[Statement])] = {
    filter(definitions.statementExtractionInferences)
  }

  lazy val termRewriteInferences: Seq[(Inference, Term, Term)] = {
    filter(definitions.termRewriteInferences)
  }
  lazy val termSimplificationInferences: Seq[(Inference, Term, Term)] = {
    filter(definitions.termSimplificationInferences)
  }
  lazy val termDesimplificationInferences: Seq[(Inference, Term, Term)] = {
    filter(definitions.termDesimplificationInferences)
  }
  lazy val statementDefinitionSimplifications: Map[StatementDefinition, Seq[(Inference, Statement, Expression)]] = {
    filter(definitions.statementDefinitionSimplifications)
  }
  def getStatementDefinitionSimplifications(statementDefinition: StatementDefinition): Seq[(Inference, Statement, Expression)] = {
    statementDefinitionSimplifications.getOrElse(statementDefinition, Nil)
  }
  lazy val statementDefinitionDeconstructions: Seq[Inference] = {
    filter(definitions.statementDefinitionDeconstructions)
  }
  lazy val structuralSimplificationInferences: Seq[(Inference, Statement)] = {
    filter(definitions.structuralSimplificationInferences)
  }
  lazy val facts: Seq[Inference] = {
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
