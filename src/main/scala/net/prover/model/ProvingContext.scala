package net.prover.model

import net.prover.entries.StepsWithContext
import net.prover.model.definitions._
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof._
import net.prover.model.utils.ExpressionUtils.TypeLikeStatement
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.extraction.{AppliedInferenceExtraction, ExtractionDefinition, ExtractionDetails, InferenceExtraction, VariableTracker}
import net.prover.theorems.GetReferencedInferences
import net.prover.util.Direction
import shapeless.{::, Generic, HList, HNil}

case class ProvingContext(availableEntries: AvailableEntries, private val definitions: Definitions) {
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
    implicit val allowableTypeLikeStatement: AlwaysAllowable[TypeLikeStatement] = alwaysAllowable
    implicit val alwaysAllowableString: AlwaysAllowable[String] = alwaysAllowable
    implicit val alwaysAllowableInt: AlwaysAllowable[Int] = alwaysAllowable
    implicit val alwaysAllowableDirection: AlwaysAllowable[Direction] = alwaysAllowable
    implicit val alwaysAllowablePossibleSubstitutions: AlwaysAllowable[Substitutions.Possible] = alwaysAllowable
    implicit val alwaysAllowableVariableTracker: AlwaysAllowable[VariableTracker] = alwaysAllowable
    implicit def allowableSeq[T](implicit inner: Allowable[T]): Allowable[Seq[T]] = allowable { x => x.forall(isAllowed) }

    implicit val allowableInference: Allowable[Inference] = allowable(i => availableEntries.allInferenceIds.contains(i.id))
    implicit val allowableStatementDefinition: Allowable[StatementDefinition] = allowable(d => availableEntries.statementDefinitionsBySymbol.contains(d.symbol))
    implicit val allowableTermDefinition: Allowable[TermDefinition] = allowable(d => availableEntries.termDefinitionsBySymbol.contains(d.symbol))

    implicit val allowableSimpleDerivation: Allowable[SimpleDerivation] = allowable(derivation => derivation.inferences.forall(isAllowed))

    implicit val allowableRelation: Allowable[BinaryJoiner[_ <: Expression]] = allowable(definedBinaryJoiners.contains)
    implicit val allowableReversal: Allowable[Reversal[_ <: Expression]] = allowable(r => isAllowed(r.joiner) && isAllowed(r.inference))
    implicit val allowableTransitivity: Allowable[Transitivity[_ <: Expression]] = allowable(r => isAllowed(r.firstPremiseJoiner) && isAllowed(r.secondPremiseJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableExpansion: Allowable[Expansion[_ <: Expression]] = allowable(r => isAllowed(r.sourceJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableSubstitution: Allowable[Substitution] = allowableGeneric(Generic[Substitution])
    implicit val allowableStep: Allowable[Step] = allowable(step => GetReferencedInferences(step).forall(isAllowed))
    implicit val allowableKnownStatement: Allowable[KnownStatement] = allowableGeneric(Generic[KnownStatement])
    implicit def allowableDesimplifiedPremise: Allowable[DesimplifiedPremise] = allowableGeneric(Generic[DesimplifiedPremise])
    implicit def allowablePremiseDesimplification: Allowable[DerivedPremise] = allowable {
      case DirectPremise(_) => true
      case desimplifiedPremise: DesimplifiedPremise => allowableDesimplifiedPremise.isAllowed(desimplifiedPremise)
    }

    implicit val alwaysAllowableOperator: Allowable[Operator] = alwaysAllowable
    implicit val allowableEquality: Allowable[Equality] = allowableGeneric(Generic[Equality])

    implicit val allowableExtractionDefinition: Allowable[ExtractionDefinition] = allowableGeneric(Generic[ExtractionDefinition])
    implicit val allowableExtractionFromSinglePremise: Allowable[ExtractionDetails] = allowableGeneric(Generic[ExtractionDetails])
    implicit val allowableInferenceExtraction: Allowable[InferenceExtraction] = allowableGeneric(Generic[InferenceExtraction])
    implicit val allowableFact: Allowable[Fact] = allowableGeneric(Generic[Fact])
    implicit val allowableCommutativity: Allowable[Commutativity] = allowableGeneric(Generic[Commutativity])
    implicit val allowableAssociativity: Allowable[Associativity] = allowableGeneric(Generic[Associativity])
    implicit val allowableLeftIdentity: Allowable[LeftIdentity] = allowableGeneric(Generic[LeftIdentity])
    implicit val allowableRightIdentity: Allowable[RightIdentity] = allowableGeneric(Generic[RightIdentity])
    implicit val allowableDoubleSidedIdentity: Allowable[DoubleSidedIdentity] = allowableGeneric(Generic[DoubleSidedIdentity])
    implicit val allowableLeftAbsorber: Allowable[LeftAbsorber] = allowableGeneric(Generic[LeftAbsorber])
    implicit val allowableRightAbsorber: Allowable[RightAbsorber] = allowableGeneric(Generic[RightAbsorber])
    implicit val allowableLeftInverse: Allowable[LeftInverse] = allowableGeneric(Generic[LeftInverse])
    implicit val allowableRightInverse: Allowable[RightInverse] = allowableGeneric(Generic[RightInverse])
    implicit val allowableDoubleSidedInverse: Allowable[DoubleSidedInverse] = allowableGeneric(Generic[DoubleSidedInverse])

    implicit val allowableRearrangeableOperator: Allowable[RearrangeableOperator] = allowableGeneric(Generic[RearrangeableOperator])
    implicit val allowableLeftDistributivity: Allowable[LeftDistributivity] = allowableGeneric(Generic[LeftDistributivity])
    implicit val allowableRightDistributivity: Allowable[RightDistributivity] = allowableGeneric(Generic[RightDistributivity])
    implicit val allowableLeftOperatorExtraction: Allowable[LeftOperatorExtraction] = allowableGeneric(Generic[LeftOperatorExtraction])
    implicit val allowableRightOperatorExtraction: Allowable[RightOperatorExtraction] = allowableGeneric(Generic[RightOperatorExtraction])

    implicit val allowablePremiseRelationSimplificationInference: Allowable[PremiseRelationSimplificationInference] = allowableGeneric(Generic[PremiseRelationSimplificationInference])
    implicit val allowablePremiseRelationRewriteInference: Allowable[RelationRewriteInference] = allowableGeneric(Generic[RelationRewriteInference])
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
      Filterable[Map[T, S]](m => m.view.filterKeys(allowableT.isAllowed).mapValues(filterableS.replace).toMap)
    }
  }

  def isAllowed[T](t: T)(implicit allowable: Allowable[T]): Boolean = {
    allowable.isAllowed(t)
  }
  def filter[T](t: T)(implicit replacable: Filterable[T]): T = replacable.replace(t)

  lazy val inferenceExtractionsByInferenceId: Map[String, Seq[InferenceExtraction]] = filter(definitions.inferenceExtractionsByInferenceId)

  def findInferenceExtraction(baseInferenceId: String, extractionDefinition: ExtractionDefinition.Serialized): Option[InferenceExtraction] = {
    for {
      extractions <- inferenceExtractionsByInferenceId.get(baseInferenceId)
      matchingExtraction <- extractions.find(_.extractionDetails.extractionDefinition.matches(extractionDefinition))
    } yield matchingExtraction
  }

  lazy val deductionDefinitionOption: Option[DeductionDefinition] = availableEntries.deductionDefinitionOption
  lazy val deductionEliminationInferenceOption: Option[(Inference, Statement, Statement)] = {
    filter(definitions.deductionEliminationInferenceOption)
  }

  lazy val generalizationDefinitionOption: Option[GeneralizationDefinition] = availableEntries.generalizationDefinitionOption
  lazy val specificationInferenceOption: Option[(Inference, Statement)] = {
    filter(definitions.specificationInferenceOption)
  }

  lazy val definedBinaryJoiners: Seq[BinaryJoiner[_ <: Expression]] = {
    Definitions.getDefinedBinaryStatements(
      availableEntries.statementDefinitions,
      availableEntries.displayShorthands,
      availableEntries.termDefinitions)
  }
  lazy val definedBinaryConnectives: Seq[BinaryConnective] = definedBinaryJoiners.ofType[BinaryConnective]
  lazy val definedBinaryRelations: Seq[BinaryRelation] = definedBinaryJoiners.ofType[BinaryRelation]

  def findRelation(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryRelationStatement] = {
    definedBinaryRelations.mapFind(relation => relation.unapply(statement).map { case (lhs, rhs) => BinaryRelationStatement(relation, lhs, rhs)(statement) })
  }

  lazy val reversals: Seq[Reversal[_ <: Expression]] = filter(definitions.reversals)
  lazy val transitivities: Seq[Transitivity[_ <: Expression]] = filter(definitions.transitivities)
  lazy val expansions: Seq[Expansion[_ <: Expression]] = filter(definitions.expansions)
  lazy val substitutions: Seq[Substitution] = filter(definitions.substitutions)

  lazy val equalityOption: Option[Equality] = {
    filter(definitions.equalityOption)
  }
  lazy val generalizationDistributions: Map[BinaryJoiner[Statement], Inference] = filter(definitions.generalizationDistributions)
  lazy val deductionDistributions: Map[BinaryJoiner[Statement], Inference] = filter(definitions.deductionDistributions)

  lazy val rearrangeableOperators: Seq[RearrangeableOperator] = {
    filter(definitions.rearrangeableOperators)
  }
  lazy val leftDistributivities: Seq[LeftDistributivity] = filter(definitions.leftDistributivities)
  lazy val rightDistributivities: Seq[RightDistributivity] = filter(definitions.rightDistributivities)
  lazy val unaryOperators: Seq[UnaryOperator] = {
    filter(definitions.unaryOperators)
  }
  lazy val leftOperatorExtractions: Seq[LeftOperatorExtraction] = filter(definitions.leftOperatorExtractions)
  lazy val rightOperatorExtractions: Seq[RightOperatorExtraction] = filter(definitions.rightOperatorExtractions)

  lazy val premiseRelationSimplificationInferences: Map[BinaryRelation, Seq[PremiseRelationSimplificationInference]] = filter(definitions.premiseRelationSimplificationInferences)
  lazy val relationRewriteInferences: Seq[RelationRewriteInference] = filter(definitions.relationRewriteInferences)
  lazy val premiseRelationRewriteInferences: Map[BinaryRelation, Seq[RelationRewriteInference]] = relationRewriteInferences.groupBy(_.premiseRelation)
  lazy val conclusionRelationRewriteInferences: Map[BinaryRelation, Seq[RelationRewriteInference]] = relationRewriteInferences.groupBy(_.conclusionRelation)

  lazy val conclusionRelationSimplificationInferences: Map[BinaryRelation, Seq[ConclusionRelationSimplificationInference]] = filter(definitions.conclusionRelationSimplificationInferences)
  lazy val conclusionSimplificationInferences: Seq[Inference] = filter(definitions.conclusionSimplificationInferences)
  lazy val termDefinitionRemovals: Map[TermDefinition, Seq[InferenceExtraction]] = filter(definitions.termDefinitionRemovals)

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Option[Statement])] = {
    filter(definitions.statementExtractionInferences)
  }

  lazy val prospectiveTermRewriteInferences: Seq[TermRewriteInference] = {
    filter(definitions.prospectiveTermRewriteInferences)
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
  lazy val facts: Seq[Fact] = {
    filter(definitions.facts)
  }
  lazy val factsBySerializedStatement: Map[String, Fact] = {
    facts.map { fact => fact.statement.serialized -> fact }.toMapPreservingEarliest
  }

  lazy val statementDeductionInferences: Seq[(Inference, Statement, Statement, Int, Int, Direction)] = {
    filter(definitions.statementDeductionInferences)
  }
  lazy val statementDefinitionIntroductionInferences: Seq[(Inference, Statement)] = {
    filter(definitions.statementDefinitionIntroductionInferences)
  }
  lazy val statementDefinitionEliminationInferences: Seq[(Inference, Statement)] = {
    filter(definitions.statementDefinitionEliminationInferences)
  }

  override def toString: String = "ProvingContext"
}

object ProvingContext {
  implicit def fromStepContext(implicit stepProvingContext: StepProvingContext): ProvingContext = stepProvingContext.provingContext
  implicit def fromStepsWithContext(implicit stepsWithContext: StepsWithContext): ProvingContext = stepsWithContext.provingContext
}
