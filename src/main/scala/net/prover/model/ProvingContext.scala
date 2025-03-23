package net.prover.model

import net.prover.entries.StepsWithContext
import net.prover.model.definitions.*
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.*
import net.prover.model.utils.ExpressionUtils.TypeLikeStatement
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.extraction.*
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.proving.structure.inferences.*
import net.prover.proving.structure.statements.*
import net.prover.theorems.GetReferencedInferences
import net.prover.util.Direction

import scala.deriving.Mirror
import scala.runtime.Tuples

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

    given allowableEmptyTuple: Allowable[EmptyTuple] = alwaysAllowable
    given allowableTuple[H, T <: Tuple](using Allowable[H], Allowable[T]): Allowable[H *: T] = allowable { t =>
      isAllowed(t.head) && isAllowed(t.tail)
    }
    given allowableProduct[T <: Product](
      using mirror: Mirror.ProductOf[T],
      allowedElemTypes: Allowable[mirror.MirroredElemTypes]
    ): Allowable[T] = allowable { t =>
      isAllowed(Tuple.fromProductTyped(t))
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

    implicit val allowableRelation: Allowable[BinaryJoiner[? <: Expression]] = allowable(definedBinaryJoiners.contains)
    implicit val allowableReversal: Allowable[Reversal[? <: Expression]] = allowable(r => isAllowed(r.joiner) && isAllowed(r.inference))
    implicit val allowableTransitivity: Allowable[Transitivity[? <: Expression]] = allowable(r => isAllowed(r.firstPremiseJoiner) && isAllowed(r.secondPremiseJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableExpansion: Allowable[Expansion[? <: Expression]] = allowable(r => isAllowed(r.sourceJoiner) && isAllowed(r.resultJoiner) && isAllowed(r.inference))
    implicit val allowableSubstitution: Allowable[Substitution] = allowableProduct[Substitution]
    implicit val allowableStep: Allowable[Step] = allowable(step => GetReferencedInferences(step).forall(isAllowed))
    implicit val allowableKnownStatement: Allowable[KnownStatement] = allowableProduct[KnownStatement]
    implicit def allowableDesimplifiedPremise: Allowable[DesimplifiedPremise] = allowableProduct[DesimplifiedPremise]
    implicit def allowablePremiseDesimplification: Allowable[DerivedPremise] = allowable {
      case DirectPremise(_) => true
      case desimplifiedPremise: DesimplifiedPremise => allowableDesimplifiedPremise.isAllowed(desimplifiedPremise)
    }

    implicit val alwaysAllowableOperator: Allowable[Operator] = alwaysAllowable
    implicit val allowableEquality: Allowable[Equality] = allowableProduct[Equality]

    implicit val allowableGeneralizationDefinition: Allowable[GeneralizationDefinition] = allowableProduct[GeneralizationDefinition]

    implicit val allowableExtractionDefinition: Allowable[ExtractionDefinition] = allowableProduct[ExtractionDefinition]
    implicit val allowableChainedRewriteStep: Allowable[PartiallyAppliedExtraction.ChainedRewriteStep] = allowableProduct[PartiallyAppliedExtraction.ChainedRewriteStep]
    implicit val allowablePartiallyAppliedExtraction: Allowable[PartiallyAppliedExtraction] = allowableProduct[PartiallyAppliedExtraction]
    implicit val allowableInferenceExtraction: Allowable[InferenceExtraction] = allowableProduct[InferenceExtraction]
    implicit val allowableFact: Allowable[Fact] = allowableProduct[Fact]
    implicit val allowableCommutativity: Allowable[Commutativity] = allowableProduct[Commutativity]
    implicit val allowableAssociativity: Allowable[Associativity] = allowableProduct[Associativity]
    implicit val allowableLeftIdentity: Allowable[LeftIdentity] = allowableProduct[LeftIdentity]
    implicit val allowableRightIdentity: Allowable[RightIdentity] = allowableProduct[RightIdentity]
    implicit val allowableDoubleSidedIdentity: Allowable[DoubleSidedIdentity] = allowableProduct[DoubleSidedIdentity]
    implicit val allowableLeftAbsorber: Allowable[LeftAbsorber] = allowableProduct[LeftAbsorber]
    implicit val allowableRightAbsorber: Allowable[RightAbsorber] = allowableProduct[RightAbsorber]
    implicit val allowableLeftInverse: Allowable[LeftInverse] = allowableProduct[LeftInverse]
    implicit val allowableRightInverse: Allowable[RightInverse] = allowableProduct[RightInverse]
    implicit val allowableDoubleSidedInverse: Allowable[DoubleSidedInverse] = allowableProduct[DoubleSidedInverse]

    implicit val allowableRearrangeableOperator: Allowable[RearrangeableOperator] = allowableProduct[RearrangeableOperator]
    implicit val allowableLeftDistributivity: Allowable[LeftDistributivity] = allowableProduct[LeftDistributivity]
    implicit val allowableRightDistributivity: Allowable[RightDistributivity] = allowableProduct[RightDistributivity]
    implicit val allowableLeftOperatorExtraction: Allowable[LeftOperatorExtraction] = allowableProduct[LeftOperatorExtraction]
    implicit val allowableRightOperatorExtraction: Allowable[RightOperatorExtraction] = allowableProduct[RightOperatorExtraction]

    implicit val allowableDeductionEliminationInference: Allowable[DeductionEliminationInference] = allowableProduct[DeductionEliminationInference]
    implicit val allowableSpecificationInference: Allowable[SpecificationInference] = allowableProduct[SpecificationInference]
    implicit val allowableStatementExtractionInference: Allowable[StatementExtractionInference] = allowableProduct[StatementExtractionInference]
    implicit val allowablePremiseRelationSimplificationInference: Allowable[PremiseRelationSimplificationInference] = allowableProduct[PremiseRelationSimplificationInference]
    implicit val allowablePremiseRelationRewriteInference: Allowable[RelationRewriteInference] = allowableProduct[RelationRewriteInference]
    implicit val allowableConclusionRelationSimplificationInference: Allowable[ConclusionRelationSimplificationInference] = allowableProduct[ConclusionRelationSimplificationInference]
    implicit val allowableChainableRewriteInference: Allowable[ChainableRewriteInference] = allowableProduct[ChainableRewriteInference]

    implicit val allowableTermRewriteInference: Allowable[TermRewriteInference] = allowableProduct[TermRewriteInference]

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
  lazy val deductionEliminationInferenceOption: Option[DeductionEliminationInference] = {
    filter(definitions.deductionEliminationInferenceOption)
  }

  lazy val generalizationDefinitionOption: Option[GeneralizationDefinition] = availableEntries.generalizationDefinitionOption
  lazy val specificationInferenceOption: Option[SpecificationInference] = {
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

  def asBinaryConnectiveStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryConnectiveStatement] = {
    definedBinaryConnectives.mapFind(relation => relation.unapply(statement).map { case (lhs, rhs) => BinaryConnectiveStatement(relation, lhs, rhs)(statement) })
  }
  def asBinaryRelationStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryRelationStatement] = {
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

  lazy val chainableRewriteInferences: Seq[ChainableRewriteInference] = filter(definitions.chainableRewriteInferences)

  lazy val conclusionRelationSimplificationInferences: Map[BinaryRelation, Seq[ConclusionRelationSimplificationInference]] = filter(definitions.conclusionRelationSimplificationInferences)
  lazy val conclusionSimplificationInferences: Seq[Inference] = filter(definitions.conclusionSimplificationInferences)
  lazy val termDefinitionRemovals: Map[TermDefinition, Seq[InferenceExtraction]] = filter(definitions.termDefinitionRemovals)

  lazy val statementExtractionInferences: Seq[StatementExtractionInference] = {
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
