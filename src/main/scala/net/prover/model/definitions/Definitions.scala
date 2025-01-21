package net.prover.model.definitions

import net.prover.entries.BookWithContext
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.extraction.{ExtractionCalculator, InferenceExtraction, StatementExtractionInference}
import net.prover.proving.structure.inferences._
import net.prover.proving.structure.statements._
import net.prover.util.Direction

import scala.Ordering.Implicits._
import scala.collection.mutable
import scala.util.Try

case class Definitions(allAvailableEntries: AvailableEntries) {

  def allInferences: Seq[Inference.FromEntry] = allAvailableEntries.allInferences
  lazy val inferenceEntries: Seq[Inference.Entry] = allAvailableEntries.allEntries.ofType[Inference.Entry]
  private val provingContext: ProvingContext = ProvingContext(allAvailableEntries, this)

  private val completenessByInference = mutable.Map.empty[String, Boolean]
  def isInferenceComplete(inference: Inference): Boolean = {
    completenessByInference.getOrElseUpdate(
      inference.id,
      allAvailableEntries.allInferencesWithContext.find(_._1.id == inference.id).exists { case (inference, entryWithContext) => inference.isComplete(entryWithContext) })
  }

  private lazy val inferencesWithExtractions: Seq[(Inference, Seq[InferenceExtraction])] = {
    allInferences.map { i => i -> ExtractionCalculator.getInferenceExtractions(i)(provingContext) }
  }

  lazy val allInferenceExtractions: Seq[InferenceExtraction] = {
    inferencesWithExtractions.flatMap(_._2).distinctBy(_.derivedSummary.id)
  }

  lazy val inferenceExtractionsByInferenceId: Map[String, Seq[InferenceExtraction]] = {
    inferencesWithExtractions.map(_.mapLeft(_.id)).toMap
  }

  lazy val deductionEliminationInferenceOption: Option[DeductionEliminationInference] = {
    allAvailableEntries.deductionDefinitionOption.flatMap { deductionDefinition =>
      inferenceEntries.iterator.collect {
        case inference @ Inference(
        _,
        Seq(deductionPremise @ deductionDefinition(StatementVariable(antecedentName, Nil), StatementVariable(consequentName, Nil)), antecedentPremise @ StatementVariable(antecedentName2, Nil)),
        StatementVariable(consequentName2, Nil)
        ) if antecedentName == antecedentName2 && consequentName == consequentName2 =>
          DeductionEliminationInference(inference, deductionPremise, antecedentPremise)
      }.headOption
    }
  }

  lazy val specificationInferenceOption: Option[SpecificationInference] = {
    allAvailableEntries.generalizationDefinitionOption.flatMap { generalizationDefinition =>
      inferenceEntries.iterator.collect {
        case inference @ Inference(
          _,
          Seq(singlePremise @ generalizationDefinition(_, StatementVariable(0, Seq(FunctionParameter(0, 0))))),
          StatementVariable(0, Seq(TermVariable(0, Nil)))
        ) =>
          SpecificationInference(inference, singlePremise, generalizationDefinition)
      }.headOption
    }
  }

  lazy val definedBinaryStatements: Seq[BinaryJoiner[_ <: Expression]] = Definitions.getDefinedBinaryStatements(allAvailableEntries.statementDefinitions, allAvailableEntries.displayShorthands, allAvailableEntries.termDefinitions)
  lazy val definedBinaryConnectives: Seq[BinaryConnective] = definedBinaryStatements.ofType[BinaryConnective]
  lazy val definedBinaryRelations: Seq[BinaryRelation] = definedBinaryStatements.ofType[BinaryRelation]

  def asBinaryStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryStatement[_ <: Expression]] = {
    asBinaryConnectiveStatement(statement) orElse asBinaryRelationStatement(statement)
  }
  def asBinaryConnectiveStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryConnectiveStatement] = {
    definedBinaryConnectives.mapFind(relation => relation.unapply(statement).map { case (lhs, rhs) => BinaryConnectiveStatement(relation, lhs, rhs)(statement) })
  }
  def asBinaryRelationStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryRelationStatement] = {
    definedBinaryRelations.mapFind(relation => relation.unapply(statement).map { case (lhs, rhs) => BinaryRelationStatement(relation, lhs, rhs)(statement) })
  }

  lazy val reversals: Seq[Reversal[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- inferenceEntries
      relation <- definedBinaryStatements
      premise <- inference.premises.single.toSeq
      if premise == relation.withVariables(0, 1) && inference.conclusion == relation.withVariables(1, 0)
    } yield relation.reversal(inference.summary, premise)
  }

  lazy val transitivities: Seq[Transitivity[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof

    def find[T <: Expression : ExpressionLenses](inference: Inference): Option[Transitivity[T]] = (for {
      (firstPremise, secondPremise) <- inference.premises match {
        case Seq(a, b) => Seq((a, b))
        case _ => Nil
      }
      conclusionJoiner <- definedBinaryStatements.ofType[BinaryJoiner[T]]
      if inference.conclusion == conclusionJoiner.withVariables(0, 2)
      firstPremiseJoiner <- definedBinaryStatements.ofType[BinaryJoiner[T]]
      if firstPremise == firstPremiseJoiner.withVariables(0, 1)
      secondPremiseJoiner <- definedBinaryStatements.ofType[BinaryJoiner[T]]
      if secondPremise == secondPremiseJoiner.withVariables(1, 2)
    } yield Transitivity[T](firstPremiseJoiner, secondPremiseJoiner, conclusionJoiner, inference.summary)).headOption

    for {
      inference <- inferenceEntries
      result <- find[Statement](inference) orElse find[Term](inference)
    } yield result
  }

  lazy val expansions: Seq[Expansion[_ <: Expression]] = {
    inferenceEntries.mapCollect(Expansion.fromInference(_, this))
  }

  lazy val substitutions: Seq[Substitution] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- inferenceEntries
      relation <- inference.premises.headOption.flatMap(p => definedBinaryRelations.find(r => r.unapply(p).nonEmpty))
      if (inference match {
        case Inference(
          _,
          Seq(
            relation(TermVariable(a, Nil), TermVariable(b, Nil)),
            StatementVariable(phi, Seq(TermVariable(c, Nil)))),
        StatementVariable(psi, Seq(TermVariable(d, Nil)))
        ) if a == c && b == d && phi == psi =>
          true
        case _ =>
          false
      })
    } yield Substitution(relation, inference.summary)
  }

  lazy val equalityOption: Option[Equality] = {
    for {
      definition <- allAvailableEntries.statementDefinitions.find(_.attributes.contains("equality"))
      relation = BinaryRelationFromDefinition(definition)
      expansion <- expansions.ofType[RelationExpansion].find(e => e.sourceJoiner == relation && e.resultJoiner == relation)
      substitution <- substitutions.find(_.relation == relation)
      reversal <- reversals.ofType[Reversal[Term]].find(_.joiner == relation)
      transitivity <- transitivities.ofType[Transitivity[Term]].find(_.isTransitivityForJoiner(relation))
    } yield Equality(relation, expansion, substitution, reversal, transitivity)
  }

  lazy val generalizationDistributions: Map[BinaryJoiner[Statement], Inference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      generalizationDefinition <- allAvailableEntries.generalizationDefinitionOption.toSeq
      inference <- inferenceEntries
      connective <- definedBinaryConnectives
      (generalizationDefinition(_, StatementVariable(a, Seq(FunctionParameter(0, 0)))), generalizationDefinition(_, StatementVariable(b, Seq(FunctionParameter(0, 0))))) <- connective.unapply(inference.conclusion)
      if inference.premises == Seq(generalizationDefinition("", connective(StatementVariable(a, Seq(FunctionParameter(0, 0))), StatementVariable(b, Seq(FunctionParameter(0, 0))))(SubstitutionContext.withExtraParameters(1))))
    } yield connective -> inference).toMap
  }
  lazy val deductionDistributions: Map[BinaryJoiner[Statement], Inference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      deductionDefinition <- allAvailableEntries.deductionDefinitionOption.toSeq
      inference <- inferenceEntries
      connective <- definedBinaryConnectives
      (deductionDefinition(StatementVariable(a, Nil), StatementVariable(b, Nil)), deductionDefinition(StatementVariable(c, Nil), StatementVariable(d, Nil))) <- connective.unapply(inference.conclusion)
      if a == c
      if inference.premises == Seq(deductionDefinition(StatementVariable(a, Nil), connective(StatementVariable(b, Nil), StatementVariable(d, Nil))))
    } yield connective -> inference).toMap
  }

  private lazy val rearrangementInferences = for {
    inferenceExtraction <- allInferenceExtractions
    if inferenceExtraction.variableDefinitions.statements.isEmpty &&
      inferenceExtraction.variableDefinitions.hasNoApplications &&
      inferenceExtraction.conclusion.usedVariables.usesAll(inferenceExtraction.variableDefinitions) &&
      inferenceExtraction.premises.forall { premise =>
        asBinaryRelationStatement(premise)(SubstitutionContext.outsideProof).exists { case BinaryRelationStatement(_, left, right) =>
          ExpressionUtils.isSimpleTermVariable(left) && ExpressionUtils.isCombinationOfTermConstants(right)
        }
      }
  } yield inferenceExtraction

  lazy val rearrangeableOperators: Seq[RearrangeableOperator] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    def findCommutativities(equality: Equality): Seq[(BinaryOperator, Commutativity)] = {
      for {
        inferenceExtraction <- rearrangementInferences
        if !inferenceExtraction.extractionDefinition.reversalInference.contains(equality.reversal.inference)
        (l, r) <- equality.unapply(inferenceExtraction.conclusion)
        if ExpressionUtils.getCombinationOfSimpleTermVariables(l).contains(Seq(0, 1))
        operator = BinaryOperator(l)
        if r == operator(TermVariable(1), TermVariable(0))
      } yield (operator, Commutativity(operator, inferenceExtraction))
    }
    def findAssociativity(operator: BinaryOperator, equality: Equality): Option[Associativity] = {
      rearrangementInferences
        .mapFind { inferenceExtraction =>
          for {
            (operator(TermVariable(0, Nil), operator(TermVariable(1, Nil), TermVariable(2, Nil))), operator(operator(TermVariable(0, Nil), TermVariable(1, Nil)), TermVariable(2, Nil))) <- equality.unapply(inferenceExtraction.conclusion)
          } yield Associativity(operator, inferenceExtraction)
        }
    }
    def getOperator(operator: BinaryOperator, commutativity: Commutativity, associativity: Associativity, equality: Equality): RearrangeableOperator = {
      val leftIdentities = rearrangementInferences.mapCollect { inferenceExtraction =>
        for {
          (operator(identityConstant, TermVariable(0, Nil)), TermVariable(0, Nil)) <- equality.unapply(inferenceExtraction.conclusion)
          if ExpressionUtils.isCombinationOfTermConstants(identityConstant)
        } yield LeftIdentity(operator, identityConstant, inferenceExtraction)
      }
      val rightIdentities = rearrangementInferences.mapCollect { inferenceExtraction =>
        for {
          (operator(TermVariable(0, Nil), identityConstant), TermVariable(0, Nil)) <- equality.unapply(inferenceExtraction.conclusion)
          if ExpressionUtils.isCombinationOfTermConstants(identityConstant)
        } yield RightIdentity(operator, identityConstant, inferenceExtraction)
      }
      val doubleSidedIdentity = for {
        leftIdentity <- leftIdentities.single
        rightIdentity <- rightIdentities.single
        if leftIdentity.identityTerm == rightIdentity.identityTerm
      } yield DoubleSidedIdentity(operator, leftIdentity.identityTerm, leftIdentity, rightIdentity)
      val leftAbsorbers = rearrangementInferences.mapCollect { inferenceExtraction =>
        for {
          (operator(absorberConstant, TermVariable(0, Nil)), rhs) <- equality.unapply(inferenceExtraction.conclusion)
          if rhs == absorberConstant && ExpressionUtils.isCombinationOfTermConstants(absorberConstant)
        } yield LeftAbsorber(operator, absorberConstant, inferenceExtraction)
      }
      val rightAbsorbers = rearrangementInferences.mapCollect { inferenceExtraction =>
        for {
          (operator(TermVariable(0, Nil), absorberConstant), rhs) <- equality.unapply(inferenceExtraction.conclusion)
          if rhs == absorberConstant && ExpressionUtils.isCombinationOfTermConstants(absorberConstant)
        } yield RightAbsorber(operator, absorberConstant, inferenceExtraction)
      }
      val inverse = (for {
        doubleSidedIdentity <- doubleSidedIdentity.toSeq
        identityTerm = doubleSidedIdentity.identityTerm
        rightInverseInferenceExtraction <- rearrangementInferences
        (operator(TermVariable(0, Nil), inverseTerm), `identityTerm`) <- equality.unapply(rightInverseInferenceExtraction.conclusion).toSeq
        if ExpressionUtils.getSingleSimpleTermVariable(inverseTerm).contains(0)
        leftInverseInferenceExtraction <- rearrangementInferences
        if leftInverseInferenceExtraction.conclusion == equality(operator(inverseTerm, TermVariable(0, Nil)), identityTerm)
        inverseOperator = UnaryOperator(inverseTerm)
      } yield DoubleSidedInverse(
        operator,
        inverseOperator,
        doubleSidedIdentity,
        RightInverse(operator, inverseOperator, doubleSidedIdentity, rightInverseInferenceExtraction),
        LeftInverse(operator, inverseOperator, doubleSidedIdentity, leftInverseInferenceExtraction))).headOption
      RearrangeableOperator(operator, commutativity, associativity, leftIdentities, rightIdentities, doubleSidedIdentity, leftAbsorbers, rightAbsorbers, inverse)
    }
    for {
      equality <- equalityOption.toSeq
      (operator, commutativity) <- findCommutativities(equality)
      associativity <- findAssociativity(operator, equality)
    } yield getOperator(operator, commutativity, associativity, equality)
  }
  lazy val leftDistributivities: Seq[LeftDistributivity] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      inferenceExtraction <- rearrangementInferences
      distributor <- rearrangeableOperators
      distributee <- rearrangeableOperators
      (
        distributor(TermVariable(0, Nil), distributee(TermVariable(1, Nil), TermVariable(2, Nil))),
        distributee(distributor(TermVariable(0, Nil), TermVariable(1, Nil)), distributor(TermVariable(0, Nil), TermVariable(2, Nil)))
      ) <- equality.unapply(inferenceExtraction.conclusion).toSeq
    } yield LeftDistributivity(distributor, distributee, inferenceExtraction)
  }
  lazy val rightDistributivities: Seq[RightDistributivity] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      inferenceExtraction <- rearrangementInferences
      distributor <- rearrangeableOperators
      distributee <- rearrangeableOperators
      (
        distributor(distributee(TermVariable(0, Nil), TermVariable(1, Nil)), TermVariable(2, Nil)),
        distributee(distributor(TermVariable(0, Nil), TermVariable(2, Nil)), distributor(TermVariable(1, Nil), TermVariable(2, Nil)))
      ) <- equality.unapply(inferenceExtraction.conclusion).toSeq
    } yield RightDistributivity(distributor, distributee, inferenceExtraction)
  }

  lazy val unaryOperators: Seq[UnaryOperator] = rearrangeableOperators.flatMap(_.inverse).map(_.inverseOperator)
  lazy val leftOperatorExtractions: Seq[LeftOperatorExtraction] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      unaryOperator <- unaryOperators
      rearrangeableOperator <- rearrangeableOperators
      binaryOperator = rearrangeableOperator.operator
      inferenceExtraction <- rearrangementInferences
      (
        binaryOperator(unaryOperator(TermVariable(0, Nil)), TermVariable(1, Nil)),
        unaryOperator(binaryOperator(TermVariable(0, Nil), TermVariable(1, Nil)))
      ) <- equality.unapply(inferenceExtraction.conclusion)
    } yield LeftOperatorExtraction(unaryOperator, binaryOperator, inferenceExtraction)
  }
  lazy val rightOperatorExtractions: Seq[RightOperatorExtraction] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      unaryOperator <- unaryOperators
      rearrangeableOperator <- rearrangeableOperators
      binaryOperator = rearrangeableOperator.operator
      inferenceExtraction <- rearrangementInferences
      (
        binaryOperator(TermVariable(0, Nil), unaryOperator(TermVariable(1, Nil))),
        unaryOperator(binaryOperator(TermVariable(0, Nil), TermVariable(1, Nil)))
      ) <- equality.unapply(inferenceExtraction.conclusion)
    } yield RightOperatorExtraction(unaryOperator, binaryOperator, inferenceExtraction)
  }

  def getPossiblePremiseDesimplifications(premise: Statement): Seq[DerivedPremise] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    def directly = DirectPremise(premise)
    def byDesimplifying = for {
      inference <- conclusionSimplificationInferences
      substitutions <- inference.conclusion.calculateSubstitutions(premise).flatMap(_.confirmTotality(inference.variableDefinitions)).toSeq
      substitutedInferencePremises <- inference.premises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
      innerDesimplifications <- getPossiblePremiseDesimplifications(substitutedInferencePremises)
    } yield DesimplifiedPremise(premise, inference, innerDesimplifications)

    directly +: byDesimplifying
  }

  def getPossiblePremiseDesimplifications(premises: Seq[Statement]): Seq[Seq[DerivedPremise]] = {
    premises.foldProduct(getPossiblePremiseDesimplifications)
  }

  lazy val premiseRelationSimplificationInferences: Map[BinaryRelation, Seq[PremiseRelationSimplificationInference]] = {
    PremiseRelationSimplificationInference.getAll(this)
  }

  lazy val conclusionRelationSimplificationInferences: Map[BinaryRelation, Seq[ConclusionRelationSimplificationInference]] = {
    ConclusionRelationSimplificationInference.getAll(this)
  }

  lazy val relationRewriteInferences: Seq[RelationRewriteInference] = RelationRewriteInference.getAll(this)

  lazy val chainableRewriteInferences: Seq[ChainableRewriteInference] = ChainableRewriteInference.getAll(this)

  // An inference such as `φ,ψ ⊢ φ∧ψ` or `φ ⊢ φ∨ψ` that can be run in reverse to allow a target conclusion to be
  // decomposed into smaller parts
  lazy val conclusionSimplificationInferences: Seq[Inference] = allInferences.filter {
    case inference
      if inference.premises.nonEmpty &&
        inference.premises.forall(_.complexity < inference.conclusion.complexity) &&
        inference.conclusion.usedVariables.usesAll(inference.variableDefinitions) &&
        inference.variableDefinitions.hasNoApplications &&
        inference.premises.forall(_.referencedDefinitions.subsetOf(inference.conclusion.referencedDefinitions))
    =>
      true
    case _ =>
      false
  }

  lazy val termDefinitionRemovals: Map[TermDefinition, Seq[InferenceExtraction]] = {
    allAvailableEntries.termDefinitions.map { termDefinition =>
      termDefinition -> (for {
        inferenceExtraction <- inferenceExtractionsByInferenceId(termDefinition.definitionInference.id)
        if inferenceExtraction.conclusion.referencedDefinitions.contains(termDefinition) &&
          !inferenceExtraction.premises.exists(_.referencedDefinitions.contains(termDefinition)) &&
          inferenceExtraction.conclusion.usedVariables.usesAll(inferenceExtraction.variableDefinitions)
      } yield inferenceExtraction)
    }.toMap
  }

  lazy val statementExtractionInferences: Seq[StatementExtractionInference] = inferenceEntries.mapCollect(StatementExtractionInference.fromInference)

  lazy val termRewriteInferences: Seq[TermRewriteInference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      inferenceExtraction <- allInferenceExtractions
      (lhs, rhs) <- equality.unapply(inferenceExtraction.conclusion)
    } yield TermRewriteInference(inferenceExtraction, lhs, rhs)
  }
  lazy val prospectiveTermRewriteInferences: Seq[TermRewriteInference] = {
    termRewriteInferences.filter { case TermRewriteInference(_, left, right) =>
      left.usedVariables.contains(right.usedVariables)
    }
  }
  lazy val termSimplificationInferences: Seq[TermRewriteInference] = {
    termRewriteInferences.filter { case TermRewriteInference(_, left, right) =>
      left.complexity > right.complexity && left.usedVariables.contains(right.usedVariables)
    }
  }
  lazy val termDesimplificationInferences: Seq[TermRewriteInference] = {
    termRewriteInferences.filter { case TermRewriteInference(_, left, right) =>
      left.complexity < right.complexity && right.usedVariables.contains(left.usedVariables)
    }
  }

  lazy val statementDefinitionDeconstructions: Seq[Inference] = {
    @scala.annotation.tailrec
    def isBinaryDefinitionWithinUnaryDefinition(firstPremise: Statement, secondPremise: Statement, conclusion: Statement): Boolean = {
      (firstPremise, secondPremise, conclusion) match {
        case (StatementVariable(v1, Nil), StatementVariable(v2, Nil), DefinedStatement(Seq(StatementVariable(v3, Nil), StatementVariable(v4, Nil)), _))
          if (v1 == v3) && (v2 == v4)
        =>
          true
        case (DefinedStatement(Seq(innerFirstPremise: Statement), firstDefinition), DefinedStatement(Seq(innerSecondPremise: Statement), secondDefinition), DefinedStatement(Seq(innerConclusion: Statement), conclusionDefinition))
          if firstDefinition == conclusionDefinition && secondDefinition == conclusionDefinition
        =>
          isBinaryDefinitionWithinUnaryDefinition(innerFirstPremise, innerSecondPremise, innerConclusion)
        case _ =>
          false
      }
    }

    inferenceEntries.collect {
      case inference @ Inference(_, Seq(firstPremise, secondPremise), conclusion) if isBinaryDefinitionWithinUnaryDefinition(firstPremise, secondPremise, conclusion) =>
        inference
    }
  }
  lazy val structuralSimplificationInferences: Seq[(Inference, Statement)] = {
    inferenceEntries.collect {
      case inference @ Inference(
        _,
        Seq(singlePremise @ DefinedStatement(components, _)),
        conclusion
      ) if conclusion.asOptionalInstanceOf[StatementVariable].exists(components.contains) =>
        (inference, singlePremise)
    }
  }

  lazy val facts: Seq[Fact] = {
    for {
      inferenceExtraction <- allInferenceExtractions
      if inferenceExtraction.premises.isEmpty && inferenceExtraction.variableDefinitions.isEmpty
    } yield Fact(inferenceExtraction)
  }

  lazy val statementDeductionInferences: Seq[(Inference, Statement, Statement, Int, Int, Direction)] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      deductionDefinition <- allAvailableEntries.deductionDefinitionOption.toSeq
      result <- for {
        inference <- allInferences
        Seq(firstPremise @ deductionDefinition(StatementVariable(a, Nil), StatementVariable(b, Nil)), otherPremise: DefinedStatement) <- Seq(inference.premises)
        swapper <- Seq(Direction.Forward, Direction.Reverse)
        (premiseIndex, conclusionIndex) = swapper.swapSourceAndResult(a, b)
        if inference.variableDefinitions.terms.isEmpty && inference.variableDefinitions.hasNoApplications
        if otherPremise.usedVariables.statements.variableIndices.contains(premiseIndex) && inference.conclusion.usedVariables.statements.variableIndices.contains(conclusionIndex)
        if otherPremise.applySubstitutions(Substitutions(inference.variableDefinitions.statements.indices.map { i => StatementVariable(if (i == premiseIndex) conclusionIndex else i)}, Nil)).contains(inference.conclusion)
      } yield (inference, firstPremise, otherPremise, premiseIndex, conclusionIndex, swapper)
    } yield result
  }

  object WrappedStatementVariable {
    def unapply(statement: Statement): Option[Int] = statement match {
      case DefinedStatement(Seq(StatementVariable(index, Nil)), _) => Some(index)
      case DefinedStatement(Seq(WrappedStatementVariable(index)), _) => Some(index)
      case _ => None
    }
  }

  lazy val statementDefinitionIntroductionInferences: Seq[(Inference, Statement)] = {
    allInferences.collect {
      case inference @ Inference(_, Seq(premise @ StatementVariable(name, Nil)), WrappedStatementVariable(conclusionName))
        if conclusionName == name
      => (inference, premise)
    }
  }
  lazy val statementDefinitionEliminationInferences: Seq[(Inference, Statement)] = {
    allInferences.collect {
      case inference @ Inference(_, Seq(premise @ WrappedStatementVariable(premiseName)), StatementVariable(name, Nil))
        if premiseName == name
      => (inference, premise)
    }
  }

  override def toString: String = s"Definitions (${allAvailableEntries.allEntries.length} entries)"
}

object Definitions {
  def apply(books: Seq[BookWithContext]): Definitions = {
    Definitions(AvailableEntries.forBooks(books))
  }

  def getDefinedBinaryStatements(statementDefinitions: Seq[StatementDefinition], shorthands: Seq[DisplayShorthand], termDefinitions: Seq[TermDefinition]): Seq[BinaryJoiner[_ <: Expression]] = {
    def fromDefinitions = for {
      definition <- statementDefinitions
      if definition.format.baseFormatString == s"%1 %0 %2"
      result <- definition.componentTypes match {
        case Seq(_: StatementComponent, _: StatementComponent) => Some(BinaryConnective(definition))
        case Seq(_: TermComponent, _: TermComponent) => Some(BinaryRelationFromDefinition(definition))
        case _ => None
      }
    } yield result

    def fromGeneralShorthands = for {
      shorthand <- shorthands
      if shorthand.template.isInstanceOf[DefinedStatementTemplate]
      if shorthand.template.variables.length == 3
      Seq(lhsIndex, symbolIndex, rhsIndex) <- "%(\\d) %(\\d) %(\\d)".r.unapplySeq(shorthand.format.baseFormatString).map(_.map(_.toInt)).toSeq
      if lhsIndex != symbolIndex && lhsIndex != rhsIndex && symbolIndex != rhsIndex
      lhsVariable = shorthand.template.variables(lhsIndex)
      symbolVariable = shorthand.template.variables(symbolIndex)
      rhsVariable = shorthand.template.variables(rhsIndex)
      if lhsVariable.isInstanceOf[TermVariableTemplate] && symbolVariable.isInstanceOf[TermVariableTemplate] && rhsVariable.isInstanceOf[TermVariableTemplate]
      if shorthand.conditions.forall(_._1 == symbolVariable.name)
      definition <- termDefinitions
      if definition.componentTypes.isEmpty
      if shorthand.conditions.map(_._2).forall(definition.attributes.contains)
    } yield BinaryRelationFromGeneralShorthand(definition, shorthand, lhsVariable.name, rhsVariable.name, symbolVariable.name)

    def fromSpecificShorthands = for {
      shorthand <- shorthands
      if shorthand.template.isInstanceOf[DefinedStatementTemplate]
      if shorthand.template.variables.length == 2
      Seq(lhsIndexText, symbol, rhsIndexText) <- "%(\\d) (.) %(\\d)".r.unapplySeq(shorthand.format.baseFormatString).toSeq
      lhsIndex <- Try(lhsIndexText.toInt).toOption
      rhsIndex <- Try(rhsIndexText.toInt).toOption
      if lhsIndex != rhsIndex
      lhsVariable = shorthand.template.variables(lhsIndex)
      rhsVariable = shorthand.template.variables(rhsIndex)
      if lhsVariable.isInstanceOf[TermVariableTemplate] && rhsVariable.isInstanceOf[TermVariableTemplate]
      if shorthand.conditions.isEmpty
    } yield BinaryRelationFromSpecificShorthand(symbol, shorthand, lhsVariable.name, rhsVariable.name)

    (fromDefinitions ++ fromGeneralShorthands ++ fromSpecificShorthands).reverse
  }
}
