package net.prover.model.definitions

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions._
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{DerivationStep, Step, SubstatementExtractor, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.model.utils.ExpressionUtils.{TypeLikeStatement, TypeStatement}
import net.prover.util.Direction

import scala.Ordering.Implicits._
import scala.collection.mutable
import scala.util.Try

case class Definitions(rootEntryContext: EntryContext) {

  lazy val allInferences: Seq[Inference.FromEntry] = rootEntryContext.allInferences
  lazy val inferenceEntries: Seq[Inference] = rootEntryContext.availableEntries.ofType[Inference]
  private val provingContext: ProvingContext = ProvingContext(rootEntryContext, this)

  val completenessByInference = mutable.Map.empty[String, Boolean]
  def isInferenceComplete(inference: Inference): Boolean = {
    completenessByInference.getOrElseUpdate(inference.id, allInferences.find(_.id == inference.id).exists(_.isComplete(this)))
  }

  lazy val extractionOptionsByInferenceId: Map[String, Seq[ExtractionOption]] = {
    allInferences.map { i =>
      (i.id, SubstatementExtractor.getExtractionOptions(i)(provingContext))
    }.toMap
  }

  lazy val deductionEliminationInferenceOption: Option[(Inference, Statement, Statement)] = {
    rootEntryContext.deductionDefinitionOption.flatMap { deductionDefinition =>
      inferenceEntries.iterator.collect {
        case inference @ Inference(
        _,
        Seq(deductionPremise @ deductionDefinition(StatementVariable(antecedentName, Nil), StatementVariable(consequentName, Nil)), antecedentPremise @ StatementVariable(antecedentName2, Nil)),
        StatementVariable(consequentName2, Nil)
        ) if antecedentName == antecedentName2 && consequentName == consequentName2 =>
          (inference, deductionPremise, antecedentPremise)
      }.headOption
    }
  }

  lazy val specificationInferenceOption: Option[(Inference, Statement, String, String)] = {
    rootEntryContext.generalizationDefinitionOption.flatMap { generalizationDefinition =>
      inferenceEntries.iterator.collect {
        case inference @ Inference(
        _,
        Seq(singlePremise @ generalizationDefinition(_, StatementVariable(premisePredicateName, Seq(FunctionParameter(0, 0))))),
        StatementVariable(conclusionPredicateName, Seq(TermVariable(variableName, Nil)))
        ) if premisePredicateName == conclusionPredicateName =>
          (inference, singlePremise, premisePredicateName, variableName)
      }.headOption
    }
  }

  lazy val definedBinaryStatements: Seq[BinaryJoiner[_ <: Expression]] = Definitions.getDefinedBinaryStatements(rootEntryContext.statementDefinitions, rootEntryContext.displayShorthands, rootEntryContext.termDefinitions)
  lazy val definedBinaryConnectives: Seq[BinaryConnective] = definedBinaryStatements.ofType[BinaryConnective]
  lazy val definedBinaryRelations: Seq[BinaryRelation] = definedBinaryStatements.ofType[BinaryRelation]

  lazy val reversals: Seq[Reversal[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- inferenceEntries
      relation <- definedBinaryStatements
      if (inference match {
        case Inference(
        _,
        Seq(relation(ExpressionVariable(a, Nil), ExpressionVariable(b, Nil))),
        relation(ExpressionVariable(c, Nil), ExpressionVariable(d, Nil))
        ) if a == d && b == c =>
          true
        case _ =>
          false
      })
    } yield relation.reversal(inference.summary)
  }

  lazy val transitivities: Seq[Transitivity[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof

    def find[T <: Expression](inference: Inference): Option[Transitivity[T]] = (for {
      (firstPremise, secondPremise) <- inference.premises match {
        case Seq(a, b) => Seq((a, b))
        case _ => Nil
      }
      conclusionJoiner <- definedBinaryStatements.ofType[BinaryJoiner[T]]
      (ExpressionVariable(conclusionLhs, Nil), ExpressionVariable(conclusionRhs, Nil)) <- conclusionJoiner.unapply(inference.conclusion).toSeq
      firstPremiseJoiner <- definedBinaryStatements.ofType[BinaryJoiner[T]]
      (ExpressionVariable(firstPremiseLhs, Nil), ExpressionVariable(firstPremiseRhs, Nil)) <- firstPremiseJoiner.unapply(firstPremise).toSeq
      secondPremiseJoiner <- definedBinaryStatements.ofType[BinaryJoiner[T]]
      (ExpressionVariable(secondPremiseLhs, Nil), ExpressionVariable(secondPremiseRhs, Nil)) <- secondPremiseJoiner.unapply(secondPremise).toSeq
      if firstPremiseLhs == conclusionLhs && firstPremiseRhs == secondPremiseLhs && secondPremiseRhs == conclusionRhs
    } yield Transitivity[T](firstPremiseJoiner, secondPremiseJoiner, conclusionJoiner, inference.summary)).headOption

    for {
      inference <- inferenceEntries
      result <- find[Statement](inference) orElse find[Term](inference)
    } yield result
  }

  lazy val expansions: Seq[Expansion[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- inferenceEntries
      premise <- inference.premises.single
      sourceRelation <- definedBinaryRelations.find(r => r.unapply(premise).nonEmpty)
      (targetRelation, constructor) <- definedBinaryRelations.find(_.unapply(inference.conclusion).nonEmpty).map(r => r -> ((i: Inference.Summary) => RelationExpansion.apply(sourceRelation, r, i))) orElse
       definedBinaryConnectives.find(_.unapply(inference.conclusion).nonEmpty).map(c => c -> ((i: Inference.Summary) => ConnectiveExpansion.apply(sourceRelation, c, i)))
      if (inference match {
        case Inference(
          _,
          Seq(sourceRelation(TermVariable(a, Nil), TermVariable(b, Nil))),
          targetRelation(ExpressionVariable(f, Seq(TermVariable(c, Nil))), ExpressionVariable(g, Seq(TermVariable(d, Nil))))
        ) if a == c && b == d && f == g =>
          true
        case _ =>
          false
      })
    } yield constructor(inference.summary)
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
      definition <- rootEntryContext.statementDefinitions.find(_.attributes.contains("equality"))
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
      generalizationDefinition <- rootEntryContext.generalizationDefinitionOption.toSeq
      inference <- inferenceEntries
      connective <- definedBinaryConnectives
      (generalizationDefinition(_, StatementVariable(a, Seq(FunctionParameter(0, 0)))), generalizationDefinition(_, StatementVariable(b, Seq(FunctionParameter(0, 0))))) <- connective.unapply(inference.conclusion)
      if inference.premises == Seq(generalizationDefinition("", connective(StatementVariable(a, Seq(FunctionParameter(0, 0))), StatementVariable(b, Seq(FunctionParameter(0, 0))))(SubstitutionContext.withExtraParameters(1))))
    } yield connective -> inference).toMap
  }
  lazy val deductionDistributions: Map[BinaryJoiner[Statement], Inference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      deductionDefinition <- rootEntryContext.deductionDefinitionOption.toSeq
      inference <- inferenceEntries
      connective <- definedBinaryConnectives
      (deductionDefinition(StatementVariable(a, Nil), StatementVariable(b, Nil)), deductionDefinition(StatementVariable(c, Nil), StatementVariable(d, Nil))) <- connective.unapply(inference.conclusion)
      if a == c
      if inference.premises == Seq(deductionDefinition(StatementVariable(a, Nil), connective(StatementVariable(b, Nil), StatementVariable(d, Nil))))
    } yield connective -> inference).toMap
  }

  private val rearrangementInferences = for {
    inference <- inferenceEntries
    extractionOption <- extractionOptionsByInferenceId(inference.id)
    substitutions = extractionOption.requiredSubstitutions
    if substitutions.statements.isEmpty &&
      substitutions.hasNoApplications &&
      extractionOption.conclusion.requiredSubstitutions.isEquivalentTo(substitutions) &&
      extractionOption.premises.forall { premise =>
        findRelation(premise)(SubstitutionContext.outsideProof).exists { case BinaryRelationStatement(_, left, right) =>
          ExpressionUtils.isSimpleTermVariable(left) && ExpressionUtils.isCombinationOfTermConstants(right)
        }
      }
  } yield (inference, extractionOption)

  lazy val rearrangeableOperators: Seq[RearrangeableOperator] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    def findCommutativities(equality: Equality): Seq[(BinaryOperator, Commutativity)] = {
      for {
        (inference, extractionOption) <- rearrangementInferences
        (l, r) <- equality.unapply(extractionOption.conclusion)
        operator = BinaryOperator(l)
        (first: TermVariable, second: TermVariable) <- operator.unapply(l)
        if r == operator(second, first)
      } yield (operator, Commutativity(operator, inference.summary, extractionOption))
    }
    def findAssociativity(operator: BinaryOperator, equality: Equality): Option[Associativity] = {
      rearrangementInferences
        .mapFind { case (inference, extractionOption) =>
          for {
            (operator(a: TermVariable, operator(b: TermVariable, c: TermVariable)), rhs) <- equality.unapply(extractionOption.conclusion)
            if rhs == operator(operator(a, b), c)
          } yield Associativity(operator, inference.summary, extractionOption)
        }
    }
    def findLeftIdentity(operator: BinaryOperator, equality: Equality): Option[LeftIdentity] = {
      rearrangementInferences
        .mapFind { case (inference, extractionOption) =>
          for {
            (operator(identityConstant, variable: TermVariable), rhs) <- equality.unapply(extractionOption.conclusion)
            if rhs == variable && ExpressionUtils.isCombinationOfTermConstants(identityConstant)
          } yield LeftIdentity(operator, identityConstant, inference.summary, extractionOption)
        }
    }
    def findRightIdentity(operator: BinaryOperator, equality: Equality): Option[RightIdentity] = {
      rearrangementInferences
        .mapFind { case (inference, extractionOption) =>
          for {
            (operator(variable: TermVariable, identityConstant), rhs) <- equality.unapply(extractionOption.conclusion)
            if rhs == variable && ExpressionUtils.isCombinationOfTermConstants(identityConstant)
          } yield RightIdentity(operator, identityConstant, inference.summary, extractionOption)
        }
    }
    def findLeftAbsorber(operator: BinaryOperator, equality: Equality): Option[LeftAbsorber] = {
      rearrangementInferences
        .mapFind { case (inference, extractionOption) =>
          for {
            (operator(absorberConstant, _: TermVariable), rhs) <- equality.unapply(extractionOption.conclusion)
            if rhs == absorberConstant && ExpressionUtils.isCombinationOfTermConstants(absorberConstant)
          } yield LeftAbsorber(operator, absorberConstant, inference.summary, extractionOption)
        }
    }
    def findRightAbsorber(operator: BinaryOperator, equality: Equality): Option[RightAbsorber] = {
      rearrangementInferences
        .mapFind { case (inference, extractionOption) =>
          for {
            (operator(_: TermVariable, absorberConstant), rhs) <- equality.unapply(extractionOption.conclusion)
            if rhs == absorberConstant && ExpressionUtils.isCombinationOfTermConstants(absorberConstant)
          } yield RightAbsorber(operator, absorberConstant, inference.summary, extractionOption)
        }
    }
    for {
      equality <- equalityOption.toSeq
      (operator, commutativity) <- findCommutativities(equality)
      associativity <- findAssociativity(operator, equality)
    } yield RearrangeableOperator(
      operator,
      commutativity,
      associativity,
      findLeftIdentity(operator, equality),
      findRightIdentity(operator, equality),
      findLeftAbsorber(operator, equality),
      findRightAbsorber(operator, equality))
  }
  lazy val leftDistributivities: Seq[LeftDistributivity] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      (inference, extractionOption) <- rearrangementInferences
      (lhs, rhs) <- equality.unapply(extractionOption.conclusion).toSeq
      distributor <- rearrangeableOperators
      (TermVariable(a, Nil), lhsRhs) <- distributor.unapply(lhs).toSeq
      distributee <- rearrangeableOperators
      (TermVariable(b, Nil), TermVariable(c, Nil)) <- distributee.unapply(lhsRhs).toSeq
      if rhs == distributee(distributor(TermVariable(a, Nil), TermVariable(b, Nil)), distributor(TermVariable(a, Nil), TermVariable(c, Nil)))
    } yield LeftDistributivity(distributor, distributee, inference.summary, extractionOption)
  }
  lazy val rightDistributivities: Seq[RightDistributivity] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      (inference, extractionOption) <- rearrangementInferences
      (lhs, rhs) <- equality.unapply(extractionOption.conclusion).toSeq
      distributor <- rearrangeableOperators
      (lhsLhs, TermVariable(c, Nil)) <- distributor.unapply(lhs).toSeq
      distributee <- rearrangeableOperators
      (TermVariable(a, Nil), TermVariable(b, Nil)) <- distributee.unapply(lhsLhs).toSeq
      if rhs == distributee(distributor(TermVariable(a, Nil), TermVariable(c, Nil)), distributor(TermVariable(b, Nil), TermVariable(c, Nil)))
    } yield RightDistributivity(distributor, distributee, inference.summary, extractionOption)
  }

  def findRelation(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[BinaryRelationStatement] = {
    definedBinaryRelations.mapFind(relation => relation.unapply(statement).map { case (lhs, rhs) => BinaryRelationStatement(relation, lhs, rhs)(statement) })
  }

  lazy val premiseRelationSimplificationInferences: Map[BinaryRelation, Seq[PremiseRelationSimplificationInference]] = {
    def checkLhsIsValid(premiseLhs: Term, conclusionLhs: Term): Boolean = {
      // valid examples:
      //   a -> a
      //   a -> a_0
      //   (a, b) -> a
      (ExpressionUtils.isSimpleTermVariable(premiseLhs) && premiseLhs == conclusionLhs) || // a -> a
        ExpressionUtils.getSimpleTermVariable(premiseLhs).exists(ExpressionUtils.getWrappedSimpleTermVariable(conclusionLhs).contains) || // a -> a_0
        ExpressionUtils.getSimpleTermVariable(conclusionLhs).exists(premiseLhs.requiredSubstitutions.terms.map(_._1).contains) // (a, b) -> a
    }
    def checkRhsIsValidSimplification(premiseRhs: Term, conclusionRhs: Term): Boolean = {
      // valid examples:
      //   {a} -> a
      //   A x B -> A
      //   ℤ+ -> ℤ
      premiseRhs.complexity > conclusionRhs.complexity &&
        (ExpressionUtils.getSimpleTermVariable(conclusionRhs).exists(premiseRhs.requiredSubstitutions.terms.map(_._1).contains) ||
          ExpressionUtils.getTermConstantDefinition(conclusionRhs).exists(conclusionDefinition => ExpressionUtils.getTermConstantDefinition(premiseRhs).exists(premiseDefinition => premiseDefinition.definingStatement.referencedDefinitions.contains(conclusionDefinition))))
    }
    def getSubstitutionTermNames(t: Term): Set[String] = t.requiredSubstitutions.terms.map(_._1).toSet
    def checkNoSubstitutionOverlap(premiseLhs: Term, conclusionRhs: Term): Boolean = {
      (getSubstitutionTermNames(premiseLhs) intersect getSubstitutionTermNames(conclusionRhs)).isEmpty
    }

    implicit val substitutionContext = SubstitutionContext.outsideProof
    (for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      singlePremise <- extractionOption.premises.single.toSeq
      if singlePremise.requiredSubstitutions.contains(extractionOption.conclusion.requiredSubstitutions)
      if extractionOption.requiredSubstitutions.statements.isEmpty && extractionOption.requiredSubstitutions.hasNoApplications
      BinaryRelationStatement(_, conclusionLhs, conclusionRhs) <- findRelation(extractionOption.conclusion).toSeq
      BinaryRelationStatement(premiseRelation, premiseLhs, premiseRhs) <- findRelation(singlePremise).toSeq
      if checkLhsIsValid(premiseLhs, conclusionLhs) && checkRhsIsValidSimplification(premiseRhs, conclusionRhs) && checkNoSubstitutionOverlap(premiseLhs, conclusionRhs)
    } yield premiseRelation -> PremiseRelationSimplificationInference(inference, singlePremise, extractionOption.conclusion, extractionOption)).toSeqMap
  }

  lazy val relationRewriteInferences: Seq[RelationRewriteInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof

    def findPremiseRelation(lastPremise: Statement): Seq[(BinaryRelationStatement, Substitutions.Possible)] = {
      val relationsFromTemplate = definedBinaryRelations.ofType[BinaryRelationFromGeneralShorthand].mapCollect { relation =>
        val generalTemplate = relation.shorthand.template.expand(
          Map.empty,
          Map(
            relation.lhsVariableName -> TermVariable(relation.lhsVariableName),
            relation.rhsVariableName -> TermVariable(relation.rhsVariableName),
            relation.symbolVariableName -> TermVariable(relation.symbolVariableName))
        ).asInstanceOf[Statement]
        for {
          substitutions <- generalTemplate.calculateSubstitutions(lastPremise)
          lhsVariableName <- ExpressionUtils.getSimpleTermVariable(substitutions.terms(relation.lhsVariableName)._2)
          rhsVariableName <- ExpressionUtils.getSimpleTermVariable(substitutions.terms(relation.rhsVariableName)._2)
          symbolVariableName <- ExpressionUtils.getSimpleTermVariable(substitutions.terms(relation.symbolVariableName)._2)
        } yield (BinaryRelationStatement.construct(relation, TermVariable(lhsVariableName), TermVariable(rhsVariableName)), Substitutions.Possible(terms = Map(symbolVariableName -> (0, relation.definition()))))
      }
      if (relationsFromTemplate.nonEmpty)
        relationsFromTemplate
      else
        findRelation(lastPremise).toSeq.map(_ -> Substitutions.Possible.empty)
    }
    def areValidSecondaryComponents(premiseComponent: Term, conclusionComponent: Term): Boolean = {
      (ExpressionUtils.isSimpleTermVariable(premiseComponent) && ExpressionUtils.isSimpleTermVariable(conclusionComponent)) ||
        (premiseComponent.requiredSubstitutions.isEmpty && conclusionComponent.requiredSubstitutions.isEmpty)
    }
    def isValidRewrite(premise: BinaryRelationStatement, conclusion: BinaryRelationStatement): Boolean = {
      // e.g. a ∈ A -> a ∈ B
      // or   a ~ b -> a ≠ b
      // or   a ∈ ℤ+ -> 0 < a
      ExpressionUtils.isSimpleTermVariable(premise.left) &&
        ((premise.left == conclusion.left && areValidSecondaryComponents(premise.right, conclusion.right)) ||
        (premise.left == conclusion.right && areValidSecondaryComponents(premise.right, conclusion.left)))
    }
    def isValidInitialPremise(initialPremiseOption: Option[Statement], conclusion: BinaryRelationStatement) = {
      def isValidTypeStatement(initialPremise: Statement): Boolean = {
        ExpressionUtils.getTypeLikeStatement(initialPremise)(rootEntryContext).exists { typeStatement =>
          ExpressionUtils.isSimpleTermVariable(typeStatement.mainTerm)
        }
      }
      def isValidRelation(initialPremise: Statement): Boolean = {
        findRelation(initialPremise).exists { initialPremiseRelation =>
          ExpressionUtils.isSimpleTermVariable(initialPremiseRelation.left) &&
            ExpressionUtils.isSimpleTermVariable(initialPremiseRelation.right) &&
            initialPremiseRelation.relation != conclusion.relation
        }
      }
      initialPremiseOption match {
        case None =>
          true
        case Some(initialPremise) =>
          isValidTypeStatement(initialPremise) || isValidRelation(initialPremise)
      }
    }

    for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      (initialPremiseOption, mainPremiseStatement) <- extractionOption.premises match {
        case Seq(a) => Seq((None, a))
        case Seq(a, b) => Seq((Some(a), b))
        case _ => Nil
      }
      if extractionOption.premises.map(_.requiredSubstitutions).foldTogether.contains(extractionOption.conclusion.requiredSubstitutions)
      if (initialPremiseOption.toSeq :+ extractionOption.conclusion).map(_.requiredSubstitutions).foldTogether.contains(mainPremiseStatement.requiredSubstitutions)
      conclusion <- findRelation(extractionOption.conclusion).toSeq
      (mainPremise, initialSubstitutions) <- findPremiseRelation(mainPremiseStatement)
      if isValidRewrite(mainPremise, conclusion) && isValidInitialPremise(initialPremiseOption, conclusion)
    } yield RelationRewriteInference(inference, extractionOption, initialPremiseOption, mainPremiseStatement, mainPremise.relation, conclusion.relation, initialSubstitutions)
  }

  def getPossiblePremiseDesimplifications(premise: Statement): Seq[DerivedPremise] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    def directly = DirectPremise(premise)
    def byDesimplifying = for {
      inference <- conclusionSimplificationInferences
      substitutions <- inference.conclusion.calculateSubstitutions(premise).flatMap(_.confirmTotality).toSeq
      substitutedInferencePremises <- inference.premises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
      innerDesimplifications <- getPossiblePremiseDesimplifications(substitutedInferencePremises)
    } yield DesimplifiedPremise(premise, inference, innerDesimplifications)

    directly +: byDesimplifying
  }

  def getPossiblePremiseDesimplifications(premises: Seq[Statement]): Seq[Seq[DerivedPremise]] = {
    premises.foldProduct(getPossiblePremiseDesimplifications)
  }

  lazy val conclusionRelationSimplificationInferences: Map[BinaryRelation, Seq[ConclusionRelationSimplificationInference]] = {

    implicit val substitutionContext = SubstitutionContext.outsideProof

    def isValidSimplification(premises: Seq[BinaryRelationStatement], conclusion: BinaryRelationStatement, optionalTypeStatement: Option[TypeLikeStatement]): Boolean = {
      // e.g. a ∈ ℕ, b ∈ ℕ -> (a, b)ℤ ∈ ℤ
      // or a ∈ ℤ, b ∈ ℤ -> a + b ∈ ℤ
      val isLhsBreakdown: Boolean = {
        ExpressionUtils.isTermConstant(conclusion.right) && premises.forall(p => ExpressionUtils.isTermConstant(p.right)) &&
          premises.forall(p => p.left.complexity < conclusion.left.complexity) &&
          ExpressionUtils.getCombinationOfSimpleTermVariables(conclusion.left).exists { termVariables =>
            termVariables.zipStrict(premises).exists(_.forall { case (variableName, premise) => ExpressionUtils.getSimpleTermVariable(premise.left).contains(variableName)})
          }
      }
      // e.g. a ∈ A, a ∉ B -> a ∈ A/B
      val isRhsBreakdown: Boolean = {
        ExpressionUtils.getSimpleTermVariable(conclusion.left).exists(v => premises.forall(p => ExpressionUtils.getSimpleTermVariable(p.left).contains(v))) &&
          premises.forall(p => p.right.complexity < conclusion.right.complexity) &&
          ExpressionUtils.getCombinationOfSimpleTermVariables(conclusion.right).exists { termVariables =>
            termVariables.zipStrict(premises).exists(_.forall { case (variableName, premise) => ExpressionUtils.getSimpleTermVariable(premise.right).contains(variableName)})
          }
      }
      val isDoubleSimplification: Boolean = {
        // e.g. a ∈ ℕ, b ∈ ℕ, a = b -> aℤ = bℤ
        premises.lastOption.exists { lastPremise =>
          lastPremise.left.complexity < conclusion.left.complexity && lastPremise.right.complexity < conclusion.right.complexity &&
          ExpressionUtils.getSimpleTermVariable(lastPremise.left).exists { lastPremiseLhsVariable =>
            ExpressionUtils.getSimpleTermVariable(lastPremise.right).exists { lastPremiseRhsVariable =>
              ExpressionUtils.getSingleSimpleTermVariable(conclusion.left).contains(lastPremiseLhsVariable) &&
                ExpressionUtils.getSingleSimpleTermVariable(conclusion.right).contains(lastPremiseRhsVariable) &&
                premises.init.forall { premise =>
                  ExpressionUtils.getSimpleTermVariable(premise.left).exists(v => v == lastPremiseLhsVariable || v == lastPremiseRhsVariable) &&
                    ExpressionUtils.isTermConstant(premise.right)
                }
            }
          }
        }
      }
      val isTypeDefinitionSimplification: Boolean = {
        // e.g. f is a function, a ∈ domain(f) -> f(a) ∈ range(f)
        (for {
          typeStatement <- optionalTypeStatement
          typeVariable <- ExpressionUtils.getSimpleTermVariable(typeStatement.mainTerm)
          singlePremise <- premises.single
          conclusionLhsVariables <- ExpressionUtils.getCombinationOfSimpleTermVariables(conclusion.left)
          otherVariable <- conclusionLhsVariables match {
            case Seq(`typeVariable`, otherVariable) => Some(otherVariable)
            case Seq(otherVariable, `typeVariable`) => Some(otherVariable)
            case _ => None
          }
          if ExpressionUtils.getWrappedSimpleTermVariable(conclusion.right).contains(typeVariable)
          if ExpressionUtils.getWrappedSimpleTermVariable(singlePremise.right).contains(typeVariable)
          if ExpressionUtils.getSimpleTermVariable(singlePremise.left).contains(otherVariable)
        } yield true) getOrElse false
      }
      isLhsBreakdown || isRhsBreakdown || isDoubleSimplification || isTypeDefinitionSimplification
    }

    def breakOffTypeStatement(premises: Seq[Statement]): (Option[TypeLikeStatement], Seq[Statement]) = {
      premises.headAndTailOption
        .flatMap { case (head, tail) => ExpressionUtils.getTypeLikeStatement(head)(rootEntryContext).map(t => Some(t) -> tail)}
        .getOrElse(None -> premises)
    }

    (for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      if extractionOption.premises.nonEmpty && extractionOption.conclusion.requiredSubstitutions.contains(extractionOption.requiredSubstitutions)
      (optionalTypeStatement, otherPremises) = breakOffTypeStatement(extractionOption.premises)
      premiseDesimplifications <- getPossiblePremiseDesimplifications(otherPremises)
      premises <- premiseDesimplifications.flatMap(_.getRootPremises).map(findRelation).traverseOption.toSeq
      conclusion <- findRelation(extractionOption.conclusion).toSeq
      if isValidSimplification(premises, conclusion, optionalTypeStatement)
    } yield conclusion.relation -> ConclusionRelationSimplificationInference(inference, extractionOption, optionalTypeStatement, premiseDesimplifications)).toSeqMap
  }

  lazy val conclusionSimplificationInferences: Seq[Inference] = allInferences.filter {
    case inference @ Inference(_, premises, conclusion)
      if premises.nonEmpty &&
        premises.forall(_.complexity < conclusion.complexity) &&
        conclusion.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
        inference.requiredSubstitutions.hasNoApplications &&
        premises.forall(_.referencedDefinitions.subsetOf(conclusion.referencedDefinitions))
    =>
      true
    case _ =>
      false
  }

  lazy val termDefinitionRemovals: Map[TermDefinition, Seq[ExtractionOption]] = {
    rootEntryContext.termDefinitions.map { termDefinition =>
      termDefinition -> (for {
        extractionOption <- extractionOptionsByInferenceId(termDefinition.definitionInference.id)
        if extractionOption.conclusion.referencedDefinitions.contains(termDefinition) &&
          !extractionOption.premises.exists(_.referencedDefinitions.contains(termDefinition)) &&
          extractionOption.conclusion.requiredSubstitutions.contains(extractionOption.requiredSubstitutions)
      } yield extractionOption)
    }.toMap
  }

  lazy val rewriteInferences: Seq[(Inference, Statement)] = {
    inferenceEntries.collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity == singlePremise.complexity &&
          conclusion.requiredSubstitutions.isEquivalentTo(singlePremise.requiredSubstitutions) &&
          inference.requiredSubstitutions.hasNoApplications &&
          conclusion != singlePremise
      => (inference, singlePremise)
    }
  }

  def isValidSinglePremiseExtraction(premise: Statement, conclusion: Statement): Boolean = {
    def isSingleStatementSimplification = ExpressionUtils.getWrappedSimpleStatementVariable(premise).exists(ExpressionUtils.getWrappedSimpleStatementVariable(conclusion).contains)
    def isDoubleStatementSimplification = ExpressionUtils.getWrappedBinaryStatementVariables(premise).map(_.toSet).exists(ExpressionUtils.getWrappedBinaryStatementVariables(conclusion).map(_.toSet).contains)
    def isDoubleToSingleStatementSimplification = ExpressionUtils.getWrappedBinaryStatementVariables(premise).map(_.toSet).exists { v => ExpressionUtils.getWrappedSimpleStatementVariable(conclusion).exists(v.contains) }
    conclusion.complexity < premise.complexity && (isSingleStatementSimplification || isDoubleStatementSimplification || isDoubleToSingleStatementSimplification)
  }
  def isValidDoublePremiseExtraction(mainPremise: Statement, subsidiaryPremise: Statement, conclusion: Statement): Boolean = {
    conclusion.complexity < mainPremise.complexity && (for {
      subsidiaryPremiseVariable <- ExpressionUtils.getWrappedSimpleStatementVariable(subsidiaryPremise)
      conclusionVariable <- ExpressionUtils.getWrappedSimpleStatementVariable(conclusion)
      if subsidiaryPremiseVariable != conclusionVariable
      if ExpressionUtils.getWrappedBinaryStatementVariables(mainPremise).exists { t => t == (subsidiaryPremiseVariable, conclusionVariable) || t == (conclusionVariable, subsidiaryPremiseVariable)}
    } yield true).getOrElse(false)
  }

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Option[Statement])] = inferenceEntries.collect {
    case inference @ Inference(_, Seq(singlePremise), conclusion)
      if isValidSinglePremiseExtraction(singlePremise, conclusion)
    =>
      (inference, singlePremise, None)
    case inference @ Inference(_, Seq(firstPremise, secondPremise), conclusion) if isValidDoublePremiseExtraction(firstPremise, secondPremise, conclusion) =>
      (inference, firstPremise, Some(secondPremise))
  }

  lazy val termRewriteInferences: Seq[TermRewriteInference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId.get(inference.id).toSeq.flatten
      (lhs, rhs) <- equality.unapply(extractionOption.conclusion)
    } yield TermRewriteInference(inference, extractionOption, lhs, rhs)
  }
  lazy val prospectiveTermRewriteInferences: Seq[TermRewriteInference] = {
    termRewriteInferences.filter { case TermRewriteInference(_, extractionOption, left, right) =>
      left.requiredSubstitutions.contains(extractionOption.conclusion.requiredSubstitutions)
    }
  }
  lazy val termSimplificationInferences: Seq[TermRewriteInference] = {
    termRewriteInferences.filter { case TermRewriteInference(_, extractionOption, left, right) =>
      left.complexity > right.complexity && left.requiredSubstitutions.contains(extractionOption.conclusion.requiredSubstitutions)
    }
  }
  lazy val termDesimplificationInferences: Seq[TermRewriteInference] = {
    termRewriteInferences.filter { case TermRewriteInference(_, extractionOption, left, right) =>
      left.complexity < right.complexity && right.requiredSubstitutions.contains(extractionOption.conclusion.requiredSubstitutions)
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
          if (firstDefinition == conclusionDefinition && secondDefinition == conclusionDefinition)
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

  lazy val facts: Seq[DerivationStep] = {
    for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      if extractionOption.premises.isEmpty && extractionOption.requiredSubstitutions.isEmpty
      assertionStep = Step.Assertion(inference.conclusion, inference.summary, Nil, Substitutions.empty)
    } yield SubstatementExtractor.createDerivationForInferenceExtraction(assertionStep, extractionOption.derivation)(provingContext)
  }

  lazy val statementDeductionInferences: Seq[(Inference, Statement, Statement, String, String, Direction)] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      deductionDefinition <- rootEntryContext.deductionDefinitionOption.toSeq
      result <- for {
        inference <- allInferences
        Seq(firstPremise @ deductionDefinition(StatementVariable(a, Nil), StatementVariable(b, Nil)), otherPremise: DefinedStatement) <- Seq.unapplySeq(inference.premises).toSeq
        swapper <- Seq(Direction.Forward, Direction.Reverse)
        (premiseName, conclusionName) = swapper.swapSourceAndResult(a, b)
        if inference.requiredSubstitutions.terms.isEmpty && inference.requiredSubstitutions.hasNoApplications
        if otherPremise.requiredSubstitutions.statements.contains((premiseName, 0)) && inference.conclusion.requiredSubstitutions.statements.contains((conclusionName, 0))
        if otherPremise.applySubstitutions(Substitutions(otherPremise.requiredSubstitutions.statements.map { case (n, a) => n -> (a, StatementVariable(if (n == premiseName) conclusionName else n))}.toMap)).contains(inference.conclusion)
      } yield (inference, firstPremise, otherPremise, premiseName, conclusionName, swapper)
    } yield result
  }

  object WrappedStatementVariable {
    def unapply(statement: Statement): Option[String] = statement match {
      case DefinedStatement(Seq(StatementVariable(name, Nil)), _) => Some(name)
      case DefinedStatement(Seq(WrappedStatementVariable(name)), _) => Some(name)
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

}

object Definitions {
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
