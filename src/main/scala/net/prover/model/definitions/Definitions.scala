package net.prover.model.definitions

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions._
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{SubstatementExtractor, SubstitutionContext}
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

  lazy val rearrangeableFunctions: Seq[(BinaryOperator, Commutativity, Associativity)] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      appropriateInferences = inferenceEntries.filter { inference =>
        val substitutions = inference.requiredSubstitutions
        substitutions.statements.isEmpty &&
          substitutions.hasNoApplications &&
          inference.conclusion.requiredSubstitutions.isEquivalentTo(substitutions)
      }
      results <- appropriateInferences
        .mapCollect { inference =>
          for {
            (l, r) <- equality.unapply(inference.conclusion)
            operator = BinaryOperator(l)
            (first: TermVariable, second: TermVariable) <- operator.unapply(l)
            if r == operator(second, first)
          } yield (operator, Commutativity(operator, inference.summary, equality))
        }
        .mapCollect { case (operator, commutativity) =>
          appropriateInferences
            .mapFind { inference =>
              for {
                (l, r) <- equality.unapply(inference.conclusion)
                (a: TermVariable, bc) <- operator.unapply(l)
                (b: TermVariable, c: TermVariable) <- operator.unapply(bc)
                if r == operator(operator(a, b), c)
              } yield (operator, commutativity, Associativity(operator, inference.summary, equality))
            }
        }
    } yield results
  }

  def getSimpleTermVariable(term: Term): Option[String] = {
    term.asOptionalInstanceOf[TermVariable].filter(_.arguments.isEmpty).map(_.name)
  }
  def isSimpleTermVariable(term: Term): Boolean = {
    getSimpleTermVariable(term).isDefined
  }
  def getSimpleTermDefinition(term: Term): Option[TermDefinition] = {
    term.asOptionalInstanceOf[DefinedTerm].filter(_.components.isEmpty).map(_.definition)
  }
  def isSimpleTermDefinition(term: Term): Boolean = {
    getSimpleTermDefinition(term).isDefined
  }
  def getWrappedSimpleTermVariable(term: Term): Option[String] = {
    getSimpleTermVariable(term) orElse
      term.asOptionalInstanceOf[DefinedTerm].flatMap(_.components.single).flatMap(_.asOptionalInstanceOf[Term]).flatMap(getWrappedSimpleTermVariable)
  }
  def findRelation(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(BinaryRelation, Term, Term)] = {
    definedBinaryRelations.mapFind(relation => relation.unapply(statement).map { case (lhs, rhs) => (relation, lhs, rhs) })
  }

  lazy val premiseRelationSimplificationInferences: Seq[PremiseRelationSimplificationInference] = {
    def checkLhsIsValid(premiseLhs: Term, conclusionLhs: Term): Boolean = {
      // valid examples:
      //   a -> a
      //   a -> a_0
      //   (a, b) -> a
      (isSimpleTermVariable(premiseLhs) && premiseLhs == conclusionLhs) || // a -> a
        getSimpleTermVariable(premiseLhs).exists(getWrappedSimpleTermVariable(conclusionLhs).contains) || // a -> a_0
        getSimpleTermVariable(conclusionLhs).exists(premiseLhs.requiredSubstitutions.terms.map(_._1).contains) // (a, b) -> a
    }
    def checkRhsIsValidSimplification(premiseRhs: Term, conclusionRhs: Term): Boolean = {
      // valid examples:
      //   {a} -> a
      //   A x B -> A
      //   ℤ+ -> ℤ
      premiseRhs.complexity > conclusionRhs.complexity &&
        (getSimpleTermVariable(conclusionRhs).exists(premiseRhs.requiredSubstitutions.terms.map(_._1).contains) ||
          getSimpleTermDefinition(conclusionRhs).exists(conclusionDefinition => getSimpleTermDefinition(premiseRhs).exists(premiseDefinition => premiseDefinition.definingStatement.referencedDefinitions.contains(conclusionDefinition))))
    }
    def getSubstitutionTermNames(t: Term): Set[String] = t.requiredSubstitutions.terms.map(_._1).toSet
    def checkNoSubstitutionOverlap(premiseLhs: Term, conclusionRhs: Term): Boolean = {
      (getSubstitutionTermNames(premiseLhs) intersect getSubstitutionTermNames(conclusionRhs)).isEmpty
    }

    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      singlePremise <- extractionOption.premises.single.toSeq
      if singlePremise.requiredSubstitutions.contains(extractionOption.conclusion.requiredSubstitutions)
      if extractionOption.requiredSubstitutions.statements.isEmpty && extractionOption.requiredSubstitutions.hasNoApplications
      (_, conclusionLhs, conclusionRhs) <- findRelation(extractionOption.conclusion).toSeq
      (_, premiseLhs, premiseRhs) <- findRelation(singlePremise).toSeq
      if checkLhsIsValid(premiseLhs, conclusionLhs) && checkRhsIsValidSimplification(premiseRhs, conclusionRhs) && checkNoSubstitutionOverlap(premiseLhs, conclusionRhs)
    } yield PremiseRelationSimplificationInference(inference, singlePremise, extractionOption.conclusion, extractionOption)
  }

  lazy val premiseRelationRewriteInferences: Seq[PremiseRelationRewriteInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof

    def findPremiseRelation(lastPremise: Statement): Seq[(BinaryRelation, Term, Term, Substitutions.Possible)] = {
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
          lhsVariableName <- getSimpleTermVariable(substitutions.terms(relation.lhsVariableName)._2)
          rhsVariableName <- getSimpleTermVariable(substitutions.terms(relation.rhsVariableName)._2)
          symbolVariableName <- getSimpleTermVariable(substitutions.terms(relation.symbolVariableName)._2)
        } yield (relation, TermVariable(lhsVariableName), TermVariable(rhsVariableName), Substitutions.Possible(terms = Map(symbolVariableName -> (0, relation.definition()))))
      }
      if (relationsFromTemplate.nonEmpty)
        relationsFromTemplate
      else
        findRelation(lastPremise).toSeq.map { case (relation, lhs, rhs) => (relation, lhs, rhs, Substitutions.Possible.empty) }
    }
    def areValidSecondaryComponents(premiseComponent: Term, conclusionComponent: Term): Boolean = {
      (isSimpleTermVariable(premiseComponent) && isSimpleTermVariable(conclusionComponent)) ||
        (premiseComponent.requiredSubstitutions.isEmpty && conclusionComponent.requiredSubstitutions.isEmpty)
    }
    def componentsAreValid(premiseLhs: Term, premiseRhs: Term, conclusionLhs: Term, conclusionRhs: Term): Boolean = {
      isSimpleTermVariable(premiseLhs) &&
        ((conclusionLhs == premiseLhs && areValidSecondaryComponents(premiseRhs, conclusionRhs)) ||
        (conclusionRhs == premiseLhs && areValidSecondaryComponents(premiseRhs, conclusionLhs)))
    }

    for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      (initialPremiseOption, lastPremise) <- extractionOption.premises match {
        case Seq(a) => Seq((None, a))
        case Seq(a, b) => Seq((Some(a), b))
        case _ => Nil
      }
      if extractionOption.premises.map(_.requiredSubstitutions).foldTogether.contains(extractionOption.conclusion.requiredSubstitutions)
      (conclusionRelation, conclusionLhs, conclusionRhs) <- findRelation(extractionOption.conclusion).toSeq
      (premiseRelation, premiseLhs, premiseRhs, initialSubstitutions) <- findPremiseRelation(lastPremise)
      if componentsAreValid(premiseLhs, premiseRhs, conclusionLhs, conclusionRhs) &&
        !initialPremiseOption.exists(initialPremise => premiseRelation.unapply(initialPremise).nonEmpty || conclusionRelation.unapply(initialPremise).nonEmpty) &&
        (initialPremiseOption.isDefined || premiseRelation != conclusionRelation)
    } yield PremiseRelationRewriteInference(inference, initialPremiseOption, lastPremise, extractionOption.conclusion, extractionOption, initialSubstitutions)
  }

  def getPossiblePremiseDesimplifications(premise: Statement): Seq[PremiseDesimplification] = {
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

  def getPossiblePremiseDesimplifications(premises: Seq[Statement]): Seq[Seq[PremiseDesimplification]] = {
    premises.foldProduct(getPossiblePremiseDesimplifications)
  }

  lazy val conclusionRelationSimplificationInferences: Seq[ConclusionRelationSimplificationInference] = {

    implicit val substitutionContext = SubstitutionContext.outsideProof

    def isLeftSimplification(conclusionLhs: Term, conclusionRhs: Term, premises: Seq[Statement]): Boolean = {
      if (isSimpleTermVariable(conclusionRhs))
        premises.forall { premise =>
          findRelation(premise).exists { case (_, premiseLhs, premiseRhs) =>
            isSimpleTermVariable(premiseLhs) && premiseRhs == conclusionRhs && premiseLhs.complexity < conclusionLhs.complexity
          }
        }
      else if (isSimpleTermDefinition(conclusionRhs))
        premises.forall { premise =>
          findRelation(premise).exists { case (_, premiseLhs, premiseRhs) =>
            isSimpleTermVariable(premiseLhs) && isSimpleTermDefinition(premiseRhs) && premiseLhs.complexity < conclusionLhs.complexity
          }
        }
      else false
    }
    def isRightSimplification(conclusionLhs: Term, conclusionRhs: Term, premises: Seq[Statement]): Boolean = {
      if (isSimpleTermVariable(conclusionLhs))
        premises.forall { premise =>
          findRelation(premise).exists { case (_, premiseLhs, premiseRhs) =>
            premiseLhs == conclusionLhs && isSimpleTermVariable(premiseRhs) && premiseRhs.complexity < conclusionRhs.complexity
          }
        }
      else if (isSimpleTermDefinition(conclusionLhs))
        premises.forall { premise =>
          findRelation(premise).exists { case (_, premiseLhs, premiseRhs) =>
            isSimpleTermDefinition(premiseLhs) && isSimpleTermVariable(premiseRhs) && premiseRhs.complexity < conclusionRhs.complexity
          }
        }
      else false
    }
    def isDoubleSidedSimplification(conclusionLhs: Term, conclusionRhs: Term, premises: Seq[Statement]): Boolean = {
      premises.lastOption.exists { lastPremise =>
        definedBinaryRelations.exists { lastPremiseRelation =>
          lastPremiseRelation.unapply(lastPremise).exists { case (lastPremiseLhs, lastPremiseRhs) =>
            getSimpleTermVariable(lastPremiseLhs).exists { lastPremiseLhsVariable =>
              getSimpleTermVariable(lastPremiseRhs).exists { lastPremiseRhsVariable =>
                lastPremiseLhs.complexity < conclusionLhs.complexity &&
                  lastPremiseRhs.complexity < conclusionRhs.complexity &&
                  conclusionLhs.requiredSubstitutions == Substitutions.Required(Nil, Seq(lastPremiseLhsVariable -> 0)) &&
                  conclusionRhs.requiredSubstitutions == Substitutions.Required(Nil, Seq(lastPremiseRhsVariable -> 0)) &&
                  premises.init.forall { premise =>
                    findRelation(premise).exists { case (_, premiseLhs, premiseRhs) =>
                      getSimpleTermVariable(premiseLhs).exists(v => v == lastPremiseLhsVariable || v == lastPremiseRhsVariable) &&
                        isSimpleTermDefinition(premiseRhs)
                    }
                  }
              }
            }
          }
        }
      }
    }

    for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      premiseDesimplifications <- getPossiblePremiseDesimplifications(extractionOption.premises)
      premises = premiseDesimplifications.flatMap(_.getRootPremises)
      if premises.nonEmpty && extractionOption.conclusion.requiredSubstitutions.contains(extractionOption.requiredSubstitutions)
      (conclusionLhs, conclusionRhs) <- definedBinaryRelations.mapFind(_.unapply(extractionOption.conclusion)).toSeq
      if isLeftSimplification(conclusionLhs, conclusionRhs, premises) ||
        isRightSimplification(conclusionLhs, conclusionRhs, premises) ||
        isDoubleSidedSimplification(conclusionLhs, conclusionRhs, premises)
    } yield ConclusionRelationSimplificationInference(inference, extractionOption, premiseDesimplifications)
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

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Option[Statement])] = inferenceEntries.collectOption {
    case inference @ Inference(_, firstPremise +: otherPremises, conclusion)
      if inference.requiredSubstitutions.copy(statements = Nil).isEmpty &&
        firstPremise.requiredSubstitutions.contains(inference.requiredSubstitutions) &&
        conclusion.complexity < firstPremise.complexity &&
        otherPremises.forall(_.complexity < firstPremise.complexity)
    =>
      otherPremises match {
        case Nil =>
          Some((inference, firstPremise, None))
        case Seq(single) =>
          Some((inference, firstPremise, Some(single)))
        case _ =>
          None
      }
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

  lazy val facts: Seq[(Statement, Inference, ExtractionOption)] = {
    for {
      inference <- allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      if extractionOption.premises.isEmpty && extractionOption.requiredSubstitutions.isEmpty
    } yield (extractionOption.conclusion, inference, extractionOption)
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
