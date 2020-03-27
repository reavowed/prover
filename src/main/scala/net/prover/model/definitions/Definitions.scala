package net.prover.model.definitions

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.entries.{DisplayShorthand, StatementDefinition, TermDefinition}
import net.prover.model.expressions._
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{Step, SubstatementExtractor, SubstitutionContext}
import net.prover.util.Direction

import scala.Ordering.Implicits._
import scala.collection.mutable
import scala.util.Try

case class Definitions(rootEntryContext: EntryContext) {

  lazy val inferenceEntries: Seq[Inference] = rootEntryContext.availableEntries.ofType[Inference]
  private val provingContext: ProvingContext = ProvingContext(rootEntryContext, this)

  val completenessByInference = mutable.Map.empty[String, Boolean]
  def isInferenceComplete(inference: Inference): Boolean = {
    completenessByInference.getOrElseUpdate(inference.id, rootEntryContext.allInferences.find(_.id == inference.id).exists(_.isComplete(this)))
  }

  lazy val extractionOptionsByInferenceId: Map[String, Seq[ExtractionOption]] = {
    rootEntryContext.allInferences.map { i =>
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
        Seq(singlePremise @ generalizationDefinition(StatementVariable(premisePredicateName, Seq(FunctionParameter(0, 0))))),
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
    for {
      inference <- inferenceEntries
      relation <- definedBinaryStatements
      if (inference match {
        case Inference(
          _,
          Seq(relation(ExpressionVariable(a, Nil), ExpressionVariable(b, Nil)), relation(ExpressionVariable(c, Nil), ExpressionVariable(d, Nil))),
          relation(ExpressionVariable(e, Nil), ExpressionVariable(f, Nil))
        ) if a == e && b == c && d == f =>
          true
        case _ =>
          false
      })
    } yield relation.transitivity(inference.summary)
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
      relation = BinaryRelation(definition.symbol, definition.defaultValue, definition.attributes)
      expansion <- expansions.ofType[RelationExpansion].find(e => e.sourceJoiner == relation && e.resultJoiner == relation)
      substitution <- substitutions.find(_.relation == relation)
      reversal <- reversals.ofType[Reversal[Term]].find(_.relation == relation)
      transitivity <- transitivities.ofType[Transitivity[Term]].find(_.statement == relation)
    } yield Equality(relation, expansion, substitution, reversal, transitivity)
  }

  lazy val generalizationDistributions: Map[BinaryJoiner[Statement], Inference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      generalizationDefinition <- rootEntryContext.generalizationDefinitionOption.toSeq
      inference <- inferenceEntries
      connective <- definedBinaryConnectives
      (generalizationDefinition(StatementVariable(a, Seq(FunctionParameter(0, 0)))), generalizationDefinition(StatementVariable(b, Seq(FunctionParameter(0, 0))))) <- connective.unapply(inference.conclusion)
      if inference.premises == Seq(generalizationDefinition(connective(StatementVariable(a, Seq(FunctionParameter(0, 0))), StatementVariable(b, Seq(FunctionParameter(0, 0))))(SubstitutionContext.withExtraParameters(1))))
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

  lazy val premiseRelationLeftHandSimplificationInferences: Seq[PremiseRelationLeftHandSimplificationInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- rootEntryContext.allInferences
      if inference.premises.length <= 1
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      premise <- extractionOption.premises.single.toSeq
      if premise.requiredSubstitutions.contains(extractionOption.conclusion.requiredSubstitutions)
      conclusionRelation <- definedBinaryRelations
      (conclusionLhs, conclusionRhs) <- conclusionRelation.unapply(extractionOption.conclusion).toSeq
      premiseRelation <- definedBinaryRelations
      (premiseLhs, premiseRhs) <- premiseRelation.unapply(premise)
      if conclusionLhs.complexity >= premiseLhs.complexity && conclusionLhs.definitionUsages.contains(premiseLhs.definitionUsages)
      if premiseRhs.complexity > conclusionRhs.complexity && premiseRhs.definitionUsages.contains(conclusionRhs.definitionUsages)
    } yield PremiseRelationLeftHandSimplificationInference(inference, premise, extractionOption.conclusion, extractionOption.extractionInferences)
  }

  lazy val premiseRelationDoubleSimplificationInferences: Seq[PremiseRelationDoubleSimplificationInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- rootEntryContext.allInferences
      if inference.premises.length <= 1
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      premise <- extractionOption.premises.single.toSeq
      conclusionRelation <- definedBinaryRelations
      (conclusionLhs, conclusionRhs) <- conclusionRelation.unapply(extractionOption.conclusion).toSeq
      premiseRelation <- definedBinaryRelations
      (premiseLhs, premiseRhs) <- premiseRelation.unapply(premise)
      if premiseLhs.complexity > conclusionLhs.complexity && premiseRhs.complexity > conclusionRhs.complexity
    } yield PremiseRelationDoubleSimplificationInference(inference, premise, extractionOption.conclusion, extractionOption.extractionInferences)
  }

  lazy val conclusionRelationSimplificationInferences: Seq[ConclusionRelationSimplificationInference] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof

    def splitPremises(currentPremises: Seq[Statement], premisesToRecurseOn: Seq[(Statement, BinaryRelation)], conclusionLhs: Term, conclusionRhs: Term): (Seq[Statement], Seq[(Statement, BinaryRelation)]) = {
      currentPremises match {
        case initPremises :+ premise =>
          (for {
            premiseRelation <- definedBinaryRelations
            (TermVariable(name, Nil), premiseRhs) <- premiseRelation.unapply(premise)
            if conclusionLhs.requiredSubstitutions.terms.contains((name, 0)) && premiseRhs.complexity <= conclusionRhs.complexity
          } yield (premise, premiseRelation)).headOption match {
            case Some((premise, premiseRelation)) =>
              splitPremises(initPremises, (premise, premiseRelation) +: premisesToRecurseOn, conclusionLhs, conclusionRhs)
            case None =>
              (currentPremises, premisesToRecurseOn)
          }
        case _ =>
          (currentPremises, premisesToRecurseOn)
      }
    }

    for {
      inference <- rootEntryContext.allInferences
      if inference.requiredSubstitutions.hasNoApplications
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      if extractionOption.premises.nonEmpty
      conclusionRelation <- definedBinaryRelations
      (conclusionLhs, conclusionRhs) <- conclusionRelation.unapply(extractionOption.conclusion).toSeq
      (premisesThatMustBePresent, premisesToRecurseOn) = splitPremises(extractionOption.premises, Nil, conclusionLhs, conclusionRhs)
      if premisesToRecurseOn.nonEmpty
    } yield ConclusionRelationSimplificationInference(inference, extractionOption, premisesThatMustBePresent, premisesToRecurseOn, conclusionRelation)
  }

  lazy val premiseSimplificationInferences: Seq[(Inference, Statement)] = rootEntryContext.allInferences.collect {
    case inference @ Inference(_, Seq(singlePremise), conclusion)
      if singlePremise.complexity > conclusion.complexity &&
        singlePremise.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
        inference.requiredSubstitutions.hasNoApplications
    =>
      (inference, singlePremise)
  }
  lazy val conclusionSimplificationInferences: Seq[Inference] = rootEntryContext.allInferences.filter {
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
      inference <- rootEntryContext.allInferences
      extractionOption <- extractionOptionsByInferenceId.get(inference.id).toSeq.flatten
      (lhs, rhs) <- equality.unapply(extractionOption.conclusion)
    } yield TermRewriteInference(inference, extractionOption, lhs, rhs)
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
  lazy val statementDefinitionSimplifications: Map[StatementDefinition, Seq[(Inference, Statement, Expression)]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    @scala.annotation.tailrec
    def helper(
      remainingInferences: Seq[Inference],
      acc: Map[StatementDefinition, Seq[(Inference, Statement, Expression)]]
    ): Map[StatementDefinition, Seq[(Inference, Statement, Expression)]] = {
      remainingInferences match {
        case Nil =>
          acc
        case inference +: tailInferences =>
          val updated = (for {
            statementDefinition <- rootEntryContext.statementDefinitions.find(_.unapplySeq(inference.conclusion).nonEmpty)
            (singlePremise, firstConclusionComponent) <- inference match {
              case Inference(_, Seq(singlePremise @ statementDefinition(firstPremiseComponent, _*)), statementDefinition(firstConclusionComponent, _*))
                if firstConclusionComponent.complexity > firstPremiseComponent.complexity &&
                  firstConclusionComponent.requiredSubstitutions.contains(firstPremiseComponent.requiredSubstitutions) &&
                  singlePremise.requiredSubstitutions.contains(inference.requiredSubstitutions)
              =>
                Some((singlePremise, firstConclusionComponent))
              case _ =>
                None
            }
            result = acc.updated(statementDefinition, acc.getOrElse(statementDefinition, Nil) :+ (inference, singlePremise, firstConclusionComponent))
          } yield result).getOrElse(acc)
          helper(tailInferences, updated)
      }
    }
    helper(inferenceEntries, Map.empty)
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
      inference <- rootEntryContext.allInferences
      extractionOption <- extractionOptionsByInferenceId(inference.id)
      if extractionOption.premises.isEmpty && extractionOption.requiredSubstitutions.isEmpty
    } yield (extractionOption.conclusion, inference, extractionOption)
  }

  lazy val statementDeductionInferences: Seq[(Inference, Statement, Statement, String, String, Direction)] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      deduction <- rootEntryContext.deductionDefinitionOption.toSeq
      result <- for {
        inference <- rootEntryContext.allInferences
        Seq(firstPremise @ deduction(StatementVariable(a, Nil), StatementVariable(b, Nil)), otherPremise: DefinedStatement) <- Seq.unapplySeq(inference.premises).toSeq
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
    rootEntryContext.allInferences.collect {
      case inference @ Inference(_, Seq(premise @ StatementVariable(name, Nil)), WrappedStatementVariable(conclusionName))
        if conclusionName == name
      => (inference, premise)
    }
  }
  lazy val statementDefinitionEliminationInferences: Seq[(Inference, Statement)] = {
    rootEntryContext.allInferences.collect {
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
      if definition.format.baseFormatString == s"%0 ${definition.symbol} %1"
      constructor <- definition.componentTypes match {
        case Seq(_: StatementComponent, _: StatementComponent) => Some(BinaryConnective.apply _)
        case Seq(_: TermComponent, _: TermComponent) => Some(BinaryRelation.apply _)
        case _ => None
      }
    } yield constructor(definition.symbol, definition.defaultValue, definition.attributes)

    def fromGeneralShorthands = for {
      shorthand <- shorthands
      if shorthand.template.isInstanceOf[Template.DefinedStatement]
      if shorthand.template.variables.length == 3
      Seq(lhsIndex, symbolIndex, rhsIndex) <- "%(\\d) %(\\d) %(\\d)".r.unapplySeq(shorthand.format.baseFormatString).map(_.map(_.toInt)).toSeq
      if lhsIndex != symbolIndex && lhsIndex != rhsIndex && symbolIndex != rhsIndex
      lhsVariable = shorthand.template.variables(lhsIndex)
      symbolVariable = shorthand.template.variables(symbolIndex)
      rhsVariable = shorthand.template.variables(rhsIndex)
      if lhsVariable.isInstanceOf[Template.TermVariable] && symbolVariable.isInstanceOf[Template.TermVariable] && rhsVariable.isInstanceOf[Template.TermVariable]
      if shorthand.conditions.forall(_._1 == symbolVariable.name)
      definition <- termDefinitions
      if definition.componentTypes.isEmpty
      if shorthand.conditions.map(_._2).forall(definition.attributes.contains)
      relation = BinaryRelation(
        definition.symbol,
        shorthand.template.expand(
          Map.empty,
          Map(
            lhsVariable.name -> TermVariable(lhsVariable.name),
            rhsVariable.name -> TermVariable(rhsVariable.name),
            symbolVariable.name -> definition.defaultValue)
        ).asInstanceOf[Statement],
        Nil)
    } yield relation

    def fromSpecificShorthands = for {
      shorthand <- shorthands
      if shorthand.template.isInstanceOf[Template.DefinedStatement]
      if shorthand.template.variables.length == 2
      Seq(lhsIndexText, symbol, rhsIndexText) <- "%(\\d) (.) %(\\d)".r.unapplySeq(shorthand.format.baseFormatString).toSeq
      lhsIndex <- Try(lhsIndexText.toInt).toOption
      rhsIndex <- Try(rhsIndexText.toInt).toOption
      if lhsIndex != rhsIndex
      lhsVariable = shorthand.template.variables(lhsIndex)
      rhsVariable = shorthand.template.variables(rhsIndex)
      if lhsVariable.isInstanceOf[Template.TermVariable] && rhsVariable.isInstanceOf[Template.TermVariable]
      if shorthand.conditions.isEmpty
      relation = BinaryRelation(
        symbol,
        shorthand.template.expand(
          Map.empty,
          Map(
            lhsVariable.name -> TermVariable(lhsVariable.name),
            rhsVariable.name -> TermVariable(rhsVariable.name))
        ).asInstanceOf[Statement],
        Nil)
    } yield relation

    (fromDefinitions ++ fromGeneralShorthands ++ fromSpecificShorthands).reverse
  }
}
