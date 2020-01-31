package net.prover.model.definitions

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.entries.{ChapterEntry, DisplayShorthand, StatementDefinition, TermDefinition}
import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext
import net.prover.util.Swapper

import scala.Ordering.Implicits._
import scala.collection.mutable

case class Definitions(availableEntries: Seq[ChapterEntry]) extends EntryContext.EntryTypes {

  lazy val inferenceEntries: Seq[Inference] = availableEntries.ofType[Inference]

  val completenessByInference = mutable.Map.empty[String, Boolean]
  def isInferenceComplete(inference: Inference): Boolean = {
    completenessByInference.getOrElseUpdate(inference.id, inferences.find(_.id == inference.id).exists(_.isComplete(this)))
  }

  lazy val equalityOption: Option[Equality] = {
    for {
      definition <- statementDefinitions.find(_.attributes.contains("equality"))
      relation = BinaryRelation(definition.symbol, definition.defaultValue, definition.attributes)
      expansion <- expansions.ofType[RelationExpansion].find(e => e.sourceJoiner == relation && e.resultJoiner == relation)
      substitution <- substitutions.find(_.relation == relation)
      reversal <- reversals.ofType[Reversal[Term]].find(_.relation == relation)
      transitivity <- transitivities.ofType[Transitivity[Term]].find(_.statement == relation)
    } yield Equality(relation, expansion, substitution, reversal, transitivity)
  }

  lazy val definedBinaryStatements: Seq[BinaryJoiner[_ <: Expression]] = Definitions.getDefinedBinaryStatements(statementDefinitions, displayShorthands, termDefinitions)
  lazy val definedBinaryConnectives: Seq[BinaryConnective] = definedBinaryStatements.ofType[BinaryConnective]
  lazy val definedBinaryRelations: Seq[BinaryRelation] = definedBinaryStatements.ofType[BinaryRelation]

  lazy val reversals: Seq[Reversal[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      inference <- inferenceEntries
      relation <- definedBinaryStatements.find(r => r.unapply(inference.conclusion).nonEmpty)
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
      relation <- definedBinaryStatements.find(r => r.unapply(inference.conclusion).nonEmpty)
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

  lazy val premiseSimplificationInferences: Seq[(Inference, Statement)] = inferenceEntries.collect {
    case inference @ Inference(_, Seq(singlePremise), conclusion)
      if singlePremise.complexity > conclusion.complexity &&
        singlePremise.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
        inference.requiredSubstitutions.hasNoApplications
    =>
      (inference, singlePremise)
  }
  lazy val conclusionSimplificationInferences: Seq[Inference] = inferenceEntries.filter {
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

  lazy val deductionEliminationInferenceOption: Option[(Inference, Statement, Statement)] = {
    deductionDefinitionOption.flatMap { deductionDefinition =>
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
    scopingDefinitionOption.flatMap { scopingDefinition =>
      inferenceEntries.iterator.collect {
        case inference @ Inference(
          _,
          Seq(singlePremise @ scopingDefinition(StatementVariable(premisePredicateName, Seq(FunctionParameter(0, 0))))),
          StatementVariable(conclusionPredicateName, Seq(TermVariable(variableName, Nil)))
        ) if premisePredicateName == conclusionPredicateName =>
          (inference, singlePremise, premisePredicateName, variableName)
      }.headOption
    }
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
  lazy val termRewriteInferences: Seq[(Inference, Term, Term)] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      result <- inferences
        .collect {
          case inference @ Inference(
          _,
          _,
          equality(left: Term, right: Term)) =>
            (inference, left, right)
        }
    } yield result
  }
  lazy val termSimplificationInferences: Seq[(Inference, Term, Term)] = {
    termRewriteInferences.filter { case (inference, left, right) =>
      left.complexity > right.complexity && left.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions)
    }
  }
  lazy val termDesimplificationInferences: Seq[(Inference, Term, Term)] = {
    termRewriteInferences.filter { case (inference, left, right) =>
      left.complexity < right.complexity && right.requiredSubstitutions.contains(inference.conclusion.requiredSubstitutions)
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
            statementDefinition <- statementDefinitions.find(_.unapplySeq(inference.conclusion).nonEmpty)
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
  lazy val statementDefinitionDeconstructions: Map[StatementDefinition, Inference] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    @scala.annotation.tailrec
    def helper(
      remainingInferences: Seq[Inference],
      acc: Map[StatementDefinition, Inference]
    ): Map[StatementDefinition, Inference] = {
      remainingInferences match {
        case Nil =>
          acc
        case inference +: tailInferences =>
          val updated = (for {
            (_, premises, DefinedStatement(components, definition)) <- Inference.unapply(inference)
            if components.map(_.asOptionalInstanceOf[StatementVariable]).traverseOption.contains(premises)
          } yield acc.updated(definition, inference)).getOrElse(acc)
          helper(tailInferences, updated)
      }
    }
    helper(inferenceEntries, Map.empty)
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

  lazy val facts: Seq[Inference] = {
    inferences.collect {
      case inference if inference.premises.isEmpty && inference.requiredSubstitutions.isEmpty =>
        inference
    }
  }

  lazy val statementDeductionInferences: Seq[(Inference, Statement, Statement, String, String, Swapper)] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    for {
      deduction <- deductionDefinitionOption.toSeq
      result <- for {
        inference <- inferences
        Seq(firstPremise @ deduction(StatementVariable(a, Nil), StatementVariable(b, Nil)), otherPremise: DefinedStatement) <- Seq.unapplySeq(inference.premises).toSeq
        swapper <- Seq(Swapper.DontSwap, Swapper.Swap)
        (premiseName, conclusionName) = swapper.swap(a, b)
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
    inferences.collect {
      case inference @ Inference(_, Seq(premise @ StatementVariable(name, Nil)), WrappedStatementVariable(conclusionName))
        if conclusionName == name
      => (inference, premise)
    }
  }
  lazy val statementDefinitionEliminationInferences: Seq[(Inference, Statement)] = {
    inferences.collect {
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

    def fromShorthands = for {
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

    fromDefinitions ++ fromShorthands
  }
}
