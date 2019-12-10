package net.prover.model.definitions

import net.prover.model.definitions.Definitions.RelationDefinitions
import net.prover.model._
import net.prover.model.entries.{ChapterEntry, DisplayShorthand, StatementDefinition, TermDefinition}
import net.prover.model.expressions.{DefinedStatement, Expression, ExpressionVariable, FunctionApplication, PredicateApplication, Statement, StatementVariable, Template, Term, TermVariable}
import net.prover.model.proof.SubstitutionContext

case class Definitions(availableEntries: Seq[ChapterEntry]) extends EntryContext.EntryTypes {

  lazy val inferenceEntries: Seq[Inference] = availableEntries.ofType[Inference]

  lazy val equalityOption: Option[Equality] = {
    for {
      definition <- statementDefinitions.find(_.attributes.contains("equality"))
      relation = BinaryRelation(definition.defaultValue)
      definitions <- definitionsByRelation.get(relation)
      expansion <- definitions.expansion
      substitution <- definitions.substitution
      reversal <- definitions.reversal
      transitivity <- definitions.transitivity
    } yield Equality(relation, expansion, substitution, reversal, transitivity)
  }

  lazy val definedBinaryRelations: Seq[(String, BinaryRelation)] = Definitions.getDefinedBinaryRelations(statementDefinitions, displayShorthands, termDefinitions)
  lazy val definitionsByRelation: Map[BinaryRelation, RelationDefinitions] = {
    @scala.annotation.tailrec
    def helper(remainingInferences: Seq[Inference], acc: Map[BinaryRelation, RelationDefinitions]): Map[BinaryRelation, RelationDefinitions] = {
      remainingInferences match {
        case Nil =>
          acc
        case inference +: tailInferences =>
          val afterReversal = findReversal(inference, acc)
          val afterTransitivity = findTransitivity(inference, afterReversal)
          val afterExpansion = findExpansion(inference, afterTransitivity)
          val afterSubstitution = findSubstitution(inference, afterExpansion)
          helper(tailInferences, afterSubstitution)
      }
    }
    helper(inferenceEntries, definedBinaryRelations.map(_._2).map(r => r -> RelationDefinitions(None, None, None, None)).toMap)
  }

  private def findReversal(inference: Inference, definitionsByRelation: Map[BinaryRelation, RelationDefinitions]): Map[BinaryRelation, RelationDefinitions] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      relation <- definitionsByRelation.keys.find(r => r.unapply(inference.conclusion).nonEmpty)
      if (inference match {
        case Inference(
          _,
          Seq(relation(ExpressionVariable(a), ExpressionVariable(b))),
          relation(ExpressionVariable(c), ExpressionVariable(d))
        ) if a == d && b == c =>
          true
        case _ =>
          false
      })
    } yield definitionsByRelation.replace(relation, _.copy(reversal = Some(Reversal(relation, inference.summary))))) getOrElse definitionsByRelation
  }
  private def findTransitivity(inference: Inference, definitionsByRelation: Map[BinaryRelation, RelationDefinitions]): Map[BinaryRelation, RelationDefinitions] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      relation <- definitionsByRelation.keys.find(r => r.unapply(inference.conclusion).nonEmpty)
      if (inference match {
        case Inference(
          _,
          Seq(relation(ExpressionVariable(a), ExpressionVariable(b)), relation(ExpressionVariable(c), ExpressionVariable(d))),
          relation(ExpressionVariable(e), ExpressionVariable(f))
        ) if a == e && b == c && d == f =>
          true
        case _ =>
          false
      })
    } yield definitionsByRelation.replace(relation, _.copy(transitivity = Some(Transitivity(relation, inference.summary))))) getOrElse definitionsByRelation
  }
  private def findExpansion(inference: Inference, definitionsByRelation: Map[BinaryRelation, RelationDefinitions]): Map[BinaryRelation, RelationDefinitions] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      relation <- inference.premises.headOption.flatMap(p => definitionsByRelation.keys.find(r => r.unapply(p).nonEmpty))
      if (inference match {
        case Inference(
          _,
          Seq(relation(TermVariable(a), TermVariable(b))),
          relation(FunctionApplication(f, Seq(TermVariable(c))), FunctionApplication(g, Seq(TermVariable(d))))
        ) if a == c && b == d && f == g =>
          true
        case _ =>
          false
      })
    } yield definitionsByRelation.replace(relation, _.copy(expansion = Some(Expansion(relation, inference.summary))))) getOrElse definitionsByRelation
  }
  private def findSubstitution(inference: Inference, definitionsByRelation: Map[BinaryRelation, RelationDefinitions]): Map[BinaryRelation, RelationDefinitions] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    (for {
      relation <- inference.premises.headOption.flatMap(p => definitionsByRelation.keys.find(r => r.unapply(p).nonEmpty))
      if (inference match {
        case Inference(
          _,
          Seq(
            relation(TermVariable(a), TermVariable(b)),
            PredicateApplication(phi, Seq(TermVariable(c)))),
          PredicateApplication(psi, Seq(TermVariable(d)))
        ) if a == c && b == d && phi == psi =>
          true
        case _ =>
          false
      })
    } yield definitionsByRelation.replace(relation, _.copy(substitution = Some(Substitution(relation, inference.summary))))) getOrElse definitionsByRelation
  }

  lazy val transitivityDefinitions: Seq[(String, Transitivity)] = {
    definedBinaryRelations.mapCollect { case (symbol, relation) =>
        definitionsByRelation.get(relation).flatMap(_.transitivity).map(symbol -> _)
    }
  }

  lazy val rearrangeableFunctions: Seq[(BinaryOperator, Commutativity, Associativity)] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      equality <- equalityOption.toSeq
      appropriateInferences = inferenceEntries.filter { inference =>
        val substitutions = inference.requiredSubstitutions
        substitutions.statements.isEmpty &&
          substitutions.predicates.isEmpty &&
          substitutions.functions.isEmpty &&
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
        singlePremise.requiredSubstitutions.predicates.isEmpty && singlePremise.requiredSubstitutions.functions.isEmpty &&
        conclusion.referencedDefinitions.subsetOf(singlePremise.referencedDefinitions)
    =>
      (inference, singlePremise)
  }
  lazy val conclusionSimplificationInferences: Seq[Inference] = inferenceEntries.filter {
    case inference @ Inference(_, premises, conclusion)
      if premises.nonEmpty &&
        premises.forall(_.complexity < conclusion.complexity) &&
        conclusion.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
        conclusion.requiredSubstitutions.predicates.isEmpty && conclusion.requiredSubstitutions.functions.isEmpty &&
        premises.forall(_.referencedDefinitions.subsetOf(conclusion.referencedDefinitions))
    =>
      true
    case _ =>
      false
  }

  implicit class ExpressionOps(expression: Expression) {
    def singleStatementVariable: Option[StatementVariable] = expression match {
      case sv: StatementVariable =>
        Some(sv)
      case DefinedStatement(Seq(singleStatement: Statement), _) =>
        singleStatement.singleStatementVariable
      case _ =>
        None
    }
    def singlePredicateApplication: Option[PredicateApplication] = expression match {
      case pa: PredicateApplication =>
        Some(pa)
      case DefinedStatement(Seq(singleStatement: Statement), _) =>
        singleStatement.singlePredicateApplication
      case _ =>
        None
    }
  }

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Seq[Statement])] = inferenceEntries.collect {
    case inference @ Inference(_, firstPremise +: otherPremises, conclusion)
      if conclusion.singleStatementVariable.isDefined &&
        inference.requiredSubstitutions.copy(statements = Nil).isEmpty &&
        firstPremise.requiredSubstitutions.contains(inference.requiredSubstitutions) &&
        (conclusion.complexity < firstPremise.complexity || firstPremise.requiredSubstitutions.statements.length > 1)

    =>
      (inference, firstPremise, otherPremises)
  }
  val predicateSpecificationInferences: Seq[(Inference, Statement, String, Seq[String])] = inferenceEntries.collect {
    case inference @ Inference(_, Seq(singlePremise), conclusion)
      if conclusion.complexity < singlePremise.complexity
    =>
      conclusion.singlePredicateApplication
        .flatMap(pa => pa.arguments.map(_.asOptionalInstanceOf[TermVariable].map(_.name)).traverseOption.map(pa.variableName -> _))
        .filter { case (predicateName, argumentNames) =>
          singlePremise.requiredSubstitutions.isEquivalentTo(Substitutions.Required(Nil, Nil, Seq((predicateName, argumentNames.length)), Nil)) &&
            inference.requiredSubstitutions.isEquivalentTo(Substitutions.Required(Nil, argumentNames, Seq((predicateName, argumentNames.length)), Nil))
        }
        .map { case (predicateName, argumentNames) => (inference, singlePremise, predicateName, argumentNames)}
  }.collectDefined
  lazy val rewriteInferences: Seq[(Inference, Statement)] = {
    inferenceEntries.collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity == singlePremise.complexity &&
          conclusion.requiredSubstitutions.isEquivalentTo(singlePremise.requiredSubstitutions) &&
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

}

object Definitions {
  case class RelationDefinitions(reversal: Option[Reversal], transitivity: Option[Transitivity], expansion: Option[Expansion], substitution: Option[Substitution])

  def getDefinedBinaryRelations(statementDefinitions: Seq[StatementDefinition], shorthands: Seq[DisplayShorthand], termDefinitions: Seq[TermDefinition]): Seq[(String, BinaryRelation)] = {
    def fromDefinitions = for {
      definition <- statementDefinitions
      if definition.componentTypes.length == 2 && definition.format.baseFormatString == s"%0 ${definition.symbol} %1"
      relation = BinaryRelation(definition.defaultValue)
    } yield (definition.symbol, relation)
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
        shorthand.template.expand(
          Map.empty,
          Map(
            lhsVariable.name -> TermVariable(lhsVariable.name),
            rhsVariable.name -> TermVariable(rhsVariable.name),
            symbolVariable.name -> definition.defaultValue)
        ).asInstanceOf[Statement])
    } yield (definition.symbol, relation)
    fromDefinitions ++ fromShorthands
  }
}
