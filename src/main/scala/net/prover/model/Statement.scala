package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil

trait Statement extends JsonSerializable.Base with Component {
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def containsTerms: Boolean
}

case class StatementVariable(i: Int) extends Statement {
  override def variables: Variables = Variables(Seq(this), Nil)
  override def freeVariables: Seq[TermVariable] = Nil
  override def calculateSubstitutions(other: Component): Option[Substitutions] = other match {
    case otherStatement: Statement =>
      Some(Substitutions(Map(this -> otherStatement), Map.empty))
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    substitutions.statements.getOrElse(this, throw new Exception(s"No replacement for statement variable $this"))
  }
  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    if (termToReplaceWith == termToBeReplaced)
      this
    else
      StatementVariableWithReplacement(this, termToReplaceWith, termToBeReplaced)
  }

  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case x: StatementVariableWithReplacement =>
      Some(DistinctVariables(x.variablesBeingReplaced.map(_ -> Variables(Seq(this), Nil)).toMap))
    case x if x == this =>
      Some(DistinctVariables.empty)
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = this

  override def containsTerms = false

  override def html: String = (944 + i).toChar.toString
}

case class StatementVariableWithReplacement(
    statementVariable: StatementVariable,
    replacements: Seq[(Term, TermVariable)])
  extends Statement {
  def replacingTerms = replacements.map(_._1)
  def variablesBeingReplaced = replacements.map(_._2)
  override def variables: Variables = replacements.foldLeft(Variables(Seq(statementVariable), Nil)) { case (variables, (term, termVariable)) =>
    variables ++ term.variables :+ termVariable
  }
  override def freeVariables: Seq[TermVariable] = replacements.foldRight(Seq.empty[TermVariable]) { case ((term, termVariable), freeVariables) =>
    freeVariables.filter(_ != termVariable).union(term.freeVariables)
  }
  override def calculateSubstitutions(other: Component): Option[Substitutions] = {
    other match {
      case otherStatementVariableWithReplacement
        @ StatementVariableWithReplacement(otherStatementVariable, otherReplacements)
        if otherReplacements.length == replacements.length
      =>
        val base = Substitutions(
          Map(statementVariable -> otherStatementVariable),
          variablesBeingReplaced.zip(otherStatementVariableWithReplacement.variablesBeingReplaced).toMap)
        val substitutionAttempts = Some(base) +: replacingTerms.zip(otherStatementVariableWithReplacement.replacingTerms).map { case (x, y) =>
            x.calculateSubstitutions(y)
        }
        Substitutions.mergeAttempts(substitutionAttempts)
      case otherStatement: Statement if !otherStatement.containsTerms =>
        Some(Substitutions(Map(statementVariable -> otherStatement), Map.empty))
      case _ =>
        Some(Substitutions(Map.empty, Map.empty))
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Component = {
    replacements.foldLeft(statementVariable.applySubstitutions(substitutions)) { case (statement, (a, b)) =>
      statement.substituteFreeVariable(
        a.applySubstitutions(substitutions).asInstanceOf[Term],
        Term.asVariable(b.applySubstitutions(substitutions))
      ).asInstanceOf[Statement]
    }
  }

  def substituteFreeVariable(
    newTermToReplaceWith: Term,
    newTermToBeReplaced: TermVariable
  ): Statement = {
    if (newTermToReplaceWith == newTermToBeReplaced || variablesBeingReplaced.contains(newTermToBeReplaced)) {
      this
    } else {
      copy(replacements = (newTermToReplaceWith -> newTermToBeReplaced) +: replacements)
    }
  }

  override def attemptSimplification(other: Component): Option[DistinctVariables] = {
    def calculateOmittedVariables(
      replacementsToSimplify: Seq[(Term, TermVariable)],
      replacementsToMatch: Seq[(Term, TermVariable)],
      acc: Seq[TermVariable]
    ): Option[Seq[TermVariable]] = {
      replacementsToSimplify match {
        case (a, b) +: tail =>
          replacementsToMatch match {
            case (`a`, `b`) +: otherTail =>
              calculateOmittedVariables(tail, otherTail, acc)
            case (`a`, c) +: otherTail if tail.nonEmpty && tail.head._1 == b && tail.head._2 == c =>
              calculateOmittedVariables(tail.tail, otherTail, acc :+ b)
            case _ =>
              calculateOmittedVariables(tail, replacementsToMatch, acc :+ b)
          }
        case Nil =>
          if (replacementsToMatch.isEmpty) Some(acc)
          else None
      }
    }

    other match {
      case x if x == this =>
        Some(DistinctVariables.empty)
      case `statementVariable` =>
        Some(DistinctVariables(variablesBeingReplaced.map(_ -> Variables(Seq(statementVariable), Nil)).toMap))
      case StatementVariableWithReplacement(`statementVariable`, otherReplacements) =>
        calculateOmittedVariables(replacements, otherReplacements, Nil).map { omittedVariables =>
          DistinctVariables(omittedVariables.map(_ -> Variables(Seq(statementVariable), Nil)).toMap)
        }
      case _ =>
        None
    }
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    def calculateSimplifiedReplacements(
      remainingReplacements: Seq[(Term, TermVariable)],
      acc: Seq[(Term, TermVariable)]
    ): Seq[(Term, TermVariable)] = {
      remainingReplacements match {
        case (a, b) +: tail =>
          if (distinctVariables.areDistinct(b, statementVariable)) {
            tail match {
              case (`b`, c) +: tailTail =>
                calculateSimplifiedReplacements(tailTail, acc :+ (a, c))
              case _ =>
                calculateSimplifiedReplacements(tail, acc)
            }
          } else {
            calculateSimplifiedReplacements(tail, acc :+ (a, b))
          }
        case Nil =>
          acc
      }
    }

    val simplifiedReplacements = calculateSimplifiedReplacements(replacements, Nil)
    if (simplifiedReplacements.isEmpty)
      statementVariable
    else
      copy(replacements = simplifiedReplacements)
  }

  override def containsTerms = true

  override def html: String = {
    replacements.map { case (term, termVariable) =>
      s"[$term/$termVariable]"
    }.mkString("") + statementVariable.html
  }
}

object StatementVariableWithReplacement {
  def apply(
    statementVariable: StatementVariable,
    term: Term,
    termVariable: TermVariable
  ): StatementVariableWithReplacement = {
    StatementVariableWithReplacement(statementVariable, Seq(term -> termVariable))
  }
}

case class ConnectiveStatement(substatements: Seq[Statement], connective: Connective) extends Statement {
  override def variables: Variables = substatements.map(_.variables).reduce(_ ++ _)
  override def freeVariables: Seq[TermVariable] = substatements.map(_.freeVariables).reduce(_ ++ _)
  override def calculateSubstitutions(other: Component): Option[Substitutions] = {
    other match {
      case ConnectiveStatement(otherSubstatements, `connective`) =>
        val substitutionAttempts = substatements.zip(otherSubstatements).map { case (substatement, otherSubstatement) =>
          substatement.calculateSubstitutions(otherSubstatement)
        }
        Substitutions.mergeAttempts(substitutionAttempts)
      case _ =>
        None
    }
  }

  override def applySubstitutions(substitutions: Substitutions): Component = {
    copy(substatements = substatements.map(_.applySubstitutions(substitutions).asInstanceOf[Statement]))
  }

  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    copy(substatements = substatements.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced).asInstanceOf[Statement]))
  }

  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case ConnectiveStatement(otherSubstatements, `connective`) =>
      substatements.zip(otherSubstatements).map { case (substatement, otherSubstatement) =>
        substatement.attemptSimplification(otherSubstatement)
      }.traverseOption.map(_.reduce(_ ++ _))
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Component = {
    copy(substatements.map(_.makeSimplifications(distinctVariables).asInstanceOf[Statement]))
  }

  def html: String = substatements match {
    case Seq(substatement) =>
      connective.symbol + substatement.safeHtml
    case _ =>
      substatements.map(_.safeHtml).mkString(" " + connective.symbol + " ")
  }

  override def containsTerms = substatements.exists(_.containsTerms)

  override def safeHtml: String = if (substatements.length == 1) html else "(" + html + ")"
}

case class QuantifierStatement(boundVariable: TermVariable, substatement: Statement, quantifier: Quantifier) extends Statement {
  override def variables: Variables = boundVariable +: substatement.variables
  override def freeVariables: Seq[TermVariable] = substatement.freeVariables.filter(_ != boundVariable)

  override def calculateSubstitutions(other: Component): Option[Substitutions] = {
    other match {
      case QuantifierStatement(otherBoundVariable, otherSubstatement, `quantifier`) =>
        substatement.calculateSubstitutions(otherSubstatement).flatMap { substatementMatch =>
          Substitutions.merge(Seq(
            substatementMatch,
            Substitutions(Map.empty, Map(boundVariable -> otherBoundVariable))))
        }
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Component = {
    copy(
      boundVariable = Term.asVariable(boundVariable.applySubstitutions(substitutions)),
      substatement = substatement.applySubstitutions(substitutions).asInstanceOf[Statement])
  }

  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Statement = {
    if (termToBeReplaced == boundVariable)
      this
    else if (termToReplaceWith.freeVariables.contains(boundVariable))
      throw new Exception("Cannot replace free variable with bound variable in quantified statement")
    else
      copy(substatement = substatement.substituteFreeVariable(
        termToReplaceWith,
        termToBeReplaced
      ).asInstanceOf[Statement])
  }

  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case QuantifierStatement(`boundVariable`, otherSubstatement, `quantifier`) =>
      substatement.attemptSimplification(otherSubstatement)
    case _ =>
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Component = {
    copy(substatement = substatement.makeSimplifications(distinctVariables).asInstanceOf[Statement])
  }

  override def containsTerms = true

  override def html: String = s"(${quantifier.symbol}${boundVariable.html})${substatement.safeHtml}"
}

case class PredicateStatement(terms: Seq[Term], predicate: Predicate) extends Statement {
  override def variables: Variables = terms.map(_.variables).reduce(_ ++ _)
  override def freeVariables: Seq[TermVariable] = terms.map(_.freeVariables).reduce(_ ++ _)
  override def calculateSubstitutions(other: Component): Option[Substitutions] = {
    other match {
      case PredicateStatement(otherTerms, `predicate`) =>
        val substitutionAttempts = terms.zip(otherTerms).map { case (term, otherTerm) =>
          term.calculateSubstitutions(otherTerm)
        }
        Substitutions.mergeAttempts(substitutionAttempts)
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Component = {
    copy(terms = terms.map(_.applySubstitutions(substitutions).asInstanceOf[Term]))
  }
  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Component = {
    copy(terms = terms.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced).asInstanceOf[Term]))
  }
  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case PredicateStatement(otherTerms, `predicate`) =>
      terms.zip(otherTerms).map { case (term, otherTerm) =>
        term.attemptSimplification(otherTerm)
      }.traverseOption.map(_.reduce(_ ++ _))
    case _ =>
      None
  }
  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    copy(terms = terms.map(_.makeSimplifications(distinctVariables).asInstanceOf[Term]))
  }
  override def containsTerms = true
  def html: String = terms.map(_.safeHtml).mkString(" " + predicate.symbol + " ")
  override def safeHtml: String = if (terms.length > 1) "(" + html + ")" else html
}

case class DefinedStatement(
    subcomponents: Seq[Component],
    statementSpecification: StatementSpecification)
 extends Statement
{
  override def containsTerms: Boolean = subcomponents.exists {
    case s: Statement =>
      s.containsTerms
    case t: Term =>
      true
  }
  override def variables: Variables = subcomponents.map(_.variables).foldLeft(Variables.empty)(_ ++ _)
  override def freeVariables: Seq[TermVariable] = subcomponents.map(_.freeVariables).foldLeft(Seq.empty[TermVariable])(_ ++ _)
  override def calculateSubstitutions(other: Component): Option[Substitutions] = other match {
    case DefinedStatement(otherSubcomponents, `statementSpecification`) =>
      val substitutionAttempts = subcomponents.zip(otherSubcomponents).map { case (component, otherComponent) =>
        component.calculateSubstitutions(otherComponent)
      }
      Substitutions.mergeAttempts(substitutionAttempts)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    copy(subcomponents = subcomponents.map(_.applySubstitutions(substitutions)))
  }
  override def substituteFreeVariable(termToReplaceWith: Term, termToBeReplaced: TermVariable): Statement = {
    copy(subcomponents = subcomponents.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced)))
  }
  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case DefinedStatement(otherSubcomponents, `statementSpecification`) =>
      subcomponents.zip(otherSubcomponents).map { case (component, otherComponent) =>
        component.attemptSimplification(otherComponent)
      }.traverseOption.map(_.foldLeft(DistinctVariables.empty)(_ ++ _))
    case _ =>
      None
  }
  override def makeSimplifications(distinctVariables: DistinctVariables): Statement = {
    copy(subcomponents = subcomponents.map(_.makeSimplifications(distinctVariables)))
  }
  override def html: String = {
    subcomponents.foldLeft(statementSpecification.format) { case (str, component) =>
      str.replaceFirst("\\{\\}", component.safeHtml)
    }
  }
  override def safeHtml: String = if (statementSpecification.requiresBrackets) s"($html)" else html
}

object Statement extends ComponentType {
  def parseStatementVariable(line: PartialLine, context: Context): (StatementVariable, PartialLine) = {
    parse(line, context) match {
      case (v: StatementVariable, remainingLine) =>
        (v, remainingLine)
      case (x, _) =>
        throw ParseException.withMessage(s"Expected statement variable, got $x", line.fullLine)
    }
  }

  def parse(line: PartialLine, context: Context): (Statement, PartialLine) = {
    object ParsableStatement {
      def unapply(s: String): Option[StatementParser] = {
        context.statementParsers.find(_.symbol == s)
      }
    }
    val (statementType, remainingLine) = line.splitFirstWord
    statementType match {
      case ParsableStatement(statementDefinition) =>
        statementDefinition.parseStatement(remainingLine, context)
      case IntParser(i) =>
        (StatementVariable(i), remainingLine)
      case "sub" =>
        val (termToReplaceWith, lineAfterFirstTerm) = Term.parse(remainingLine, context)
        val (termToBeReplaced, lineAfterSecondTerm) = Term.parse(lineAfterFirstTerm, context).mapLeft(Term.asVariable)
        val (statement, lineAfterStatement) = parse(lineAfterSecondTerm, context)
        (statement.substituteFreeVariable(termToReplaceWith, termToBeReplaced).asInstanceOf[Statement], lineAfterStatement)
      case _ =>
        throw ParseException.withMessage(s"Unrecognised statement type $statementType", line.fullLine)
    }
  }

  def parseList(
    line: PartialLine,
    context: Context,
    statementsSoFar: Seq[Statement] = Nil
  ): (Seq[Statement], PartialLine) = {
    val (statement, lineAfterStatement) = parse(line, context)
    lineAfterStatement match {
      case WordAndRemainingText("&", remainingText) =>
        parseList(remainingText, context, statementsSoFar :+ statement)
      case _ =>
        (statementsSoFar :+ statement, lineAfterStatement)
    }
  }

  def parseOptional(line: PartialLine, context: Context): (Option[Statement], PartialLine) = {
    if (line.nonEmpty && line.remainingText.head != ')') {
      Statement.parse(line, context).mapLeft(Some(_))
    } else {
      (None, line)
    }
  }
}
