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
  def allBoundVariables: Seq[TermVariable]
  def containsTerms: Boolean
}

case class StatementVariable(i: Int) extends Statement {
  override def variables: Variables = Variables(Seq(this), Nil)
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

  override def allBoundVariables = Nil

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

  override def allBoundVariables = Nil

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

case class DefinedStatement(
    subcomponents: Seq[Component],
    boundVariables: Seq[TermVariable],
    definition: StatementDefinition)
 extends Statement
{
  override def variables: Variables = subcomponents.map(_.variables).foldLeft(Variables.empty)(_ ++ _)
  override def calculateSubstitutions(other: Component): Option[Substitutions] = other match {
    case DefinedStatement(otherSubcomponents, _, `definition`) =>
      val substitutionAttempts = subcomponents.zip(otherSubcomponents).map { case (component, otherComponent) =>
        component.calculateSubstitutions(otherComponent)
      }
      Substitutions.mergeAttempts(substitutionAttempts)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    copy(
      subcomponents = subcomponents.map(_.applySubstitutions(substitutions)),
      boundVariables = boundVariables.map(_.applySubstitutions(substitutions)).map(Term.asVariable))
  }
  override def substituteFreeVariable(termToReplaceWith: Term, termToBeReplaced: TermVariable): Statement = {
    boundVariables.intersect(termToReplaceWith.variables.termVariables) match {
      case v :+ _ =>
        throw new Exception(s"Cannot substitute $termToReplaceWith into $this - bound variable $v would be captured")
      case Nil =>
    }
    copy(subcomponents = subcomponents.map(_.substituteFreeVariable(termToReplaceWith, termToBeReplaced)))
  }
  override def attemptSimplification(other: Component): Option[DistinctVariables] = other match {
    case DefinedStatement(otherSubcomponents, _, `definition`) =>
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
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def containsTerms: Boolean = subcomponents.exists {
    case s: Statement =>
      s.containsTerms
    case t: Term =>
      true
  }

  override def allBoundVariables = boundVariables ++ subcomponents.ofType[Statement].flatMap(_.allBoundVariables)
}

object Statement extends ComponentType {

  def parser(context: Context): Parser[Statement] = {
    object ParsableStatement {
      def unapply(s: String): Option[StatementDefinition] = {
        context.statementDefinitions.find(_.symbol == s)
      }
    }

    def parserForStatementType(statementType: String): Parser[Statement] = statementType match {
      case ParsableStatement(statementDefinition) =>
        statementDefinition.statementParser(context)
      case IntParser(i) =>
        Parser.constant(StatementVariable(i))
      case "sub" =>
        for {
          termToReplaceWith <- Term.parser(context)
          termToBeReplaced <- Term.variableParser(context)
          statement <- parser(context)
        } yield {
          statement.substituteFreeVariable(termToReplaceWith, termToBeReplaced).asInstanceOf[Statement]
        }
      case _ =>
        throw new Exception(s"Unrecognised statement type $statementType")
    }

    Parser.singleWord.flatMap(parserForStatementType)
  }

  def listParser(context: Context): Parser[Seq[Statement]] = parser(context).listInParens(Some(","))

  def variableParser(context: Context): Parser[StatementVariable] = parser(context).map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }
}
