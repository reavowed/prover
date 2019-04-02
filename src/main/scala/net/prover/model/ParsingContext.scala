package net.prover.model

import net.prover.model.entries.{ChapterEntry, ExpressionDefinition, StatementDefinition, TermDefinition}
import net.prover.model.expressions._

import scala.util.Try

case class ParsingContext(
    inferences: Seq[Inference],
    statementDefinitions: Seq[StatementDefinition],
    termDefinitions: Seq[TermDefinition],
    termVariableNames: Set[String],
    parameterLists: Seq[Seq[(String, Int)]])
{
  def parameterDepth: Int = parameterLists.length
  def deductionStatementOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Deduction))
  }
  def scopingStatementOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Scoping))
  }

  def transitivityInferences: Map[ExpressionDefinition, Inference] = {
    inferences.mapCollect {
      case inference @ Inference(
        _,
        Seq(
          DefinedExpression(d1, Nil, Seq(ExpressionVariable(a1), ExpressionVariable(b1))),
          DefinedExpression(d2, Nil, Seq(ExpressionVariable(b2), ExpressionVariable(c1)))),
        DefinedExpression(d3, Nil, Seq(ExpressionVariable(a2), ExpressionVariable(c2)))
      ) if d1 == d2 && d2 == d3 && a1 == a2 && b1 == b2 && c1 == c2 =>
        Some((d1, inference))
      case _ =>
        None
    }.toMap
  }

  def add(chapterEntry: ChapterEntry): ParsingContext = {
    val contextWithDefinitions = chapterEntry match {
      case statementDefinition: StatementDefinition =>
        copy(statementDefinitions = statementDefinitions :+ statementDefinition)
      case termDefinition: TermDefinition =>
        copy(termDefinitions = termDefinitions :+ termDefinition)
      case _ =>
        this
    }
    contextWithDefinitions.copy(inferences = inferences ++ chapterEntry.inferences)
  }

  def addStatementDefinition(statementDefinition: StatementDefinition): ParsingContext = {
    copy(statementDefinitions = statementDefinitions :+ statementDefinition)
  }

  def addTermDefinition(termDefinition: TermDefinition) = {
    copy(termDefinitions = termDefinitions :+ termDefinition)
  }

  def addParameters(parameters: String *) = {
    copy(parameterLists = parameterLists :+ parameters.zipWithIndex)
  }

  def addParameterList(parameters: Seq[(String, Int)]) = {
    copy(parameterLists = parameterLists :+ parameters)
  }

  def withPlaceholderParameters(numberOfParameters: Int): ParsingContext = {
    val parameters = if(numberOfParameters == 1) Seq("$") else (1 to numberOfParameters).map(i => s"$$_$i")
    copy(parameterLists = parameters.zipWithIndex +: parameterLists)
  }

  object RecognisedStatementVariable {
    def unapply(string: String): Option[String] = {
      "([α-ω])".r.unapplySeq(string).flatMap(_.headOption)
    }
  }
  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      statementDefinitions.find(_.symbol == string)
    }
  }

  object RecognisedTermVariable {
    def unapply(string: String): Option[String] = {
      "([a-zA-Z](?:'|[_\\^].*)?)".r.unapplySeq(string).flatMap(_.headOption) orElse
        termVariableNames.find(_ == string)
    }
  }
  object RecognisedTermDefinition {
    def unapply(s: String): Option[TermDefinition] = {
      termDefinitions.find(_.symbol == s)
    }
  }

  object RecognisedParameter {
    val literalPattern = "(\\$+)(.*)".r
    def unapply(string: String): Option[FunctionParameter] = {
      parameterLists.reverse.zipWithIndex.mapFind {
        case (parameterList, level) =>
          parameterList.find(_._1 == string).map(_._2).map(index => FunctionParameter(index, level))
      } orElse (string match {
        case literalPattern(dollars, indexString) =>
          val level = dollars.length - 1
          for {
            index <- Try(indexString.toInt).toOption
          } yield FunctionParameter(index, level)
        case _ =>
          None
      })
    }
  }

  def matchScopingStatement(statement: Statement): Option[(Statement, StatementDefinition)] = {
    scopingStatementOption.flatMap { scopingStatement =>
      statement match {
        case DefinedStatement(Seq(substatement), `scopingStatement`) =>
          substatement.asOptionalInstanceOf[Statement].map(_ -> scopingStatement)
        case _ =>
          None
      }
    }
  }

  def matchDeductionStatement(statement: Statement): Option[(Statement, Statement, StatementDefinition)] = {
    deductionStatementOption.flatMap { deductionStatement =>
      statement match {
        case DefinedStatement(Seq(antecedentExpression, consequentExpression), `deductionStatement`) =>
          for {
            antecedent <- antecedentExpression.asOptionalInstanceOf[Statement]
            consequent <- consequentExpression.asOptionalInstanceOf[Statement]
          } yield (antecedent, consequent, deductionStatement)
        case _ =>
          None
      }
    }
  }
}

object ParsingContext {
  val empty = ParsingContext(
    inferences = Nil,
    statementDefinitions = Nil,
    termDefinitions = Nil,
    termVariableNames = Set.empty,
    parameterLists = Seq.empty)
}
