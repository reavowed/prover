package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.expressions._
import net.prover.model.{Format, Inference, Parser, ParsingContext}

@JsonIgnoreProperties(Array("symbol", "defaultVariables", "format"))
case class StatementDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    defaultVariables: Seq[Variable],
    name: String,
    format: Format,
    definingStatement: Option[Statement],
    chapterKey: String,
    bookKey: String,
    isTransformation: Boolean = false)
  extends ChapterEntry(StatementDefinition)
{
  val defaultValue = {
    DefinedStatement(
      defaultVariables,
      this)(
      boundVariableNames)
  }

  def statementParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      updatedContext = context.addBoundVariables(newBoundVariableNames)
      components <- defaultVariables.expressionsParser(updatedContext)
    } yield DefinedStatement(components, this)(newBoundVariableNames)
  }

  override def inferences: Seq[Inference] = {
    definingStatement.toSeq.flatMap { s =>
      Seq(
        Inference.Definition(name, chapterKey, bookKey, Seq(s), defaultValue),
        Inference.Definition(name, chapterKey, bookKey, Seq(defaultValue), s))
    }
  }
}

object StatementDefinition extends ChapterEntryParser[StatementDefinition] {
  override val name: String = "statement"

  def nameParser(implicit context: ParsingContext): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: ParsingContext): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  private def boundAndDefaultVariablesParser(implicit context: ParsingContext): Parser[(Seq[String], Seq[Variable])] = {
    val boundVariablePattern = "\\$(.*)".r
    Parser
      .selectWord("bound variable or variable") {
        case boundVariablePattern(boundVariableName) =>
          Left(boundVariableName)
        case context.RecognisedVariable(variable) =>
          Right(variable)
      }
      .listInParens(None)
      .map { list =>
        val boundVariables = list.collect {
          case Left(x) => x
        }
        val defaultVariables = list.collect {
          case Right(x) => x
        }
        (boundVariables, defaultVariables)
      }
  }

  def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundAndDefaultVariables <- boundAndDefaultVariablesParser
      boundVariables = boundAndDefaultVariables._1
      defaultVariables = boundAndDefaultVariables._2
      name <- nameParser.getOrElse(symbol)
      format <- Format.optionalParser(symbol, boundVariables ++ defaultVariables.map(_.text))
      optionalDefiningStatement <- definingStatementParser
      isTransformation <- Parser.optionalWord("transformation").isDefined
    } yield {
      StatementDefinition(
        symbol,
        boundVariables,
        defaultVariables,
        name,
        format,
        optionalDefiningStatement,
        chapterKey,
        bookKey,
        isTransformation)
    }
  }
}
